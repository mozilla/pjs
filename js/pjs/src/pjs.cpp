/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sw=4 et tw=99:
 *
 * ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is the Pjs project.
 *
 * The Initial Developer of the Original Code is
 * Mozilla Corporation.
 * Portions created by the Initial Developer are Copyright (C) 2010
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Nicholas Matsakis <nmatsakis@mozilla.com>
 *   Donovan Preston <dpreston@mozilla.com>
 *   Fadi Meawad <fmeawad@mozilla.com>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either of the GNU General Public License Version 2 or later (the "GPL"),
 * or the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */

#include <memory>
#include <deque>
#include <string.h>

#include <prthread.h>
#include <prlock.h>
#include <prcvar.h>
#include <jsapi.h>
#include <jslock.h>
#include <jsfriendapi.h>
#include "membrane.h"
#include "util.h"
#include "proxyrack.h"
#include "jstypedarray.h"
#include "jsobj.h"
#include "jsnum.h"
#include "pjs.h"

extern size_t gMaxStackSize;

using namespace js;
using namespace std;

#if 1
#  define DEBUG(...) 
#else
#  define DEBUG(...) fprintf(stderr, __VA_ARGS__)
#endif

namespace pjs {

/*************************************************************
 * Execution Model
 * ---------------
 *
 * The thread pool features a fixed number of threads called engine
 * runners (to distinguish them from web workers).  Each runner has
 * associated with it a runtime, context, and a set of executing
 * tasks. Once associated with a particular runner, a task never
 * moves: this is because the task has only a compartment, which is
 * bound to a particular runtime and context.
 *
 * For now, there is a central queue and each runner has an associated
 * queue as well.  The central queue stores tasks that are waiting to
 * be claimed; the runner queue stores tasks which have begun
 * execution and are waiting for their children to complete.  Once a
 * task begins execution with a particular runner, it is linked to the
 * JSRuntime of that runner and thus can never migrate. 
 *
 * The main loop of a runner first checks its local queue for work: if
 * none is found, then it blocks loading from the main queue.  The
 * runner may be reawoken if one of its associated tasks is ready to
 * execute; more on that later.
 * 
 * Tasks are represented using two halves: the TaskHandle and the
 * TaskContext.  Both have associated JS objects.  The TaskHandle is
 * the result of a fork operation and represents the task "from the
 * outside".  The TaskContext is created when the task is claimed by a
 * runner and represents the task "from the inside."  It has an
 * associated JS object but this is purely for GC purposes: the object
 * is never exposed to the running JavaScript code. The TaskContext
 * stores the task global along with other information.
 *
 * There are actually two kinds of TaskHandles: the RootTaskHandle,
 * which exists only for the root task, and ChildTaskHandles, which
 * are for all other tasks.  The root task is somewhat special in that
 * it has no JS counterpart and is generally simpler.
 *
 * To manage the parallel forking and joining, the TaskContext defines
 * a few central fields: a list of forked TaskHandles, a counter, and
 * a "oncompletion" callback (initially NULL when every turn begins).
 * A fork() call causes a new TaskHandle to be created and added to
 * the list of forked handles.  A call to "oncompletion()" will set
 * the oncompletion callback.  When the turn ends, the oncompletion
 * callback is checked: if it is non-NULL, then the counter is
 * incremented by 1 for each pending TaskHandle and the pending
 * TaskHandles are pushed onto the central queue.  The runner then
 * returns to the idle loop.  We will cover the case where the
 * oncallback handler is NULL below.
 *
 * At this point, whichever runners are idle will draw the child tasks
 * from the main queue.  They will then create an associated
 * TaskContext (which has a pointer to the TaskHandle, though this
 * pointer is not available to JS code) and begin executing the task
 * function (for now, this is passed in a string, but it will
 * eventually be a proxied closure).  At this point, we are back
 * at the beginning: these child task contexts may themselves create
 * new children, repeating the process described in the previous
 * paragraph.  But let us assume that they do not create new children,
 * or at least do not invoke oncallback().  In that case, at the end
 * of the child task's turn, the oncallback handler will be NULL.
 *
 * If the oncallback handler is NULL, the child task is assumed to be
 * completed (in the future we will consider other pending callbacks
 * as well, such as async I/O).  In that case, the counter on the
 * parent task context is atomically decremented. If the counter
 * reaches 0, then the parent task context is ready to be reawoken:
 * the parent task is thus pushed onto the runner's local queue and
 * the runner's lock is pulsed to reawaken if it is sleeping.
 *
 * Passing results around
 * ----------------------
 *
 * When a task completes its turn without invoking `oncompletion()`,
 * its return value is used as the result.  This return value is
 * (currently) encoded using structured clone.  When all children have
 * completed, the parent thread will declone the results from all
 * children before executing the `oncompletion()` handler.  
 *
 * Garbage collection
 * ------------------
 * 
 * The GC strategy is as follows: each active ChildTaskHandle and
 * TaskContext is "self-rooted", which means that the C++ object adds
 * the corresponding JS object to the root set.  When the task
 * completes, its task context is deleted and the GC root removed.
 * The ChildTaskHandle for the task remains live until the parent
 * resumes (meaning that the task and all its siblings must have
 * completed).  At that point, the parent will de-clone the result of
 * the task and delete the C++ object.  The JS object corresponding to
 * the task (which carries the result) may live on longer, if the JS
 * program keeps it alive, or may be collected immediately.
 *
 ************************************************************/

class ThreadPool;
class TaskHandle;
class TaskContext;
class ChildTaskHandle;
class Runner;

template<typename T> T* check_null(T* ptr) {
	if (ptr == NULL)
		throw "Fiddlesticks!";
	return ptr;
}

class CrossCompartment {
private:
	JSCrossCompartmentCall *cc;

public:
	CrossCompartment(JSContext *cx, JSObject *obj) {
		cc = JS_EnterCrossCompartmentCall(cx, obj);
	}

	~CrossCompartment() {
		JS_LeaveCrossCompartmentCall(cc);
	}
};

inline bool ValueIsFunction(JSContext *cx, const Value &v) {
	return v.isObject() && JS_ObjectIsFunction(cx, v.toObjectOrNull());
}

// ____________________________________________________________
// Misc

class ArrRooter {
private:
	JSContext *_cx;
	Value *_values;
	int _len;

	ArrRooter(JSContext *cx, Value *values, int len) :
			_cx(cx), _values(values), _len(len) {
	}

public:
	static ArrRooter *create(JSContext *cx, Value *values, int len);
	~ArrRooter();
};

class AutoContextPrivate {
private:
	JSContext *_cx;

public:
	AutoContextPrivate(JSContext *cx, void *v) :
			_cx(cx) {
		JS_SetContextPrivate(_cx, v);
	}

	~AutoContextPrivate() {
		JS_SetContextPrivate(_cx, NULL);
	}
};

// ____________________________________________________________
// Global interface

class Global {
public:
	static JSClass jsClass;
};

enum task_stat_cat {
	task_taken, task_stolen, task_enqueued, task_stat_max
};

// ______________________________________________________________________
// Global functions

/* The error reporter callback. */
void reportError(JSContext *cx, const char *message, JSErrorReport *report) {
	fprintf(stderr, "%s:%u:%s\n",
			report->filename ? report->filename : "<no filename>",
			(unsigned int) report->lineno, message);
}

JSBool print(JSContext *cx, unsigned argc, jsval *vp) {
	jsval *argv;
	unsigned i;
	JSString *str;
	char *bytes;

	argv = JS_ARGV(cx, vp);
	for (i = 0; i < argc; i++) {
		str = JS_ValueToString(cx, argv[i]);
		if (!str)
			return JS_FALSE;
		bytes = JS_EncodeString(cx, str);
		if (!bytes)
			return JS_FALSE;
		printf("%s%s", i ? " " : "", bytes);
		JS_free(cx, bytes);
	}
	printf("\n");
	JS_SET_RVAL(cx, vp, JSVAL_VOID);
	return JS_TRUE;
}

JSBool dumpObjects(JSContext *cx, unsigned argc, jsval *vp) {
	jsval *argv;
	unsigned i;
	JSString *str;
	char *bytes;

	argv = JS_ARGV(cx, vp);
	for (i = 0; i < argc; i++) {
		if (argv[i].isObject())
			argv[i].toObjectOrNull()->dump();
	}
	printf("\n");
	JS_SET_RVAL(cx, vp, JSVAL_VOID);
	return JS_TRUE;
}

JSBool assert(JSContext *cx, unsigned argc, jsval *vp) {
	JSBool chk;
	JSString *str;
	if (!JS_ConvertArguments(cx, argc, JS_ARGV(cx, vp), "bS", &chk, &str))
		return JS_FALSE;
	JS_SET_RVAL(cx, vp, JSVAL_VOID);

	if (!chk) {
		char *bytes = JS_EncodeString(cx, str);
		if (!bytes)
			return JS_FALSE;
		JS_ReportError(cx, "Assertion failure: %s\n", bytes);
		JS_free(cx, bytes);
		return JS_FALSE;
	}

	return JS_TRUE;
}

JSBool fork(JSContext *cx, unsigned argc, jsval *vp) {
	TaskContext *taskContext = (TaskContext*) JS_GetContextPrivate(cx);

	jsval *argv = JS_ARGV(cx, vp);
	auto_ptr<Closure> closure(Closure::create(cx, argv[0], argv + 1, argc - 1));
	if (!closure.get()) {
		return JS_FALSE;
	}

	ChildTaskHandle *th = ChildTaskHandle::create(cx, taskContext, closure);
	if (th == NULL) {
		return JS_FALSE;
	}
	JS_SET_RVAL(cx, vp, OBJECT_TO_JSVAL(th->object()));

	DEBUG("forked TaskHandle %p with parent context %p/g %p/c %p\n",
			th, taskContext, taskContext->global(),
			taskContext->global()->compartment());

	taskContext->addTaskToFork(th);
	return JS_TRUE;
}

JSBool forkN(JSContext *cx, unsigned argc, jsval *vp) {
	TaskContext *taskContext = (TaskContext*) JS_GetContextPrivate(cx);

	jsval *argv = JS_ARGV(cx, vp);

	uint32_t count = argv[0].payloadAsRawUint32();

	jsval typedArray = argv[2];
	if (typedArray.isObject()) {
		JSObject * arrObj = typedArray.toObjectOrNull();
		if (arrObj->isTypedArray()) {
			uint32_t length = TypedArray::length(arrObj);
			ArrayBufferObject * arrayBufferObject = TypedArray::buffer(arrObj);
			uint32_t type = TypedArray::type(arrObj);
			JSObject *(*JS_NewArrayWithBufferFn)(JSContext*, JSObject*,
					uint32_t, int32_t);
			switch (type) {
			case TypedArray::TYPE_FLOAT32:
				JS_NewArrayWithBufferFn = &JS_NewFloat32ArrayWithBuffer;
				break;
			case TypedArray::TYPE_FLOAT64:
				JS_NewArrayWithBufferFn = &JS_NewFloat64ArrayWithBuffer;
				break;
			case TypedArray::TYPE_INT16:
				JS_NewArrayWithBufferFn = &JS_NewInt16ArrayWithBuffer;
				break;

			case TypedArray::TYPE_INT32:
				JS_NewArrayWithBufferFn = &JS_NewInt32ArrayWithBuffer;
				break;
			case TypedArray::TYPE_INT8:
				JS_NewArrayWithBufferFn = &JS_NewInt8ArrayWithBuffer;
				break;
			case TypedArray::TYPE_UINT16:
				JS_NewArrayWithBufferFn = &JS_NewUint16ArrayWithBuffer;
				break;

			case TypedArray::TYPE_UINT32:
				JS_NewArrayWithBufferFn = &JS_NewUint32ArrayWithBuffer;
				break;
			case TypedArray::TYPE_UINT8:
				JS_NewArrayWithBufferFn = &JS_NewUint8ArrayWithBuffer;
				break;
			default:
				JS_ReportError(cx, "unknown type for TypedArray\n");
				break;
			}
			int step = length / count;
			int rem = length % count;
			int start = 0;
			int slotWidth = TypedArray::slotWidth(type);
			JSObject **views = check_null(new JSObject*[count]);
			memset(views, 0, sizeof(JSObject*) * count);
			int viewindex = 0;
			if (rem > 0) {
				views[viewindex++] = JS_NewArrayWithBufferFn(cx,
						arrayBufferObject, 0, rem + step);
				start = rem + step;
			}
			for (int i = start; i < length; i += step) {
				views[viewindex++] = JS_NewArrayWithBufferFn(cx,
						arrayBufferObject, i * slotWidth, step);

			}
			RootedObject resarr(cx, JS_NewArrayObject(cx, count, NULL));
			for (int i = 0; i < count; i++) {
				*(argv + 2) = OBJECT_TO_JSVAL(views[i]);
				auto_ptr<Closure> closure(
						Closure::create(cx, argv[1], argv + 2, argc - 2, i));
				if (!closure.get()) {
					return JS_FALSE;
				}
				ChildTaskHandle *th = ChildTaskHandle::create(cx, taskContext,
						closure);
				if (th == NULL) {
					return JS_FALSE;
				}

				resarr->setElement(cx, resarr, i,
						&OBJECT_TO_JSVAL(th->object()), false);
				DEBUG("forked TaskHandle %p with parent context %p/g %p/c %p\n",
						th, taskContext, taskContext->global(),
						taskContext->global()->compartment());

				taskContext->addTaskToFork(th);
			}
			JS_SET_RVAL(cx, vp, OBJECT_TO_JSVAL(resarr));
			//TODO: rebuild the arg list and create a closure and a task handle for each view.
		}
		return JS_TRUE;
	}
	return JS_FALSE;
}

JSBool oncompletion(JSContext *cx, unsigned argc, jsval *vp) {
	TaskContext *taskContext = (TaskContext*) JS_GetContextPrivate(cx);
	JSObject *func;
	if (!JS_ConvertArguments(cx, argc, JS_ARGV(cx, vp), "o", &func))
		return JS_FALSE;
	if (!JS_ObjectIsFunction(cx, func)) {
		JS_ReportError(cx, "expected function as argument");
		return JS_FALSE;
	}
	taskContext->setOncompletion(cx, OBJECT_TO_JSVAL(func));
	return JS_TRUE;
}

static JSFunctionSpec pjsGlobalFunctions[] = { JS_FN("print", print, 0, 0),
		JS_FN("dumpObjects", dumpObjects, 0, 0),
		JS_FN("assert", assert, 2, 0), JS_FN("fork", fork, 1, 0),
				JS_FN("forkN", forkN, 1, 0),
				JS_FN("oncompletion", oncompletion, 1, 0), JS_FS_END };

// ______________________________________________________________________
// Global impl

JSClass Global::jsClass = { "Global", JSCLASS_GLOBAL_FLAGS, JS_PropertyStub,
		JS_PropertyStub, JS_PropertyStub, JS_StrictPropertyStub,
		JS_EnumerateStub, JS_ResolveStub, JS_ConvertStub, NULL,
		JSCLASS_NO_OPTIONAL_MEMBERS };

/*
 JS_PUBLIC_API(JSBool)
 JS_ResolveStub(JSContext *cx, JSHandleObject obj, JSHandleId id)
 {
 }
 */

// ______________________________________________________________________
// ArrRooter impl
ArrRooter *
ArrRooter::create(JSContext *cx, Value *values, int len) {
	ArrRooter *result = NULL;
	int rooted = 0;

	for (; rooted < len; rooted++) {
		values[rooted] = JSVAL_NULL;
		if (!JS_AddNamedValueRoot(cx, &values[rooted], "RootedValueArr")) {
			goto fail;
		}
	}

	result = new ArrRooter(cx, values, len);
	if (!result) {
		JS_ReportOutOfMemory(cx);
		goto fail;
	}

	return result;

	fail: for (int i = 0; i < rooted; i++) {
		JS_RemoveValueRoot(cx, &values[i]);
	}
	return false;
}

ArrRooter::~ArrRooter() {
	for (int i = 0; i < _len; i++) {
		JS_RemoveValueRoot(_cx, &_values[i]);
	}
}

// ______________________________________________________________________
// ClonedObj impl

ClonedObj::~ClonedObj() {
	js::Foreground::free_(_data);
}

JSBool ClonedObj::pack(JSContext *cx, jsval val, ClonedObj **rval) {
	uint64_t *data;
	size_t nbytes;
	if (!JS_WriteStructuredClone(cx, val, &data, &nbytes, NULL, NULL)) {
		*rval = NULL;
		return false;
	}
	*rval = new ClonedObj(data, nbytes);
	return true;
}

JSBool ClonedObj::unpack(JSContext *cx, jsval *rval) {
	return JS_ReadStructuredClone(cx, _data, _nbytes,
			JS_STRUCTURED_CLONE_VERSION, rval, NULL, NULL);
}

// ______________________________________________________________________
// Closure impl

Closure *Closure::create(JSContext *cx, jsval fn, const jsval *argv, int argc,
		int taskIndex) {
	TaskContext *taskContext = (TaskContext*) JS_GetContextPrivate(cx);

	// Create an array containing
	// [0] - the function
	// [1..N] - the arguments to the function

	auto_arr<jsval> toProxy(new jsval[1 + argc]);
	if (!toProxy.get()) {
		JS_ReportOutOfMemory(cx);
		return NULL;
	}
	int p = 0;
	toProxy[p++] = fn;
	for (int i = 0; i < argc; i++) {
		toProxy[p++] = argv[i];
	}

	auto_ptr<Closure> c(new Closure(cx, toProxy, argc, taskIndex));
	if (!c.get()) {
		JS_ReportOutOfMemory(cx);
		return NULL;
	}
	if (!c->addRoots()) {
		return NULL;
	}
	return c.release();
}

bool Closure::addRoots() {
	for (; _rooted <= _argc; _rooted++) {
		if (!JS_AddNamedValueRoot(_cx, &_toProxy[_rooted],
				"Closure::toProxyArgv[]"))
			return false;
	}
	return true;
}

void Closure::delRoots() {
	for (unsigned i = 0; i < _rooted; i++)
		JS_RemoveValueRoot(_cx, &_toProxy[i]);
}

Closure::~Closure() {
	delRoots();
}
static PRLock *debugLock = PR_NewLock();

JSBool Closure::execute(Membrane *m, JSContext *cx, JSObject *global,
		jsval *rval) {
	// Wrap the function:
	int p = 0;
	AutoValueRooter fn(cx, _toProxy[p++]);
	if (!m->wrap(fn.addr())) {
		return false;
	}
	JS_ASSERT(ValueIsFunction(cx, fn.value()));

	auto_arr<jsval> argv(new jsval[_argc]); // ensure it gets freed
	if (!argv.get())
		return JS_FALSE;
	AutoArrayRooter argvRoot(cx, _argc, argv.get()); // ensure it is rooted
	for (int i = 0; i < _argc; i++) {
		argv[i] = _toProxy[p++];
		if (!m->wrap(&argv[i], this->_taskindex >= 0 ? true : false))
			return JS_FALSE;
	}

	return JS_CallFunctionValue(cx, global, fn.value(), _argc, argv.get(), rval);
}

// ______________________________________________________________________
// TaskHandle impl

void RootTaskHandle::onCompleted(Runner *runner, jsval result) {
	runner->terminate();
}

JSBool RootTaskHandle::execute(JSContext *cx, JSObject *global,
		auto_ptr<Membrane> &rmembrane, jsval *rval) {
	JSScript *scr = JS_CompileUTF8File(cx, global, scriptfn);
	if (scr == NULL)
		return 0;

	return JS_ExecuteScript(cx, global, scr, rval);
}

void ChildTaskHandle::onCompleted(Runner *runner, jsval result) {
	ClonedObj::pack(runner->cx(), result, &_result);
	_parent->onChildCompleted();
}

JSBool ChildTaskHandle::execute(JSContext *cx, JSObject *global,
		auto_ptr<Membrane> &rmembrane, jsval *rval) {

	static JSNative safeNatives[] = { ChildTaskHandle::jsGet, /*fork, forkN,
	 print, oncompletion, dumpObjects, eval, num_parseInt,*/NULL };

	auto_ptr<Membrane> m(
			Membrane::create(_parent->cx(), _parent->global(), cx, global,
					safeNatives, _parent->getProxyRack()));
	if (!m.get()) {
		return false;
	}

	// add (proxied) globals
	{
		AutoReadOnly ro(cx);
		JSObject *parentGlobal = _parent->global();
		AutoIdVector props(cx);
		if (!GetPropertyNames(cx, parentGlobal,
				JSITER_OWNONLY /*| JSITER_HIDDEN*/, &props))
			return false;
		for (jsid *v = props.begin(), *v_end = props.end(); v < v_end; v++) {
			jsid pid = *v, cid = *v;

//            if (!m->wrapId(&cid))
//                return false;

			// stop if this prop is already present on the child
			JSBool foundp;
			if (!JS_HasPropertyById(cx, global, cid, &foundp))
				return false;
			if (foundp)
				continue;

			jsval pval;
			if (!JS_GetPropertyById(cx, parentGlobal, pid, &pval))
				return false;
			if (!m->wrap(&pval))
				return false;
			AutoReadOnly rw(cx, false);
			if (!JS_SetPropertyById(cx, global, cid, &pval))
				return false;
		}
	}

	rmembrane = m;
	return _closure->execute(rmembrane.get(), cx, _parent->global(), rval);
}

ChildTaskHandle *ChildTaskHandle::create(JSContext *cx, TaskContext *parent,
		auto_ptr<Closure> &closure) {
	// To start, create the JS object representative:
	JSObject *object = JS_NewObject(cx, &jsClass, NULL, NULL);
	if (!object)
		return NULL;

	// Create C++ object:
	ChildTaskHandle *th = new ChildTaskHandle(cx, parent, object, closure);
	th->addRoot(cx);
	return th;
}

void ChildTaskHandle::clearResult() {
	if (_result) {
		delete _result;
		_result = 0;
	}
}

JSBool ChildTaskHandle::finalizeAndDelete(JSContext *cx) {
	if (!_result) {
		JS_ReportError(cx, "internal error---no result to unpack");
		return false;
	}

	jsval rval;
	if (!_result->unpack(cx, &rval))
		return false;

	JS_SetReservedSlot(_object, ResultSlot, rval);
	JS_SetReservedSlot(_object, ReadySlot, JSVAL_TRUE);
	delRoot(cx);
	delete this;
	return true;
}

JSBool ChildTaskHandle::addRoot(JSContext *cx) {
	PJS_ASSERT_CX(cx, _checkCx);
	return JS_AddNamedObjectRoot(cx, &_object, "ChildTaskHandle::addRoot()");
}

void ChildTaskHandle::delRoot(JSContext *cx) {
//	PJS_ASSERT_CX(cx, _checkCx);
	JS_RemoveObjectRoot(cx, &_object);
}

JSBool ChildTaskHandle::initClass(JSContext *cx, JSObject *global) {
	return !!JS_InitClass(cx, global, NULL, // obj, parent_proto
			&jsClass, // clasp
			NULL, 0, // constructor, nargs
			NULL, // JSPropertySpec *ps
			jsMethods, // JSFunctionSpec *fs
			NULL, // JSPropertySpec *static_ps
			NULL); // JSFunctionSpec *static_fs
}

JSClass ChildTaskHandle::jsClass = { "TaskHandle", JSCLASS_HAS_PRIVATE
		| JSCLASS_HAS_RESERVED_SLOTS(MaxSlot), JS_PropertyStub, JS_PropertyStub,
		JS_PropertyStub, JS_StrictPropertyStub, JS_EnumerateStub,
		JS_ResolveStub, JS_ConvertStub, NULL, JSCLASS_NO_OPTIONAL_MEMBERS };

JSFunctionSpec ChildTaskHandle::jsMethods[2] = { { "get", &jsGet, 0, 0 },
		JS_FS_END };

JSBool ChildTaskHandle::jsGet(JSContext *cx, unsigned argc, jsval *vp) {
	TaskContext *taskContext = static_cast<TaskContext*>(JS_GetContextPrivate(
			cx));
	JSObject *self = JS_THIS_OBJECT(cx, vp);

	// Is this a proxied task handle?
	if (Membrane::IsCrossThreadWrapper(self)) {
		JS_ReportError(cx, "all child tasks not yet completed");
		return JS_FALSE;
	}

	// Sanity check
// TODO: pjs disabled: sanity check no longer valid?
//	if (!JS_InstanceOf(cx, self, &jsClass, NULL)) {
//		JS_ReportError(cx, "expected TaskHandle as this");
//		return JS_FALSE;
//	}

	// Check if the generation has completed:
	jsval ready = JS_GetReservedSlot(self, ReadySlot);
	if (ready != JSVAL_TRUE) {
		JS_ReportError(cx, "all child tasks not yet completed");
		return JS_FALSE;
	}

	// If it has, there should be a value waiting for us:
	*vp = JS_GetReservedSlot(self, ResultSlot);
	return JS_TRUE;
}

// ______________________________________________________________________
// TaskContext impl

TaskContext *TaskContext::create(JSContext *cx, TaskHandle *aTask,
		Runner *aRunner, JSObject *aGlobal) {
	// To start, create the JS object representative:
	JSObject *object = JS_NewObject(cx, &jsClass, NULL, NULL);
	if (!object) {
		return NULL;
	}

	// Create C++ object:
	auto_ptr<TaskContext> tc(
			new TaskContext(cx, aTask, aRunner, aGlobal, object));
	if (!tc.get() || !tc->addRoot(cx)) {
		return NULL;
	}

	DEBUG("TaskContext %p for handle %p "
			"created with global %p and compartment %p\n",
			tc.get(), aTask, aGlobal,
			aGlobal->compartment());

	return tc.release();
}

JSBool TaskContext::addRoot(JSContext *cx) {
	if (!JS_AddNamedObjectRoot(cx, &_object, "TaskContext::_object"))
		return false;
	if (!JS_AddNamedObjectRoot(cx, &_global, "TaskContext::_global")) {
		JS_RemoveObjectRoot(cx, &_object);
		return false;
	}
	return true;
}

JSBool TaskContext::delRoot(JSContext *cx) {
	JS_RemoveObjectRoot(cx, &_object);
	JS_RemoveObjectRoot(cx, &_global);
}

JSContext *TaskContext::cx() {
	return _runner->cx();
}

void TaskContext::addTaskToFork(ChildTaskHandle *th) {
	_toFork.append(th);
}

void TaskContext::onChildCompleted() {
	PRInt32 v = JS_ATOMIC_DECREMENT(&_outstandingChildren);
	if (v == 0) {
		_runner->reawaken(this);
	}
}

JSBool TaskContext::unpackResults(JSContext *cx) {
	JSBool result = true;

	for (ChildTaskHandle **p = _toFork.begin(), **pn = _toFork.end(); p < pn;
			p++) {
		result = (*p)->finalizeAndDelete(cx) && result;
	}

	_toFork.clear();
	return result;
}

void TaskContext::resume(Runner *runner) {
	JSContext *cx = runner->cx();
	CrossCompartment cc(cx, _global);
	jsval rval;

	PJS_ASSERT_CX(cx, _checkCx);

	// If we break from this loop, this task context has completed,
	// either in error or successfully:
	while (true) {
		rval = JSVAL_NULL;

		AutoContextPrivate priv(cx, this);

		jsval fn = JS_GetReservedSlot(_object, OnCompletionSlot);
		if (JSVAL_IS_NULL(fn)) {
			// Initial generation: no callback fn set yet.
			if (!_taskHandle->execute(cx, _global, _membrane, &rval))
				break;
		} else {
			// Subsequent generation: callback fn was set last time.
			PJS_ClearSuspended(cx);
			if (!unpackResults(cx))
				break;
			setOncompletion(cx, JSVAL_NULL);
			if (!JS_CallFunctionValue(cx, _global, fn, 0, NULL, &rval))
				break;
		}

		// If the _oncompletion handler is set, fork off any tasks
		// and block until they complete:
		fn = JS_GetReservedSlot(_object, OnCompletionSlot);
		if (!JSVAL_IS_NULL(fn)) {
			if (!_toFork.empty()) {
				PJS_SetSuspended(cx);
				JS_ATOMIC_ADD(&_outstandingChildren, _toFork.length());
				runner->enqueueTasks(_toFork.begin(), _toFork.end());
				return;
			}
			continue; // degenerate case: no tasks, just loop around
		}

		// no _oncompletion handler is set, so we are done.
		break;
	}

	// we have finished, notify parent.
	_taskHandle->onCompleted(runner, rval);
	delRoot(cx);
	// release the membrane to prevent it from getting deleted with the TaskContext
	Membrane *m = _membrane.release();
	// release the membrane proxies, so that they would get Garbage Collected.
	// the membrane itself is deleted when all proxies are finalized.
	if (m)
		m->releaseProxies();
	delete this;
	return;
}

JSClass TaskContext::jsClass = { "TaskContext", JSCLASS_HAS_PRIVATE
		| JSCLASS_HAS_RESERVED_SLOTS(MaxSlot), JS_PropertyStub, JS_PropertyStub,
		JS_PropertyStub, JS_StrictPropertyStub, JS_EnumerateStub,
		JS_ResolveStub, JS_ConvertStub, NULL, JSCLASS_NO_OPTIONAL_MEMBERS };

// ______________________________________________________________________
// Runner impl

Runner *Runner::create(ThreadPool *aThreadPool, int anIndex) {
	JSRuntime *rt = check_null(JS_NewRuntime(10L * 1024L * 1024L));
	JSContext *cx = check_null(JS_NewContext(rt, 8192));
	JS_SetOptions(cx, JSOPTION_VAROBJFIX | JSOPTION_METHODJIT);
	JS_SetVersion(cx, JSVERSION_LATEST);
	JS_SetErrorReporter(cx, reportError);
	JSObject *global = JS_NewGlobalObject(
	/*JSContext *cx: */cx,
	/*JSClass *clasp: */&Global::jsClass, /*JSPrincipals*/NULL);
//	global->dump();
//	fprintf(stderr,"\nbefore\n");
	if (!JS_InitStandardClasses(cx, global))
		return NULL;
//	global->dump();
	if (!ChildTaskHandle::initClass(cx, global))
		return NULL;

	if (!JS_DefineFunctions(cx, global, pjsGlobalFunctions))
		return NULL; // XXX

	char *pjs_zeal = getenv("PJS_ZEAL");
	if (pjs_zeal) {
		int v1, v2;
		if (sscanf(pjs_zeal, "%d,%d", &v1, &v2) < 2) {
			fprintf(stderr, "PJS_ZEAL should have the form N,N, not %s\n",
					pjs_zeal);
			exit(1);
		}
		JS_SetGCZeal(cx, v1, v2);
	}

	CrossCompartment cc(cx, global);

	JS_ClearRuntimeThread(rt);

	return new Runner(aThreadPool, anIndex, rt, cx, global);
}

bool Runner::haveWorkStolen(TaskHandle **create) {
	AutoLock hold(_runnerLock);

	if (_toCreate.empty())
		return false;

	*create = _toCreate.back();
	_toCreate.pop_back();
	return true;
}

bool Runner::stealWork(TaskHandle **create) {

	int n = _threadPool->runnerCount();

	for (int i = _index + 1; i < n; i++)
		if (_threadPool->runner(i)->haveWorkStolen(create)) {
			return true;
		}

	for (int i = 0; i < _index; i++)
		if (_threadPool->runner(i)->haveWorkStolen(create)) {
			return true;
		}

	return false;
}

bool Runner::getLocalWork(TaskContext **reawaken, TaskHandle **create) {
	AutoLock hold(_runnerLock);

	if (!_toReawaken.empty()) {
		*reawaken = _toReawaken.popCopy();
		return true;
	}

	if (!_toCreate.empty()) {
		*create = _toCreate.front();
		_toCreate.pop_front();
		return true;
	}

	return false;
}

bool Runner::getWork(TaskContext **reawaken, TaskHandle **create) {
	*reawaken = NULL;
	*create = NULL;

	while (!_threadPool->terminating()) {
		workCounter_t wc = _threadPool->readWorkCounter();

		if (getLocalWork(reawaken, create)) {
			_stats[PJS_TASK_TAKEN] += 1;
			return true;
		}

		if (stealWork(create)) {
			_stats[PJS_TASK_STOLEN] += 1;
			return true;
		}

		_threadPool->awaitNewWork(wc);
	}

	return false;
}

void Runner::reawaken(TaskContext *ctx) {
	{
		AutoLock hold(_runnerLock);
		_toReawaken.append(ctx);
	}

	// FIXME--This is kind of lame.  only the current runner has new
	// work here, but there is no easy way to atomically check whether
	// the current runner is idle and only signal in that case.
	_threadPool->signalNewWork();
}

void Runner::enqueueRootTask(RootTaskHandle *p) {
	AutoLock hold(_runnerLock);
	_toCreate.push_front(p);
	_stats[PJS_TASK_ENQUEUED] += 1;
	// no need to signal new work for the root task.
}

void Runner::enqueueTasks(ChildTaskHandle **begin, ChildTaskHandle **end) {
	{
		AutoLock hold(_runnerLock);
		for (ChildTaskHandle **p = begin; p < end; p++) {
			_toCreate.push_front(*p);
			_stats[PJS_TASK_ENQUEUED] += 1;
		}
	}

	_threadPool->signalNewWork();
}

void Runner::start() {
	_threadPool->awaitStartBarrier();

	TaskContext *reawaken = NULL;
	TaskHandle *create = NULL;
	JS_SetRuntimeThread(_rt);
	while (getWork(&reawaken, &create)) {
		JS_BeginRequest(_cx);
		if (reawaken) {
			reawaken->resume(this);
		}

		if (create) {
			TaskContext *ctx =
					create->isRootTaskHandle() ?
							createRootTaskContext(create) :
							createTaskContext(create);
			if (ctx) {
				ctx->resume(this);
			}
		}
		JS_EndRequest(_cx);
	}
}

TaskContext *Runner::createTaskContext(TaskHandle *handle) {
	return TaskContext::create(_cx, handle, this, _global);
}

TaskContext *Runner::createRootTaskContext(TaskHandle *handle) {
	JSObject *global = JS_NewGlobalObject(
	/*JSContext *cx: */_cx,
	/*JSClass *clasp: */&Global::jsClass, /*JSPrincipals*/NULL);
	CrossCompartment cc(_cx, global);
	if (!JS_InitStandardClasses(_cx, global))
		return NULL;

	if (!JS_DefineFunctions(_cx, global, pjsGlobalFunctions))
		return NULL; // XXX

	if (!ChildTaskHandle::initClass(_cx, global))
		return NULL;

	this->_threadPool->setGlobal(global);

	return TaskContext::create(_cx, handle, this, global);
}

void Runner::terminate() {
	_threadPool->terminate();
}

// ______________________________________________________________________
// ThreadPool impl

ThreadPool *ThreadPool::create(int threadCount) {
	PRLock *lock = check_null(PR_NewLock());
	PRCondVar *condVar = check_null(PR_NewCondVar(lock));

	PRThread **threads = check_null(new PRThread*[threadCount]);
	memset(threads, 0, sizeof(PRThread*) * threadCount);

	Runner **runners = check_null(new Runner*[threadCount]);
	memset(threads, 0, sizeof(Runner*) * threadCount);

	ThreadPool *tp = check_null(
			new ThreadPool(lock, condVar, threadCount, threads, runners));

	for (int i = 0; i < threadCount; i++) {
		runners[i] = check_null(Runner::create(tp, i));
		threads[i] = PR_CreateThread(PR_USER_THREAD, start, runners[i],
				PR_PRIORITY_NORMAL, PR_LOCAL_THREAD, PR_JOINABLE_THREAD, 0);
		check_null(threads[i]);
	}

	return tp;
}

void ThreadPool::start(void* arg) {
	((Runner*) arg)->start();
}

void ThreadPool::signalNewWork() {
	AutoLock hold(_tpLock);

	// Increment work counter.  The low bit indicates whether there
	// are idle (unsignaled) workers hanging around.  If there are,
	// then notify.
	_workCounter++;
	if (_idlingWorkers) {
		PR_NotifyAllCondVar(_tpCondVar);
		_idlingWorkers = false;
	}

	// I don't worry about overflow.  There is one bad case: if some
	// worker reads the new work counter when it has value X, searches
	// unsuccessfully for work, then idles while a whole lot of work
	// comes in.  If there is just the right amount of work, the
	// counter might roll-over and reach X again, thus deceiving the
	// worker into thinking nothing had happened.  The worker would
	// then go to sleep and (unless more work was produced, which it
	// probably would be) never awake.  This seems rather unlikely so
	// I am not overly concerned.  Even if it did happen, it would
	// mean a loss of parallelism but not that PJs itself would
	// completely stall. I am not sure if there is any way to prevent
	// it that doesn't create bigger problems than the problem it's
	// trying to solve.
}

workCounter_t ThreadPool::readWorkCounter() {
	return _workCounter;
}

void ThreadPool::awaitNewWork(workCounter_t since) {
	AutoLock hold(_tpLock);

	while (true) {
		// If we are terminating or new work has been produced, bail.
		if (_terminating)
			return;
		if (_workCounter != since)
			return;

		// Ensure that idle flag is set, then block.
		_idlingWorkers = true;
		PR_WaitCondVar(_tpCondVar, PR_INTERVAL_NO_TIMEOUT);
	}
}

void ThreadPool::signalStartBarrier() {
	if (!_started) {
		AutoLock hold(_tpLock);
		_started = 1;
		PR_NotifyAllCondVar(_tpCondVar);
	}
}

void ThreadPool::awaitStartBarrier() {
	AutoLock hold(_tpLock);
	while (!_started) {
		PR_WaitCondVar(_tpCondVar, PR_INTERVAL_NO_TIMEOUT);
	}
}

void ThreadPool::start(RootTaskHandle *rth) {
	runner(0)->enqueueRootTask(rth);
	signalStartBarrier();
}

void ThreadPool::terminate() {
	signalStartBarrier(); // in case it hasn't happened yet

	if (!_terminating) {
		AutoLock hold(_tpLock);
		_terminating = 1;
		PR_NotifyAllCondVar(_tpCondVar);
	}
}

void ThreadPool::await() {
	for (int i = 0; i < _threadCount; i++) {
		if (_threads[i]) {
			PR_JoinThread(_threads[i]);
			_threads[i] = NULL;
		}
	}

	if (getenv("PJS_STATS") != NULL) {
		int totals[PJS_TASK_CAT_MAX] = { 0, 0 };
		fprintf(stderr, "Runner | Enqueued | Taken | Stolen\n");
		for (int i = 0; i < _threadCount; i++) {
			fprintf(stderr, "%6d | %8d | %5d | %6d\n", i,
					_runners[i]->stats(PJS_TASK_ENQUEUED),
					_runners[i]->stats(PJS_TASK_TAKEN),
					_runners[i]->stats(PJS_TASK_STOLEN));
			for (int j = 0; j < PJS_TASK_CAT_MAX; j++)
				totals[j] += _runners[i]->stats(j);
		}
		fprintf(stderr, "%6s | %8d | %5d | %6d\n", "total",
				totals[PJS_TASK_ENQUEUED], totals[PJS_TASK_TAKEN],
				totals[PJS_TASK_STOLEN]);
		fprintf(stderr, "%6s | %8.0f | %5.0f | %6.0f\n", "avg",
				totals[PJS_TASK_ENQUEUED] / (double) _threadCount,
				totals[PJS_TASK_TAKEN] / (double) _threadCount,
				totals[PJS_TASK_STOLEN] / (double) _threadCount);
	}
}

ThreadPool::~ThreadPool() {
	PR_DestroyLock(_tpLock);
	PR_DestroyCondVar(_tpCondVar);
	delete[] _threads;
	for (int i = 0; i < _threadCount; i++)
		delete _runners[i];
	delete[] _runners;
}

// ______________________________________________________________________
// Init

ThreadPool *init(const char *scriptfn, int threadCount) {
	ThreadPool *tp = check_null(ThreadPool::create(threadCount));
	RootTaskHandle *rth = new RootTaskHandle(scriptfn);
	tp->start(rth);
	tp->await();
}

}
