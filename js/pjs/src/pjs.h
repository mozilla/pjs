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

#ifndef pjs_h___
#define pjs_h___
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
#include "proxyrack.h"
#include "jstypedarray.h"
#include "jsobj.h"
#include "jsnum.h"
#include "util.h"

#define PJS_CHECK_CX

#ifdef PJS_CHECK_CX
#  define PJS_ASSERT_CX(cx1, cx2) JS_ASSERT((cx1) == (cx2))
#else
#  define PJS_ASSERT_CX(cx1, cx2) do {} while(false)
#endif


namespace pjs {

class Runner;
class ThreadPool;
class ClonedObj;
class TaskHandle;
class ChildTaskHandle;
class RootTaskHandle;
class TaskContext;

typedef unsigned long long workCounter_t;
typedef Vector<TaskHandle*, 4, SystemAllocPolicy> TaskHandleVec;
typedef Vector<ChildTaskHandle*, 4, SystemAllocPolicy> ChildTaskHandleVec;
typedef Vector<TaskContext*, 4, SystemAllocPolicy> TaskContextVec;
typedef deque<TaskHandle*> TaskHandleDeque;

// ____________________________________________________________
// ClonedObj interface

class ClonedObj {
private:
	uint64_t *_data;
	size_t _nbytes;
	ClonedObj(uint64_t *data, size_t nbytes) :
			_data(data), _nbytes(nbytes) {
	}
public:
	~ClonedObj();
	static JSBool pack(JSContext *cx, jsval val, ClonedObj **rval);
	JSBool unpack(JSContext *cx, jsval *rval);
};

// ____________________________________________________________
// Closure interface

class Closure {
private:
	JSContext *_cx;
	auto_arr<jsval> _toProxy; // [0] == fn, [1..argc] == args
	unsigned _argc;
	unsigned _rooted;
	int _taskindex;

	Closure(JSContext *cx, auto_arr<jsval>& toProxy, unsigned argc,
			unsigned taskIndex = -1) :
			_cx(cx), _toProxy(toProxy), _argc(argc), _rooted(0),
					_taskindex(taskIndex) {
	}

	bool addRoots();
	void delRoots();

public:
	static Closure *create(JSContext *cx, jsval fn, const jsval *argv, int argc,
			int taskIndex = -1);
	~Closure();

	JSBool execute(Membrane *m, JSContext *cx, JSObject *global, jsval *rval);
};

// ____________________________________________________________
// ThreadPool interface

class ThreadPool {
private:
	int32_t _started;
	int32_t _terminating;
	int _threadCount;
	PRThread **_threads;
	Runner **_runners;
	JSObject* _global;

	PRLock *_tpLock;
	PRCondVar *_tpCondVar;
	volatile workCounter_t _workCounter;
	volatile bool _idlingWorkers;

	void signalStartBarrier();

	static void start(void* arg);

	explicit ThreadPool(PRLock *aLock, PRCondVar *aCondVar, int threadCount,
			PRThread **threads, Runner **runners) :
			_started(0), _terminating(0), _threadCount(threadCount),
					_threads(threads), _runners(runners), _tpLock(aLock),
					_tpCondVar(aCondVar), _workCounter(0), _idlingWorkers(false) {
	}

public:
	~ThreadPool();

	void awaitStartBarrier();

	void signalNewWork();
	inline workCounter_t readWorkCounter();
	void awaitNewWork(workCounter_t since);

	int runnerCount() {
		return _threadCount;
	}

	Runner *runner(int i) {
		JS_ASSERT(i < _threadCount);
		return _runners[i];
	}

	void start(RootTaskHandle *rth);

	static ThreadPool *create(int threadCount);
	void terminate();
	void await();
	int terminating() {
		return _terminating;
	}

	void setGlobal(JSObject* global) {
		_global = global;
	}
	JSObject* getGlobal() {
		return _global;
	}
};

// ____________________________________________________________
// Runner interface

#define PJS_TASK_TAKEN   0
#define PJS_TASK_STOLEN  1
#define PJS_TASK_ENQUEUED  2
#define PJS_TASK_CAT_MAX 3

class Runner {
private:
	ThreadPool *_threadPool;
	int _index;
	TaskContextVec _toReawaken;
	TaskHandleDeque _toCreate;

	// The runnerLock protects _toCreate and _toReawaken.  It could
	// be removed if those two structures were replaced with thread-safe
	// equivalents.
	PRLock *_runnerLock;

	JSRuntime *_rt;
	JSContext *_cx;
	JSObject *_global;

	int _stats[PJS_TASK_CAT_MAX];

	bool getWork(TaskContext **reawaken, TaskHandle **create);

	bool getLocalWork(TaskContext **reawaken, TaskHandle **create);

	bool stealWork(TaskHandle **create);
	bool haveWorkStolen(TaskHandle **create);

	Runner(ThreadPool *aThreadPool, int anIndex, JSRuntime *aRt, JSContext *aCx,
			JSObject *global) :
			_threadPool(aThreadPool), _index(anIndex),
					_runnerLock(PR_NewLock()), _rt(aRt), _cx(aCx),
					_global(global) {
		_stats[PJS_TASK_TAKEN] = 0;
		_stats[PJS_TASK_STOLEN] = 0;
		_stats[PJS_TASK_ENQUEUED] = 0;
	}

public:

	static Runner *create(ThreadPool *aThreadPool, int anIndex);

	~Runner() {
		if (_runnerLock)
			PR_DestroyLock(_runnerLock);
	}

	JSRuntime *rt() {
		return _rt;
	}
	JSContext *cx() {
		return _cx;
	}

	int stats(int i) {
		return _stats[i];
	}

	void start();
	void reawaken(TaskContext *ctx);
	void enqueueRootTask(RootTaskHandle *p);
	void enqueueTasks(ChildTaskHandle **begin, ChildTaskHandle **end);
	TaskContext *createTaskContext(TaskHandle *handle);
	TaskContext *createRootTaskContext(TaskHandle *handle);
	void terminate();

};

// ____________________________________________________________
// TaskHandle interface

class TaskHandle {
private:
	TaskHandle(const TaskHandle &) MOZ_DELETE;
	TaskHandle & operator=(const TaskHandle &) MOZ_DELETE;

protected:
	TaskHandle() {
	}

public:
	virtual ~TaskHandle() {
	}

	virtual JSBool execute(JSContext *cx, JSObject *global,
			auto_ptr<Membrane> &rmembrane, jsval *rval) = 0;
	virtual void onCompleted(Runner *runner, jsval result) = 0;

	virtual bool isRootTaskHandle() {
		return false;
	}
};

// ____________________________________________________________
// RootTaskHandle interface

class RootTaskHandle: public TaskHandle {
	const char *scriptfn;

public:
	RootTaskHandle(const char *afn) :
			scriptfn(afn) {
	}

	virtual JSBool execute(JSContext *cx, JSObject *global,
			auto_ptr<Membrane> &rmembrane, jsval *rval);
	virtual void onCompleted(Runner *runner, jsval result);
	virtual bool isRootTaskHandle() {
		return true;
	}
};

// ____________________________________________________________
// ChildTaskHandle interface

class ChildTaskHandle: public TaskHandle {
private:
	enum Reserved {
		ResultSlot, ReadySlot, MaxSlot
	};

	TaskContext *_parent;
	JSObject *_object;

	auto_ptr<Closure> _closure;

	ClonedObj *_result;

#   ifdef PJS_CHECK_CX
	JSContext *_checkCx;
#   endif

	JSBool addRoot(JSContext *cx);
	void delRoot(JSContext *cx);

	explicit ChildTaskHandle(JSContext *cx, TaskContext *parent,
			JSObject *object, auto_ptr<Closure> &closure) :
			_parent(parent), _object(object), _closure(closure), _result(NULL)
#       ifdef PJS_CHECK_CX
					, _checkCx(cx)
#       endif
	{
		JS_SetReservedSlot(_object, ResultSlot, JSVAL_NULL);
		JS_SetReservedSlot(_object, ReadySlot, JSVAL_FALSE);
	}

	void clearResult();

	static JSClass jsClass;
	static JSFunctionSpec jsMethods[2];
	static JSBool jsGet(JSContext *cx, unsigned argc, jsval *vp);

public:
	virtual ~ChildTaskHandle() {
		clearResult();
	}

	virtual JSBool execute(JSContext *cx, JSObject *global,
			auto_ptr<Membrane> &rmembrane, jsval *rval);
	virtual void onCompleted(Runner *runner, jsval result);

	// Invoked by the parent of the task:
	JSBool finalizeAndDelete(JSContext *cx);

	JSObject *object() {
		return _object;
	}

	static ChildTaskHandle *create(JSContext *cx, TaskContext *parent,
			auto_ptr<Closure> &closure);

	static JSBool initClass(JSContext *cx, JSObject *global);
};

// ______________________________________________________________________
// TaskContext interface

class TaskContext {
public:
	enum TaskContextSlots {
		OnCompletionSlot, MaxSlot
	};

private:
	TaskHandle *_taskHandle;
	JSObject *_global;
	JSObject *_object;
	PRInt32 _outstandingChildren;
	ChildTaskHandleVec _toFork;
	Runner *_runner;

	// The proxyRack is initially NULL. When the task becomes a parent
	// for another task, the _proxyRack is initialized. And the proxyRack
	// is passed to the child task(s) to create/reuse existing proxies.
	auto_ptr<ProxyRack> _proxyRack;

	// The membrane is initially NULL.  When _taskHandle->execute() is
	// called, it *may* create a membrane and store it in _membrane.
	// I find this a little goofy but I can't seem to come up with a
	// more elegant structure at the moment.  The situation is that
	// RootTaskHandle's do not require a membrane, and indeed cannot
	// create one as there is no parent context.  So it is really the
	// TaskHandle which ought to create the membrane, and yet it is
	// really the TaskContext which ought to store it, so that it can
	// be used during GC tracing and so forth.  So we end up with the
	// current scenario, where the task handle creates it in execute
	// but stores it in the TaskContext.
	auto_ptr<Membrane> _membrane;

#   ifdef PJS_CHECK_CX
	JSContext *_checkCx;
#   endif
	JSContext *_cx;

	TaskContext(JSContext *cx, TaskHandle *aTask, Runner *aRunner,
			JSObject *aGlobal, JSObject *object) :
			_taskHandle(aTask), _global(aGlobal), _object(object),
					_outstandingChildren(0), _runner(aRunner), _membrane(NULL), _cx(cx)
#       ifdef PJS_CHECK_CX
							, _checkCx(cx)
#       endif
	{
		setOncompletion(cx, JSVAL_NULL);
	}

	JSBool addRoot(JSContext *cx);
	JSBool delRoot(JSContext *cx);
	JSBool unpackResults(JSContext *cx);

public:
	static TaskContext *create(JSContext *cx, TaskHandle *aTask,
			Runner *aRunner, JSObject *aGlobal);

	JSContext *cx();
	JSObject *global() {
		return _global;
	}

	void addTaskToFork(ChildTaskHandle *th);

	void onChildCompleted();

	void resume(Runner *runner);

	void setOncompletion(JSContext *cx, jsval val) {
		JS_SetReservedSlot(_object, OnCompletionSlot, val);
	}

	ProxyRack *getProxyRack() {
		if (!_proxyRack.get()) {
			_proxyRack.reset(ProxyRack::create(_cx, _global));
		}
		return _proxyRack.get();
	}

	Membrane *getMembrane() {
		return _membrane.get();
	}

	JSObject *getGlobal() {
		return _global;
	}

	static JSClass jsClass;
};

ThreadPool* init(const char *initialScript, int threadCount);

}

#endif /* pjs_h___ */

