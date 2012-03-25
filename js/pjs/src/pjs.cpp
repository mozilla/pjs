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

extern size_t gMaxStackSize;

using namespace js;
using namespace std;

#define PJS_CHECK_CX

#ifdef PJS_CHECK_CX
#  define PJS_ASSERT_CX(cx1, cx2) JS_ASSERT((cx1) == (cx2))
#else
#  define PJS_ASSERT_CX(cx1, cx2) do {} while(false)
#endif

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

typedef Vector<TaskHandle*, 4, SystemAllocPolicy> TaskHandleVec;
typedef Vector<ChildTaskHandle*, 4, SystemAllocPolicy> ChildTaskHandleVec;
typedef Vector<TaskContext*, 4, SystemAllocPolicy> TaskContextVec;
typedef deque<TaskHandle*> TaskHandleDeque;

class CrossCompartment
{
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

    ArrRooter(JSContext *cx, Value *values, int len)
        : _cx(cx)
        , _values(values)
        , _len(len)
    {}

public:
    static ArrRooter *create(JSContext *cx, Value *values, int len);
    ~ArrRooter();
};

class ClonedObj {
private:
    uint64_t *_data;
    size_t _nbytes;
    ClonedObj(uint64_t *data, size_t nbytes)
        : _data(data)
        , _nbytes(nbytes)
    {}
public:
    ~ClonedObj();
    static JSBool pack(JSContext *cx, jsval val, ClonedObj **rval);
    JSBool unpack(JSContext *cx, jsval *rval);
};

// ____________________________________________________________
// Closure interface

class AutoContextPrivate {
private:
    JSContext *_cx;

public:
    AutoContextPrivate(JSContext *cx, void *v)
        : _cx(cx)
    {
        JS_SetContextPrivate(_cx, v);
    }

    ~AutoContextPrivate()
    {
        JS_SetContextPrivate(_cx, NULL);
    }
};

// ____________________________________________________________
// Closure interface

class Closure {
private:
    JSContext *_cx;
    auto_arr<jsval> _toProxy; // [0] == fn, [1..argc] == args
    uintN _argc;
    uintN _rooted;

    Closure(JSContext *cx, auto_arr<jsval>& toProxy,
            uintN argc)
        : _cx(cx)
        , _toProxy(toProxy)
        , _argc(argc)
        , _rooted(0)
    {}

    bool addRoots();
    void delRoots();

public:
    static Closure *create(JSContext *cx, jsval fn, const jsval *argv, int argc);
    ~Closure();

    JSBool execute(Membrane *m, JSContext *cx,
                   JSObject *global, jsval *rval);
};

// ____________________________________________________________
// TaskHandle interface

class TaskHandle
{
private:
    TaskHandle(const TaskHandle &) MOZ_DELETE;
    TaskHandle & operator=(const TaskHandle &) MOZ_DELETE;

protected:
    TaskHandle()
    {}

public:
    virtual ~TaskHandle() {}

    virtual JSBool execute(JSContext *cx, JSObject *global,
                           auto_ptr<Membrane> &rmembrane, jsval *rval) = 0;
    virtual void onCompleted(Runner *runner, jsval result) = 0;
};

class RootTaskHandle : public TaskHandle
{
    const char *scriptfn;

public:
    RootTaskHandle(const char *afn)
        : scriptfn(afn)
    {}

    virtual JSBool execute(JSContext *cx, JSObject *global,
                           auto_ptr<Membrane> &rmembrane, jsval *rval);
    virtual void onCompleted(Runner *runner, jsval result);
};

class ChildTaskHandle : public TaskHandle
{
private:
    enum Reserved { ResultSlot, ReadySlot, MaxSlot };

    TaskContext *_parent;
    JSObject *_object;

    auto_ptr<Closure> _closure;

    ClonedObj *_result;

#   ifdef PJS_CHECK_CX
    JSContext *_checkCx;
#   endif

    JSBool addRoot(JSContext *cx);
    JSBool delRoot(JSContext *cx);

    explicit ChildTaskHandle(JSContext *cx, TaskContext *parent,
                             JSObject *object, auto_ptr<Closure> &closure)
        : _parent(parent)
        , _object(object)
        , _closure(closure)
        , _result(NULL)
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
    static JSBool jsGet(JSContext *cx, uintN argc, jsval *vp);

public:
    virtual ~ChildTaskHandle() {
        clearResult();
    }

    virtual JSBool execute(JSContext *cx, JSObject *global,
                           auto_ptr<Membrane> &rmembrane, jsval *rval);
    virtual void onCompleted(Runner *runner, jsval result);

    // Invoked by the parent of the task:
    JSBool finalizeAndDelete(JSContext *cx);

    JSObject *object() { return _object; }

    static ChildTaskHandle *create(JSContext *cx,
                                   TaskContext *parent,
                                   auto_ptr<Closure> &closure);

    static JSBool initClass(JSContext *cx, JSObject *global);
};

// ______________________________________________________________________
// TaskContext interface

class TaskContext
{
public:
    enum TaskContextSlots { OnCompletionSlot, MaxSlot };

private:
    TaskHandle *_taskHandle;
    JSObject *_global;
    JSObject *_object;
    jsrefcount _outstandingChildren;
    ChildTaskHandleVec _toFork;
    Runner *_runner;

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

    TaskContext(JSContext *cx, TaskHandle *aTask,
                Runner *aRunner, JSObject *aGlobal,
                JSObject *object)
        : _taskHandle(aTask)
        , _global(aGlobal)
        , _object(object)
        , _outstandingChildren(0)
        , _runner(aRunner)
        , _membrane(NULL)
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
    static TaskContext *create(JSContext *cx,
                               TaskHandle *aTask,
                               Runner *aRunner,
                               JSObject *aGlobal);

    JSContext *cx();
    JSObject *global() { return _global; }

    void addTaskToFork(ChildTaskHandle *th);

    void onChildCompleted();

    void resume(Runner *runner);

    void setOncompletion(JSContext *cx, jsval val) {
        JS_SetReservedSlot(_object, OnCompletionSlot, val);
    }

    static JSClass jsClass;
};

// ____________________________________________________________
// Global interface

class Global
{
public:
    static JSClass jsClass;
};

// ____________________________________________________________
// Runner interface

#define PJS_TASK_TAKEN   0
#define PJS_TASK_STOLEN  1
#define PJS_TASK_CAT_MAX 2

enum task_stat_cat { task_taken, task_stolen, task_stat_max };

class Runner
{
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

    int _stats[PJS_TASK_CAT_MAX];

    bool getWork(TaskContext **reawaken, TaskHandle **create);

    bool getLocalWork(TaskContext **reawaken, TaskHandle **create);

    bool stealWork(TaskHandle **create);
    bool haveWorkStolen(TaskHandle **create);

    Runner(ThreadPool *aThreadPool, int anIndex,
           JSRuntime *aRt, JSContext *aCx)
      : _threadPool(aThreadPool)
      , _index(anIndex)
      , _runnerLock(PR_NewLock())
      , _rt(aRt)
      , _cx(aCx)
    {
        _stats[PJS_TASK_TAKEN] = 0;
        _stats[PJS_TASK_STOLEN] = 0;
    }

public:
    
    static Runner *create(ThreadPool *aThreadPool, int anIndex);

    ~Runner() {
        if (_runnerLock)
            PR_DestroyLock(_runnerLock);
    }
    
    JSRuntime *rt() { return _rt; }
    JSContext *cx() { return _cx; }

    int stats(int i) { return _stats[i]; }

    void start();
    void reawaken(TaskContext *ctx);
    void enqueueRootTask(RootTaskHandle *p);
    void enqueueTasks(ChildTaskHandle **begin, ChildTaskHandle **end);
    TaskContext *createTaskContext(TaskHandle *handle);
    void terminate();
};

typedef unsigned long long workCounter_t;

class ThreadPool
{
private:
    int32_t _started;
    int32_t _terminating;
    int _threadCount;
    PRThread **_threads;
    Runner **_runners;

    PRLock *_tpLock;
    PRCondVar *_tpCondVar;
    volatile workCounter_t _workCounter;
    volatile bool _idlingWorkers;

    void signalStartBarrier();

    static void start(void* arg) {
        ((Runner*) arg)->start();
    }

    explicit ThreadPool(PRLock *aLock, PRCondVar *aCondVar,
                        int threadCount, PRThread **threads, Runner **runners)
        : _terminating(0)
        , _threadCount(threadCount)
        , _threads(threads)
        , _runners(runners)
        , _tpLock(aLock)
        , _tpCondVar(aCondVar)
        , _workCounter(0)
        , _idlingWorkers(false)
    {
    }

public:
    ~ThreadPool() {
        PR_DestroyLock(_tpLock);
        PR_DestroyCondVar(_tpCondVar);
        delete[] _threads;
        for (int i = 0; i < _threadCount; i++)
            delete _runners[i];
        delete[] _runners;
    }

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

    static ThreadPool *create();
    void terminate();
    void await();
    int terminating() { return _terminating; }
};

// ______________________________________________________________________
// Global functions

/* The error reporter callback. */
void reportError(JSContext *cx, const char *message, JSErrorReport *report)
{
    fprintf(stderr, "%s:%u:%s\n",
            report->filename ? report->filename : "<no filename>",
            (unsigned int) report->lineno,
            message);
}

JSBool print(JSContext *cx, uintN argc, jsval *vp) {
    jsval *argv;
    uintN i;
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

JSBool assert(JSContext *cx, uintN argc, jsval *vp) {
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

JSBool fork(JSContext *cx, uintN argc, jsval *vp) {
    TaskContext *taskContext = (TaskContext*) JS_GetContextPrivate(cx);

    jsval *argv = JS_ARGV(cx, vp);
    auto_ptr<Closure> closure(Closure::create(cx, argv[0], argv+1, argc-1));
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

JSBool oncompletion(JSContext *cx, uintN argc, jsval *vp) {
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

static JSFunctionSpec pjsGlobalFunctions[] = {
    JS_FN("print", print, 0, 0),
    JS_FN("assert", assert, 2, 0),
    JS_FN("fork", fork, 1, 0),
    JS_FN("oncompletion", oncompletion, 1, 0),
    JS_FS_END
};

// ______________________________________________________________________
// Global impl

JSClass Global::jsClass = {
    "Global", JSCLASS_GLOBAL_FLAGS,
    JS_PropertyStub, JS_PropertyStub, JS_PropertyStub, JS_StrictPropertyStub,
    JS_EnumerateStub, JS_ResolveStub, JS_ConvertStub, JS_FinalizeStub,
    JSCLASS_NO_OPTIONAL_MEMBERS
};

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

fail:
    for (int i = 0; i < rooted; i++) {
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
                                  JS_STRUCTURED_CLONE_VERSION, rval,
                                  NULL, NULL);
}

// ______________________________________________________________________
// Closure impl

Closure *Closure::create(JSContext *cx, jsval fn, const jsval *argv, int argc) {
    TaskContext *taskContext = (TaskContext*) JS_GetContextPrivate(cx);

    // Create an array containing
    // [0] - the function
    // [1..N] - the arguments to the function

    auto_arr<jsval> toProxy(new jsval[1+argc]);
    if (!toProxy.get()) { JS_ReportOutOfMemory(cx); return NULL; }
    int p = 0;
    toProxy[p++] = fn;
    for (int i = 0; i < argc; i++) {
        toProxy[p++] = argv[i];
    }

    auto_ptr<Closure> c(new Closure(cx, toProxy, argc));
    if (!c.get()) { JS_ReportOutOfMemory(cx); return NULL; }
    if (!c->addRoots()) { return NULL; }
    return c.release();
}

bool Closure::addRoots() {
    for (; _rooted <= _argc; _rooted++) {
        if (!JS_AddNamedValueRoot(_cx, &_toProxy[_rooted], "Closure::toProxyArgv[]"))
            return false;
    }
}

void Closure::delRoots() {
    for (uintN i = 0; i < _rooted; i++)
        JS_RemoveValueRoot(_cx, &_toProxy[i]);
}

Closure::~Closure() {
    delRoots();
}

JSBool Closure::execute(Membrane *m, JSContext *cx,
                        JSObject *global, jsval *rval) {

    // Wrap the function:
    int p = 0;
    AutoValueRooter fn(cx, _toProxy[p++]);
    if (!m->wrap(fn.addr())) { return false; }
    JS_ASSERT(ValueIsFunction(cx, fn.value()));

    auto_arr<jsval> argv(new jsval[_argc]);          // ensure it gets freed
    if (!argv.get()) return JS_FALSE;
    AutoArrayRooter argvRoot(cx, _argc, argv.get()); // ensure it is rooted
    for (int i = 0; i < _argc; i++) {
        argv[i] = _toProxy[p++];
        if (!m->wrap(&argv[i]))
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

    static JSNative safeNatives[] = {
        ChildTaskHandle::jsGet,
        NULL
    };

    auto_ptr<Membrane> m(Membrane::create(_parent->cx(), _parent->global(),
                                          cx, global,
                                          safeNatives));
    if (!m.get()) {
        return false;
    }
    rmembrane = m;
    return _closure->execute(rmembrane.get(), cx, global, rval);
}

ChildTaskHandle *ChildTaskHandle::create(JSContext *cx,
                                         TaskContext *parent,
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

JSBool ChildTaskHandle::delRoot(JSContext *cx) {
    PJS_ASSERT_CX(cx, _checkCx);
    return JS_RemoveObjectRoot(cx, &_object);
}

JSBool ChildTaskHandle::initClass(JSContext *cx, JSObject *global) {
    return !!JS_InitClass(cx, 
                          global, NULL, // obj, parent_proto
                          &jsClass,     // clasp
                          NULL, 0,      // constructor, nargs
                          NULL,         // JSPropertySpec *ps
                          jsMethods,    // JSFunctionSpec *fs
                          NULL,         // JSPropertySpec *static_ps
                          NULL);        // JSFunctionSpec *static_fs
}

JSClass ChildTaskHandle::jsClass = {
    "TaskHandle", JSCLASS_HAS_PRIVATE | JSCLASS_HAS_RESERVED_SLOTS(MaxSlot),
    JS_PropertyStub, JS_PropertyStub, JS_PropertyStub, JS_StrictPropertyStub,
    JS_EnumerateStub, JS_ResolveStub, JS_ConvertStub, JS_FinalizeStub,
    JSCLASS_NO_OPTIONAL_MEMBERS
};

JSFunctionSpec ChildTaskHandle::jsMethods[2] = {
    { "get", &jsGet, 0, 0 },
    JS_FS_END
};

JSBool ChildTaskHandle::jsGet(JSContext *cx, uintN argc, jsval *vp) {
    TaskContext *taskContext = static_cast<TaskContext*>(
        JS_GetContextPrivate(cx));
    JSObject *self = JS_THIS_OBJECT(cx, vp);

    // Is this a proxied task handle?
    if (Membrane::IsCrossThreadWrapper(self)) {
        JS_ReportError(cx, "all child tasks not yet completed");
        return false;
    }

    // Sanity check
    if (!JS_InstanceOf(cx, self, &jsClass, NULL)) {
        JS_ReportError(cx, "expected TaskHandle as this");
        return JS_FALSE;
    }

    // Check if the generation has completed:
    jsval ready = JS_GetReservedSlot(self, ReadySlot);
    if (ready != JSVAL_TRUE) {
        JS_ReportError(cx, "all child tasks not yet completed");
        return false;
    }

    // If it has, there should be a value waiting for us:
    *vp = JS_GetReservedSlot(self, ResultSlot);
    return true;
}

// ______________________________________________________________________
// TaskContext impl

TaskContext *TaskContext::create(JSContext *cx,
                                 TaskHandle *aTask,
                                 Runner *aRunner,
                                 JSObject *aGlobal) {
    // To start, create the JS object representative:
    JSObject *object = JS_NewObject(cx, &jsClass, NULL, NULL);
    if (!object) {
        return NULL;
    }

    // Create C++ object:
    auto_ptr<TaskContext> tc(new TaskContext(cx, aTask, aRunner, aGlobal, object));
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
    jsrefcount v = JS_ATOMIC_DECREMENT(&_outstandingChildren);
    if (v == 0) {
        _runner->reawaken(this);
    }
}

JSBool TaskContext::unpackResults(JSContext *cx) {
    JSBool result = true;

    for (ChildTaskHandle **p = _toFork.begin(), **pn = _toFork.end();
         p < pn;
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
    delete this;
    return;
}

JSClass TaskContext::jsClass = {
    "TaskContext", JSCLASS_HAS_PRIVATE | JSCLASS_HAS_RESERVED_SLOTS(MaxSlot),
    JS_PropertyStub, JS_PropertyStub, JS_PropertyStub, JS_StrictPropertyStub,
    JS_EnumerateStub, JS_ResolveStub, JS_ConvertStub, JS_FinalizeStub,
    JSCLASS_NO_OPTIONAL_MEMBERS
};

// ______________________________________________________________________
// Runner impl

Runner *Runner::create(ThreadPool *aThreadPool, int anIndex) {
    JSRuntime *rt = check_null(JS_NewRuntime(10L * 1024L * 1024L));
    JSContext *cx = check_null(JS_NewContext(rt, 8192));
    JS_SetOptions(cx, JSOPTION_VAROBJFIX | JSOPTION_METHODJIT);
    JS_SetVersion(cx, JSVERSION_LATEST);
    JS_SetErrorReporter(cx, reportError);
    JS_ClearRuntimeThread(rt);

    char *pjs_zeal = getenv("PJS_ZEAL");
    if (pjs_zeal) {
        int v1, v2;
        if (sscanf(pjs_zeal, "%d,%d", &v1, &v2) < 2) {
            fprintf(stderr, "PJS_ZEAL should have the form N,N, not %s\n",
                    pjs_zeal);
            exit(1);
        }
        JS_SetGCZeal(cx, v1, v2, false);
    }
    return new Runner(aThreadPool, anIndex, rt, cx);
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
        if (_threadPool->runner(i)->haveWorkStolen(create))
            return true;

    for (int i = 0; i < _index; i++)
        if (_threadPool->runner(i)->haveWorkStolen(create))
            return true;

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

    // no need to signal new work for the root task.
}

void Runner::enqueueTasks(ChildTaskHandle **begin, ChildTaskHandle **end) {
    {
        AutoLock hold(_runnerLock);
        for (ChildTaskHandle **p = begin; p < end; p++)
            _toCreate.push_front(*p);
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
            TaskContext *ctx = createTaskContext(create);
            if (ctx) {
                ctx->resume(this);
            }
        }
        JS_EndRequest(_cx);
    }
}

TaskContext *Runner::createTaskContext(TaskHandle *handle) {
    JSObject *global = JS_NewCompartmentAndGlobalObject(
        /*JSContext *cx: */ _cx, 
        /*JSClass *clasp: */ &Global::jsClass,
        /*JSPrincipals*/ NULL);

    CrossCompartment cc(_cx, global);

    if (!JS_InitStandardClasses(_cx, global))
        return NULL;
        
    if (!JS_DefineFunctions(_cx, global, pjsGlobalFunctions))
        return NULL;

    if (!ChildTaskHandle::initClass(_cx, global))
        return NULL;

    return TaskContext::create(_cx, handle, this, global);
}

void Runner::terminate() {
    _threadPool->terminate();
}

// ______________________________________________________________________
// ThreadPool impl

ThreadPool *ThreadPool::create() {
    PRLock *lock = check_null(PR_NewLock());
    PRCondVar *condVar = check_null(PR_NewCondVar(lock));

    const int threadCount = 4; // for now

    PRThread **threads = check_null(new PRThread*[threadCount]);
    memset(threads, 0, sizeof(PRThread*) * threadCount);

    Runner **runners = check_null(new Runner*[threadCount]);
    memset(threads, 0, sizeof(Runner*) * threadCount);

    ThreadPool *tp = check_null(
        new ThreadPool(lock, condVar, threadCount, threads, runners));

    for (int i = 0; i < threadCount; i++) {
        runners[i] = check_null(Runner::create(tp, i));
        threads[i] = PR_CreateThread(PR_USER_THREAD, 
                                     start, runners[i], 
                                     PR_PRIORITY_NORMAL,
                                     PR_LOCAL_THREAD, 
                                     PR_JOINABLE_THREAD, 
                                     0);
        check_null(threads[i]);
    }

    return tp;
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
        if (_terminating) return;
        if (_workCounter != since) return;

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
        fprintf(stderr, "Runner | Taken | Stolen\n");
        for (int i = 0; i < _threadCount; i++) {
            fprintf(stderr, "%6d | %5d | %6d\n",
                    i, _runners[i]->stats(PJS_TASK_TAKEN),
                    _runners[i]->stats(PJS_TASK_STOLEN));
            for (int j = 0; j < PJS_TASK_CAT_MAX; j++)
                totals[j] += _runners[i]->stats(j);
        }
        fprintf(stderr, "%6s | %5d | %6d\n", "total", totals[PJS_TASK_TAKEN],
                totals[PJS_TASK_STOLEN]);
        fprintf(stderr, "%6s | %5.0f | %6.0f\n", "avg",
                totals[PJS_TASK_TAKEN] / (double) _threadCount,
                totals[PJS_TASK_STOLEN] / (double) _threadCount);
    }
}

// ______________________________________________________________________
// Init

ThreadPool *init(const char *scriptfn) {
    ThreadPool *tp = check_null(ThreadPool::create());
    RootTaskHandle *rth = new RootTaskHandle(scriptfn);
    tp->start(rth);
    tp->await();
}

}
