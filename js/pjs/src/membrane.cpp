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
 * The Original Code is the PJs project.
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

#include "membrane.h"
#include <vm/String.h>

#include "jsobj.h"
#include "jscompartment.h"

using namespace JS;
using namespace js;
using namespace std;

namespace pjs {

Membrane *Membrane::create(JSContext* cx, JSObject *gl) {
    Membrane *m = new Membrane(cx, gl);
    if (!m->_map.init()) {
        delete m;
        return NULL;
    }
    
    return m;
}

bool
Membrane::IsCrossThreadWrapper(const JSObject *wrapper)
{
    if (!IsWrapper(wrapper))
        return false;
    return !!(Wrapper::wrapperHandler(wrapper)->flags() & MEMBRANE);
}

static inline JSCompartment*
GetStringCompartment(JSString *obj) {
    return reinterpret_cast<js::gc::Cell *>(obj)->compartment();
}

bool Membrane::wrap(JSObject **objp)
{
    if (!*objp)
        return true;
    AutoValueRooter tvr(_childCx, ObjectValue(**objp));
    if (!wrap(tvr.addr()))
        return false;
    *objp = &tvr.value().toObject();
    return true;
}

bool Membrane::wrap(Value *vp) {
    JSContext *cx = _childCx;

    JS_CHECK_RECURSION(cx, return false);
    
    if (!vp->isMarkable())
        return true;

    if (vp->isString()) {
        JSString *str = vp->toString();

        /* If already in child compartment, done. */
        if (str->compartment() == _childCompartment)
            return true;

        /* If the string is an atom, we don't have to copy. */
        if (str->isAtom()) {
            return true;
        }
    }

    // This code to find the global object is taken from
    // jscompartment.cpp.  I do not fully understand what is
    // going on here.
    //JSObject *global;
    //if (cx->hasfp()) {
    //    global = &cx->fp()->scopeChain().global();
    //} else {
    //    global = JS_ObjectToInnerObject(cx, cx->globalObject);
    //    if (!global)
    //        return false;
    //}
    JSObject *global = _childGlobal;

    /* Unwrap incoming objects. */
    if (vp->isObject()) {
        JSObject *obj = &vp->toObject();

        /* If already in child compartment, done. */
        if (GetObjectCompartment(obj) == _childCompartment)
            return true;

        /* Translate StopIteration singleton. */
        //NDM if (obj->isStopIteration())
        //NDM    return js_FindClassObject(cx, NULL, JSProto_StopIteration, vp);
    }


    /* If we already have a wrapper for this value, use it. */
    WrapperMap::Ptr p = _map.lookup(*vp);
    if (p.found()) {
        *vp = p->value;
        if (vp->isObject()) {
            JSObject *obj = &vp->toObject();
            JS_ASSERT(IsCrossThreadWrapper(obj));
            if (/*JS_GetClass(cx, global) != &dummy_class &&*/
                JS_GetParent(cx, obj) != global) {
                do {
                    if (!JS_SetParent(cx, obj, global))
                        return false;
                    obj = obj->getProto();
                } while (obj && IsCrossThreadWrapper(obj));
            }
        }
        return true;
    }

    /* Copy strings */
    if (vp->isString()) {
        Value orig = *vp;
        JSString *str = vp->toString();
        const jschar *chars = str->getChars(cx);
        if (!chars)
            return false;
        JSString *wrapped = JS_NewUCStringCopyN(cx, chars, str->length());
        if (!wrapped)
            return false;
        vp->setString(wrapped);
        return _map.put(orig, *vp);
    }

    JSObject *obj = &vp->toObject();

    /*
     * Recurse to wrap the prototype. Long prototype chains will run out of
     * stack, causing an error in CHECK_RECURSE.
     *
     * Wrapping the proto before creating the new wrapper and adding it to the
     * cache helps avoid leaving a bad entry in the cache on OOM. But note that
     * if we wrapped both proto and parent, we would get infinite recursion
     * here (since Object.prototype->parent->proto leads to Object.prototype
     * itself).
     */
    JSObject *proto = obj->getProto();
    if (!wrap(&proto))
        return false;

    JSObject *wrapper = New(cx, obj, proto, global, this);
    vp->setObject(*wrapper);

    if (!_map.put(GetProxyPrivate(wrapper), *vp))
        return false;

    return true;
}

bool Membrane::enter(JSContext *cx, JSObject *wrapper,
                     jsid id, Action act, bool *bp)
{
    switch (act) {
      case GET:
        return true;  // allow GET operations
      case SET:
        return false; // prevent write operations
      case CALL:
        return false; // for now, prevent call operations
    }
}

#define PIERCE(cx, wrapper, mode, pre, op, post)            \
    JS_BEGIN_MACRO                                          \
        AutoCompartment call(cx, wrappedObject(wrapper));   \
        if (!call.enter())                                  \
            return false;                                   \
        bool ok = (pre) && (op);                            \
        call.leave();                                       \
        return ok && (post);                                \
    JS_END_MACRO

#define NOTHING (true)

bool
Membrane::getPropertyDescriptor(JSContext *cx, JSObject *wrapper, jsid id,
                                               bool set, PropertyDescriptor *desc)
{
    PIERCE(cx, wrapper, set ? SET : GET,
           call.destination->wrapId(cx, &id),
           Wrapper::getPropertyDescriptor(cx, wrapper, id, set, desc),
           call.origin->wrap(cx, desc));
}

bool
Membrane::getOwnPropertyDescriptor(JSContext *cx, JSObject *wrapper, jsid id,
                                                  bool set, PropertyDescriptor *desc)
{
    PIERCE(cx, wrapper, set ? SET : GET,
           call.destination->wrapId(cx, &id),
           Wrapper::getOwnPropertyDescriptor(cx, wrapper, id, set, desc),
           call.origin->wrap(cx, desc));
}

bool
Membrane::defineProperty(JSContext *cx, JSObject *proxy, jsid id,
                                     PropertyDescriptor *desc)
{
	return false;
}

bool
Membrane::getOwnPropertyNames(JSContext *cx, JSObject *wrapper, AutoIdVector &props)
{
    PIERCE(cx, wrapper, GET,
           NOTHING,
           Wrapper::getOwnPropertyNames(cx, wrapper, props),
           call.origin->wrap(cx, props));
}

bool
Membrane::delete_(JSContext *cx, JSObject *wrapper, jsid id, bool *bp)
{
	return false;
}


bool
Membrane::enumerate(JSContext *cx, JSObject *wrapper, AutoIdVector &props)
{
    PIERCE(cx, wrapper, GET,
           NOTHING,
           Wrapper::enumerate(cx, wrapper, props),
           call.origin->wrap(cx, props));
}

bool
Membrane::has(JSContext *cx, JSObject *wrapper, jsid id, bool *bp)
{
    PIERCE(cx, wrapper, GET,
           call.destination->wrapId(cx, &id),
           Wrapper::has(cx, wrapper, id, bp),
           NOTHING);
}

bool
Membrane::hasOwn(JSContext *cx, JSObject *wrapper, jsid id, bool *bp)
{
    PIERCE(cx, wrapper, GET,
           call.destination->wrapId(cx, &id),
           Wrapper::hasOwn(cx, wrapper, id, bp),
           NOTHING);
}

bool
Membrane::get(JSContext *cx, JSObject *wrapper, JSObject *receiver, jsid id, Value *vp)
{
    PIERCE(cx, wrapper, GET,
           call.destination->wrap(cx, &receiver) && call.destination->wrapId(cx, &id),
           Wrapper::get(cx, wrapper, receiver, id, vp),
           call.origin->wrap(cx, vp));
}

bool
Membrane::set(JSContext *cx, JSObject *proxy, JSObject *receiver, jsid id, bool strict,
                          Value *vp)
{
	return false;
}

bool
Membrane::keys(JSContext *cx, JSObject *wrapper, AutoIdVector &props)
{
    PIERCE(cx, wrapper, GET,
           NOTHING,
           Wrapper::keys(cx, wrapper, props),
           call.origin->wrap(cx, props));
}

struct AutoCloseIterator
{
    AutoCloseIterator(JSContext *cx, JSObject *obj) : cx(cx), obj(obj) {}

    ~AutoCloseIterator() { if (obj) js_CloseIterator(cx, obj); }

    void clear() { obj = NULL; }

  private:
    JSContext *cx;
    JSObject *obj;
};

bool
Membrane::iterate(JSContext *cx, JSObject *wrapper, uintN flags, Value *vp)
{
    throw "FIXME";
}

bool
Membrane::call(JSContext *cx, JSObject *wrapper, uintN argc, Value *vp)
{
    AutoCompartment call(cx, wrappedObject(wrapper));
    if (!call.enter())
        return false;

    vp[0] = ObjectValue(*call.target);
    if (!call.destination->wrap(cx, &vp[1]))
        return false;
    Value *argv = JS_ARGV(cx, vp);
    for (size_t n = 0; n < argc; ++n) {
        if (!call.destination->wrap(cx, &argv[n]))
            return false;
    }
    if (!Wrapper::call(cx, wrapper, argc, vp))
        return false;

    call.leave();
    return call.origin->wrap(cx, vp);
}

bool
Membrane::construct(JSContext *cx, JSObject *wrapper, uintN argc, Value *argv,
                                   Value *rval)
{
    AutoCompartment call(cx, wrappedObject(wrapper));
    if (!call.enter())
        return false;

    for (size_t n = 0; n < argc; ++n) {
        if (!call.destination->wrap(cx, &argv[n]))
            return false;
    }
    if (!Wrapper::construct(cx, wrapper, argc, argv, rval))
        return false;

    call.leave();
    return call.origin->wrap(cx, rval);
}


JSBool
js_generic_native_method_dispatcher(JSContext *cx, uintN argc, Value *vp)
{
    JSFunctionSpec *fs = (JSFunctionSpec *)
        vp->toObject().toFunction()->getExtendedSlot(0).toPrivate();
    JS_ASSERT((fs->flags & JSFUN_GENERIC_NATIVE) != 0);

    if (argc < 1) {
        js_ReportMissingArg(cx, *vp, 0);
        return JS_FALSE;
    }

    /*
     * Copy all actual (argc) arguments down over our |this| parameter, vp[1],
     * which is almost always the class constructor object, e.g. Array.  Then
     * call the corresponding prototype native method with our first argument
     * passed as |this|.
     */
    memmove(vp + 1, vp + 2, argc * sizeof(jsval));

    /* Clear the last parameter in case too few arguments were passed. */
    vp[2 + --argc].setUndefined();

    return fs->call(cx, argc, vp);
}

bool
Membrane::nativeCall(JSContext *cx, JSObject *wrapper, Class *clasp, Native native, CallArgs srcArgs)
{
    JS_ASSERT_IF(!srcArgs.calleev().isUndefined(),
                 srcArgs.callee().toFunction()->native() == native ||
                 srcArgs.callee().toFunction()->native() == js_generic_native_method_dispatcher);
    JS_ASSERT(&srcArgs.thisv().toObject() == wrapper);
	//src/membrane.cpp:363: error: ‘struct JSObject’ has no member named ‘isMembrane’

    //JS_ASSERT(!UnwrapObject(wrapper)->isMembrane());

    JSObject *wrapped = wrappedObject(wrapper);
    AutoCompartment call(cx, wrapped);
    if (!call.enter())
        return false;

    InvokeArgsGuard dstArgs;
    if (!cx->stack.pushInvokeArgs(cx, srcArgs.length(), &dstArgs))
        return false;

    Value *src = srcArgs.base(); 
    Value *srcend = srcArgs.array() + srcArgs.length();
    Value *dst = dstArgs.base();
    for (; src != srcend; ++src, ++dst) {
        *dst = *src;
        if (!call.destination->wrap(cx, dst))
            return false;
    }

    if (!Wrapper::nativeCall(cx, wrapper, clasp, native, dstArgs))
        return false;

    dstArgs.pop();
    call.leave();
    srcArgs.rval() = dstArgs.rval();
    return call.origin->wrap(cx, &srcArgs.rval());
}

bool
Membrane::hasInstance(JSContext *cx, JSObject *wrapper, const Value *vp, bool *bp)
{
    AutoCompartment call(cx, wrappedObject(wrapper));
    if (!call.enter())
        return false;

    Value v = *vp;
    if (!call.destination->wrap(cx, &v))
        return false;
    return Wrapper::hasInstance(cx, wrapper, &v, bp);
}

JSString *
Membrane::obj_toString(JSContext *cx, JSObject *wrapper)
{
    AutoCompartment call(cx, wrappedObject(wrapper));
    if (!call.enter())
        return NULL;

    JSString *str = Wrapper::obj_toString(cx, wrapper);
    if (!str)
        return NULL;

    call.leave();
    if (!call.origin->wrap(cx, &str))
        return NULL;
    return str;
}

JSString *
Membrane::fun_toString(JSContext *cx, JSObject *wrapper, uintN indent)
{
    AutoCompartment call(cx, wrappedObject(wrapper));
    if (!call.enter())
        return NULL;

    JSString *str = Wrapper::fun_toString(cx, wrapper, indent);
    if (!str)
        return NULL;

    call.leave();
    if (!call.origin->wrap(cx, &str))
        return NULL;
    return str;
}

bool
Membrane::defaultValue(JSContext *cx, JSObject *wrapper, JSType hint, Value *vp)
{
    AutoCompartment call(cx, wrappedObject(wrapper));
    if (!call.enter())
        return false;

    if (!Wrapper::defaultValue(cx, wrapper, hint, vp))
        return false;

    call.leave();
    return call.origin->wrap(cx, vp);
}


void
Membrane::trace(JSTracer *trc, JSObject *wrapper)
{
	// TODO GC stuff
	// Maybe not needed because it's known that the parent holds
	// a reference to the wrapped object until the children are dead
    //MarkCrossCompartmentValue(trc, wrappedObject(wrapper),
    //                          "wrappedObject");
}

}
