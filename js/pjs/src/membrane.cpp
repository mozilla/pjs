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
#include "jsinterp.h"
#include "util.h"

using namespace JS;
using namespace js;
using namespace std;

#if 1
#  define DEBUG(...) 
#else
#  define DEBUG(...) fprintf(stderr, __VA_ARGS__)
#endif

namespace pjs {

char *Membrane::MEMBRANE = "Membrane";

static JSObject *wrappedObject(JSObject *obj) {
    return GetProxyPrivate(obj).toObjectOrNull();
}

class AutoReadOnly
{
private:
    JSContext *_cx;
    bool _v;

public:
    AutoReadOnly(JSContext *cx) {
        _cx = cx;
        _v = PJS_SetReadOnly(cx, true);
    }

    ~AutoReadOnly() {
        PJS_SetReadOnly(_cx, _v);
    }
};

class ProxyRooter
{
private:
    JSContext *_cx;
    auto_arr<Value> _values;
    int _size, _capacity;
    ProxyRooter *_next;

    ProxyRooter(JSContext *cx, auto_arr<Value> &v, int c, ProxyRooter *n)
        : _cx(cx)
        , _values(v)
        , _size(0)
        , _capacity(c)
        , _next(n)
    {}

public:
    static ProxyRooter *create(JSContext *cx, int c, ProxyRooter *n) {
        auto_arr<Value> values(new Value[c]);
        if (!values.get()) {
            JS_ReportOutOfMemory(cx);
            return NULL;
        }

        ProxyRooter *pr = new ProxyRooter(cx, values, c, n);
        if (!pr) {
            JS_ReportOutOfMemory(cx);
            return NULL;
        }

        return pr;
    }

    ~ProxyRooter() {
        for (int i = 0; i < _size; i++) {
            JS_RemoveValueRoot(_cx, &_values[i]);
        }
        delete _next;
    }

    JSBool add(Value value, ProxyRooter **rval) {
        JS_ASSERT(*rval == this);

        if (_size == _capacity) {
            ProxyRooter *pr = ProxyRooter::create(_cx, _capacity, this);
            if (!pr) {
                return false;
            }
            *rval = pr;
            return pr->add(value, rval);
        }

        _values[_size] = value;
        if (!JS_AddValueRoot(_cx, &_values[_size]))
            return false;
        _size++;
        return true;
    }
};

Membrane *Membrane::create(JSContext *parentCx, JSContext* childCx, JSObject *gl) {
    Membrane *m = new Membrane(parentCx, childCx, gl);
    if (!m->_map.init()) {
        delete m;
        return NULL;
    }
    
    return m;
}

Membrane::~Membrane() {
    delete _rooter;
}

bool
Membrane::IsCrossThreadWrapper(const JSObject *wrapper)
{
    return wrapper->isWrapper() && GetProxyHandler(wrapper) == (void*)MEMBRANE;
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

JSBool Membrane::put(Value key, Value value) {
    if (_rooter == NULL) {
        _rooter = ProxyRooter::create(_childCx, 128, NULL);
        if (!_rooter)
            return false;
    }
    if (!_map.put(key, value)) {
        return false;
    }
    if (!_rooter->add(value, &_rooter)) {
        _map.remove(key);
        return false;
    }
    return true;
}

bool Membrane::wrap(Value *vp) {
    JSContext *cx = _childCx;

    JS_CHECK_RECURSION(cx, return false);
    
    if (!vp->isMarkable())
        return true;

    /* Unwrap incoming objects. */
    if (vp->isObject()) {
        JSObject *obj = &vp->toObject();
        JS_ASSERT(obj->compartment() != _childCompartment);

        /* Translate StopIteration singleton. */
        //NDM if (obj->isStopIteration())
        //NDM    return js_FindClassObject(cx, NULL, JSProto_StopIteration, vp);
    }

    /* If we already have a wrapper for this value, use it. */
    WrapperMap::Ptr p = _map.lookup(*vp);
    if (p.found()) {
        *vp = p->value;
        return true;
    }

    /* Copy strings */
    if (vp->isString()) {
        Value orig = *vp;
        JSString *str = vp->toString();
        const jschar *chars = str->getChars(cx);
        if (!chars)
            return false;

        JSString *wrapped;
        if (!str->isAtom()) {
            wrapped = JS_NewUCStringCopyN(cx, chars, str->length());
        } else {
            wrapped = js_AtomizeChars(cx, chars, str->length());
        }

        if (!wrapped)
            return false;
        vp->setString(wrapped);
        return put(orig, *vp);
    }

    JSObject *obj = &vp->toObject();

    /* Split closures */
    if (JS_ObjectIsFunction(cx, obj)) {
        JSFunction *fn = obj->toFunction();
        if (!fn->isInterpreted()) {
            JS_ReportError(cx, "Cannot access native functions from child tasks");
            return false;
        }

        JSObject *env = fn->environment();
        if (!wrap(&env))
            return false;

        JSObject *wrapper = JS_CloneFunctionObject(cx, obj, env);
        JSFunction *cloned_fn = wrapper->toFunction();
        JS_ASSERT(cloned_fn->atom == NULL); // FIXME: clone this
        vp->setObject(*wrapper);
        DEBUG("Wrapping fn %p/%p to %p/%p for %p->%p\n",
              fn, fn->maybeScript(),
              wrapper, wrapper->toFunction()->maybeScript(),
              _parentCx, _childCx);
        return put(OBJECT_TO_JSVAL(obj), *vp);
    }

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

    // FIXME--using global as the parent parameter to New() here is
    // wrong but should work for now.  We should eventually proxy it.

    JSObject *wrapper = NewProxyObject(
        cx, this, ObjectValue(*obj), proto, _childGlobal,
        obj->isCallable() ? obj : NULL, NULL);
    vp->setObject(*wrapper);
    DEBUG("Wrapping obj %p to %p for %p->%p\n",
          obj, wrapper,
          _parentCx, _childCx);
    return put(GetProxyPrivate(wrapper), *vp);
}

bool Membrane::unwrap(Value *vp) {
    JSContext *cx = _childCx;

    JS_CHECK_RECURSION(cx, return false);
    
    if (!vp->isMarkable())
        return true;

    /* Unwrap incoming objects. */
    if (vp->isObject()) {
        JSObject *obj = &vp->toObject();
        JS_ASSERT(obj->compartment() == _childCompartment);

        if (IsCrossThreadWrapper(obj)) {
            vp->setObject(*wrappedObject(obj));
            return true;
        }
    }

    /* Lookup strings and return any existing ones. */
    if (vp->isString()) {
        Value orig = *vp;
        JSString *str = vp->toString();
        size_t length = str->length();
        const jschar *chars = str->getChars(cx);
        if (!chars)
            return false;

        // FIXME: This lookup is probably not actually thread-safe.
        JSAtom *atom = js_GetExistingStringAtom(_parentCx, chars, length);
        if (atom != NULL) {
            vp->setString(atom);
            return true;
        }
    }

    /* We can't always unwrap. */
    return false;
}

bool
Membrane::wrapId(jsid *idp)
{
    if (JSID_IS_INT(*idp))
        return true;
    AutoValueRooter tvr(_childCx, IdToValue(*idp));
    if (!wrap(tvr.addr()))
        return false;
    return ValueToId(_childCx, tvr.value(), idp);
}

bool
Membrane::unwrapId(jsid *idp)
{
    if (JSID_IS_INT(*idp))
        return true;
    AutoValueRooter tvr(_childCx, IdToValue(*idp));
    if (!unwrap(tvr.addr()))
        return false;
    return ValueToId(_childCx, tvr.value(), idp);
}

bool
Membrane::wrap(AutoIdVector &props)
{
    jsid *vector = props.begin();
    jsint length = props.length();
    for (size_t n = 0; n < size_t(length); ++n) {
        if (!wrapId(&vector[n]))
            return false;
    }
    return true;
}

bool
Membrane::wrap(PropertyOp *propp)
{
    Value v = CastAsObjectJsval(*propp);
    if (!wrap(&v))
        return false;
    *propp = CastAsPropertyOp(v.toObjectOrNull());
    return true;
}

bool
Membrane::wrap(StrictPropertyOp *propp)
{
    Value v = CastAsObjectJsval(*propp);
    if (!wrap(&v))
        return false;
    *propp = CastAsStrictPropertyOp(v.toObjectOrNull());
    return true;
}

bool
Membrane::wrap(PropertyDescriptor *desc)
{
    // Some things that were non-obvious to me at first:
    // 1. We are mutating the fields of the PropertyDescriptor in place. It
    //    is an rval.
    // 2. If this is a "real" property, then the result of the "get()"
    //    will be desc->value (which we will wrap) and the desc->getter
    //    will either be NULL or JS_PropertyStub.
    // 3. Otherwise, desc->getter may be a JS function (or other kind of
    //    object).  In that case, the JSPROP_GETTER/JSPROP_SETTER flag
    //    will be set.
    
    return wrap(&desc->obj) &&
           (!(desc->attrs & JSPROP_GETTER) || wrap(&desc->getter)) &&
           (!(desc->attrs & JSPROP_SETTER) || wrap(&desc->setter)) &&
           wrap(&desc->value);
}

#define PIERCE(cx, wrapper, pre, op, post)      \
    JS_BEGIN_MACRO                              \
        AutoReadOnly ro(cx);                    \
        bool ok = (pre) && (op);                \
        return ok && (post);                    \
    JS_END_MACRO

#define NOTHING (true)

bool
Membrane::getPropertyDescriptor(JSContext *cx, JSObject *wrapper, jsid id,
                                bool set, PropertyDescriptor *desc)
{
    desc->obj = NULL; // default result if we refuse to perform this action
    PIERCE(cx, wrapper,
           unwrapId(&id),
           JS_GetPropertyDescriptorById(cx, wrappedObject(wrapper), id, JSRESOLVE_QUALIFIED, desc),
           wrap(desc));
}

bool
Membrane::getOwnPropertyDescriptor(JSContext *cx, JSObject *wrapper, jsid id,
                                                  bool set, PropertyDescriptor *desc)
{
    desc->obj = NULL;
    PIERCE(cx, wrapper,
           unwrapId(&id),
           GetOwnPropertyDescriptor(cx, wrappedObject(wrapper), id, desc),
           wrap(desc));
}

bool
Membrane::defineProperty(JSContext *cx, JSObject *proxy, jsid id,
                                     PropertyDescriptor *desc)
{
    JS_ReportError(cx, "Cannot define a property on a parent object");
    return false;
}

bool
Membrane::getOwnPropertyNames(JSContext *cx, JSObject *wrapper, AutoIdVector &props)
{
    jsid id = JSID_VOID;
    PIERCE(cx, wrapper,
           NOTHING,
           GetPropertyNames(cx, wrappedObject(wrapper), JSITER_OWNONLY | JSITER_HIDDEN, &props),
           wrap(props));
}

bool
Membrane::delete_(JSContext *cx, JSObject *wrapper, jsid id, bool *bp)
{
	return false;
}


bool
Membrane::enumerate(JSContext *cx, JSObject *wrapper, AutoIdVector &props)
{
    PIERCE(cx, wrapper,
           NOTHING,
           GetPropertyNames(cx, wrappedObject(wrapper), 0, &props),
           wrap(props));
}

bool
Membrane::fix(JSContext *cx, JSObject *wrapper, Value *vp)
{
    vp->setUndefined();
    return true;
}

void
Membrane::trace(JSTracer *trc, JSObject *wrapper)
{
    
}

}
