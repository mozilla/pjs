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

using namespace JS;
using namespace js;
using namespace std;

namespace pjs {

class AutoReadOnly
{
private:
    JSContext *_cx;
    bool _v;

public:
    AutoReadOnly(JSContext *cx) {
        _v = PJS_SetReadOnly(cx, true);
    }

    ~AutoReadOnly() {
        PJS_SetReadOnly(_cx, _v);
    }
};

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
        return _map.put(orig, *vp);
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
        vp->setObject(*wrapper);
        return _map.put(OBJECT_TO_JSVAL(obj), *vp);
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

    JSObject *wrapper = New(cx, obj, proto, _childGlobal, this);
    vp->setObject(*wrapper);
    return _map.put(GetProxyPrivate(wrapper), *vp);
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
            vp->setObject(*Wrapper::wrappedObject(obj));
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

bool Membrane::enter(JSContext *cx, JSObject *wrapper,
                     jsid id, Action act, bool *bp)
{
    switch (act) {
      case GET: {
          return true;  // allow GET operations
      }
      case SET: {
          JS_ReportError(cx, "Cannot modify parent objects");
          *bp = false;
          return false; // prevent write operations
      }
      case CALL: {
          JS_ReportError(cx, "Cannot call methods on parent objects");
          *bp = false;
          return false; // for now, prevent call operations
      }
    }
}

#define PIERCE(cx, wrapper, mode, pre, op, post)            \
    JS_BEGIN_MACRO                                          \
        AutoReadOnly ro(cx);                                \
        bool ok = (pre) && (op);                            \
        return ok && (post);                                \
    JS_END_MACRO

#define NOTHING (true)

bool
Membrane::getPropertyDescriptor(JSContext *cx, JSObject *wrapper, jsid id,
                                bool set, PropertyDescriptor *desc)
{
    PIERCE(cx, wrapper, set ? SET : GET,
           unwrapId(&id),
           Wrapper::getPropertyDescriptor(cx, wrapper, id, set, desc),
           wrap(desc));
}

bool
Membrane::getOwnPropertyDescriptor(JSContext *cx, JSObject *wrapper, jsid id,
                                                  bool set, PropertyDescriptor *desc)
{
    PIERCE(cx, wrapper, set ? SET : GET,
           unwrapId(&id),
           Wrapper::getOwnPropertyDescriptor(cx, wrapper, id, set, desc),
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
    PIERCE(cx, wrapper, GET,
           NOTHING,
           Wrapper::getOwnPropertyNames(cx, wrapper, props),
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
    PIERCE(cx, wrapper, GET,
           NOTHING,
           Wrapper::enumerate(cx, wrapper, props),
           wrap(props));
}

void
Membrane::trace(JSTracer *trc, JSObject *wrapper)
{
    
}

}
