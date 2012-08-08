/* vim: set ts=8 sw=4 et tw=99:
 *
 * ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http: //www.mozilla.org/MPL/
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
 * Nicholas Matsakis <nmatsakis@mozilla.com>
 * Donovan Preston <dpreston@mozilla.com>
 * Fadi Meawad <fmeawad@mozilla.com>
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
#include "jsatominlines.h"
#include "util.h"
#include "jstypedarray.h"

using namespace JS;
using namespace js;
using namespace std;

//#define DEBUG_DUMPS

#ifndef DEBUG_DUMPS
#  define DEBUG(...) 
#else
#  define DEBUG(...) fprintf(stderr, __VA_ARGS__)
#endif

namespace pjs {

char *Membrane::MEMBRANE = "Membrane";

static JSObject *wrappedObject(JSObject *obj) {
	return GetProxyPrivate(obj).toObjectOrNull();
}

Membrane *Membrane::create(JSContext *parentCx, JSObject *parentGlobal,
		JSContext* childCx, JSObject *childGlobal, JSNative *safeNatives,
		ProxyRack *proxyRack) {
	Membrane *m = new Membrane(parentCx, parentGlobal, childCx, childGlobal,
			safeNatives, proxyRack);
	if (!m->_map.init()) {
		delete m;
		return NULL;
	}

	return m;
}

Membrane::~Membrane() {
}

bool Membrane::IsCrossThreadWrapper(const JSObject *wrapper) {
	return wrapper->isProxy()
			&& GetProxyHandler(wrapper)->family() == (void*) MEMBRANE;
}

JSBool Membrane::put(Value key, Value value) {
	//TODO(fadi): restore rooter, or translate it into a JSArray.
//    if (_rooter == NULL) {
//        _rooter = ProxyRooter::create(_childCx, 128, NULL);
//        if (!_rooter)
//            return false;
//    }
	if (!_map.put(key, value)) {
		return false;
	}
//    if (!_rooter->add(value, &_rooter)) {
//        _map.remove(key);
//        return false;
//    }
	return true;
}

bool Membrane::isSafeNative(JSNative n) {
	for (JSNative *p = _safeNatives; *p; p++)
		if (*p == n)
			return true;
	return false;
}

bool Membrane::copyAndWrapProperties(JSObject *from, JSObject *to) {
	JSContext *cx = _childCx;
	AutoReadOnly ro(cx);
	AutoIdVector props(cx);
	if (!GetPropertyNames(cx, from, JSITER_OWNONLY | JSITER_HIDDEN, &props))
		return false;
	for (jsid *v = props.begin(), *v_end = props.end(); v < v_end; v++) {
		jsid propid = *v;
		JSPropertyDescriptor desc;
		if (!JS_GetPropertyDescriptorById(cx, from, propid, 0, &desc))
			return false;

		if (!wrapId(&propid))
			return false;
		if (!wrap(&desc))
			return false;

		AutoReadOnly ro(cx, false);
		if (!JS_DefinePropertyById(cx, to, propid, desc.value, desc.getter,
				desc.setter, desc.attrs))
			return false;
	}

	return true;
}

bool Membrane::wrap(JSObject **objp) {
	if (!*objp)
		return true;
	AutoValueRooter tvr(_childCx, ObjectValue(**objp));
	if (!wrap(tvr.addr()))
		return false;
	*objp = &tvr.value().toObject();
	_childCompartment->wrap(_childCx, objp);
	return true;
}

bool Membrane::wrap(HeapPtrAtom *objp) {
	if (!*objp)
		return true;

	AutoValueRooter tvr(_childCx, StringValue(*objp));
	if (!wrap(tvr.addr()))
		return false;
	JSString *str = tvr.value().toString();
	*objp = &str->asAtom();
	return true;
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

		if (obj == _childGlobal) {
			return _parentGlobal;
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

#       ifdef DEBUG_DUMPS
		{
			char *c_str = JS_EncodeString(cx, str);
			DEBUG("Unwrapping id string %p (%s) for %p->%p\n",
					str, c_str, _parentCx, _childCx);
			JS_free(cx, c_str);
		}
#       endif

		// FIXME: This lookup is probably not actually thread-safe.
		JSAtom *atom = js_GetExistingStringAtom(_parentCx, chars, length);
		if (atom != NULL) {
			vp->setString(atom);
			return true;
		} else {
			DEBUG("...did not find pre-existing string atom %p->%p\n",
					_parentCx, _childCx);
		}
	}

	/* We can't always unwrap. */
	return false;
}

bool Membrane::wrapId(jsid *idp) {
	if (JSID_IS_INT(*idp))
		return true;
	AutoValueRooter tvr(_childCx, IdToValue(*idp));
	if (!wrap(tvr.addr()))
		return false;
	return ValueToId(_childCx, tvr.value(), idp);
}

bool Membrane::unwrapId(jsid *idp) {
	if (JSID_IS_INT(*idp))
		return true;
	AutoValueRooter tvr(_childCx, IdToValue(*idp));
	if (!unwrap(tvr.addr()))
		return false;
	return ValueToId(_childCx, tvr.value(), idp);
}

bool Membrane::wrap(AutoIdVector &props) {
	jsid *vector = props.begin();
	int length = props.length();
	for (size_t n = 0; n < size_t(length); ++n) {
		if (!wrapId(&vector[n]))
			return false;
	}
	return true;
}

bool Membrane::wrap(PropertyOp *propp) {
	Value v = CastAsObjectJsval(*propp);
	if (!wrap(&v))
		return false;
	*propp = CastAsPropertyOp(v.toObjectOrNull());
	return true;
}

bool Membrane::wrap(StrictPropertyOp *propp) {
	Value v = CastAsObjectJsval(*propp);
	if (!wrap(&v))
		return false;
	*propp = CastAsStrictPropertyOp(v.toObjectOrNull());
	return true;
}

bool Membrane::wrap(PropertyDescriptor *desc) {
	// Some things that were non-obvious to me at first:
	// 1. We are mutating the fields of the PropertyDescriptor in place. It
	//    is an rval.
	// 2. If this is a "real" property, then the result of the "get()"
	//    will be desc->value (which we will wrap) and the desc->getter
	//    will either be NULL or JS_PropertyStub.
	// 3. Otherwise, desc->getter may be a JS function (or other kind of
	//    object).  In that case, the JSPROP_GETTER/JSPROP_SETTER flag
	//    will be set.

	return wrap(&desc->obj)
			&& (!(desc->attrs & JSPROP_GETTER) || wrap(&desc->getter))
			&& (!(desc->attrs & JSPROP_SETTER) || wrap(&desc->setter))
			&& wrap(&desc->value);
}

#define PIERCE(cx, pre, op, post)      \
    JS_BEGIN_MACRO                              \
        AutoReadOnly ro(cx);                    \
        bool ok = (pre) && (op);                \
        return ok && (post);                    \
    JS_END_MACRO

#define NOTHING (true)

bool Membrane::wrap(Value *vp, bool isArg) {
	JSContext *cx = _childCx;

	JS_CHECK_RECURSION(cx, return false);

	if (!vp->isMarkable()) {
		return true;
	}

	/* Unwrap incoming objects. */
	if (vp->isObject()) {
		JSObject *obj = &vp->toObject();

		/* Translate StopIteration singleton. */
		//NDM if (obj->isStopIteration())
		//NDM    return js_FindClassObject(cx, NULL, JSProto_StopIteration, vp);
		if (obj->isTypedArray()) {
			if (isArg /*|| obj->getSlot(DataViewObject::PJS_TASK_ID).asRawBits() == 1*/)
				return true;
		}
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

#       ifdef DEBUG_DUMPS
		{
			char *c_str = JS_EncodeString(cx, wrapped);
			DEBUG("Wrapping string %p to %p (%s) for %p->%p (%p)\n",
					str, wrapped, c_str,
					_parentCx, _childCx,
					_childCompartment);
			JS_free(cx, c_str);
		}
#       endif

		vp->setString(wrapped);
		return put(orig, *vp);
	}

	JSObject *obj = &vp->toObject();

	if (obj == _parentGlobal) {
		vp->setObject(*_childGlobal);
		return true;
	}

	/* Split closures */
	if (JS_ObjectIsFunction(cx, obj)) {
		JSFunction *fn = obj->toFunction();
//		analyzeFunction(fn, obj, cx);
		JSObject *env;
		if (!fn->isInterpreted()) {
			if (fn->isNativeConstructor()) // Assuming all native constructors does not mutate the arguments.
				return true;
			if (!isSafeNative(fn->native())) {
//				fprintf(stderr, "%s\n",
//						JS_EncodeString(cx, StringValue(fn->atom).toString()));
				JS_ReportError(cx, "Cannot access native functions "
						"from child tasks");
				return false;
			}
			env = NULL;
		} else {
			env = fn->environment();
			if (!wrap(&env))
				return false;
		}
		JSObject* wrapper = NULL;

		wrapper = JS_CloneFunctionObject(cx, obj, env);

		JSFunction *cloned_fn = wrapper->toFunction();
		if (!wrap(&cloned_fn->atom)) // FIXME: total hack
			return false;

		vp->setObject(*wrapper);
		DEBUG("Wrapping fn %p/%p to %p/%p for %p->%p (%p)\n",
				fn, fn->maybeScript(),
				wrapper, wrapper->toFunction()->maybeScript(),
				_parentCx, _childCx, _childCompartment);

//		uint32_t length = 0;
//		void * data = JS_EncodeInterpretedFunction(_parentCx, obj, &length);
//		wrapper = JS_DecodeInterpretedFunction(cx, data, length, NULL, NULL);

		if (!put(OBJECT_TO_JSVAL(obj), *vp))
			return false;

		// We now have to copy over any properties.  I do this *after*
		// putting the wrapper into the table lest there is a
		// recursive reference.
		if (!copyAndWrapProperties(obj, wrapper))
			return false;

		// if (!JS_FreezeObject(cx, wrapper))
		//    return false;

		return true;
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

	JSObject *wrapper = this->_proxyRack->getProxyObject(cx, ObjectValue(*obj),
			proto, _childGlobal, obj->isCallable() ? obj : NULL, NULL);
////	JSObject *wrapper = NewProxyObject(cx, this, ObjectValue(*obj), proto,
////			_childGlobal, obj->isCallable() ? obj : NULL, NULL);
//
//// increment the refcount of the proxy handler.
////	_refCount++;
	vp->setObject(*wrapper);
	DEBUG("Wrapping obj %p to %p for %p->%p\n", obj, wrapper, _parentCx, _childCx);
	return put(GetProxyPrivate(wrapper), *vp);
}

void Membrane::releaseProxies() {

}

enum EntryType {
	CONST, LOCAL, GLOBAL, UNDEFINED, ARGUMENT
};

class StackEntry {
public:
	EntryType type;
	int index;
	StackEntry(EntryType type, int index) :
			type(type), index(index) {
	}
};

class TypesStack {
private:
	Vector<StackEntry*> *data;

public:
	TypesStack(JSContext* cx) {
		data = new Vector<StackEntry*>(cx);
	}
	void pushN(EntryType type, unsigned int n, int index = -1) {
		while (n > 0) {
			data->append(new StackEntry(type, index));
			n--;
		}
	}
	void push(EntryType type, int index = -1) {
		pushN(type, 1, index);
	}
	StackEntry *pop() {
		if (this->length() > 0)
			return data->popCopy();
		fprintf(stderr, "empty types stack\n");
		return NULL;
	}
	void popN(unsigned int n) {
		while (n > 0) {
			free(pop());
			n--;
		}
	}
	unsigned int length() {
		return data->length();
	}
	~TypesStack() {
		data->clearAndFree();
	}
};

char *getAtom(JSScript *script, jsbytecode *pc, JSContext *cx) {
	Value v = StringValue(script->getAtom(GET_UINT32_INDEX(pc)));
	if (JSVAL_IS_OBJECT_IMPL(JSVAL_TO_IMPL(v)))
		JSObject * obj = JSVAL_TO_OBJECT(v);

	return JS_EncodeString(cx, v.toString());
}

bool Membrane::analyzeFunction(JSFunction* fn, JSObject* obj, JSContext* cx,
		int *argIDs) {
//	JS_DumpBytecode(cx, fn->script());
//	BindingNames names(cx);
//	fprintf(stderr, "%d, %d\n", fn->script()->bindings.numArgs(),
//			fn->script()->bindings.numVars());
//	fn->script()->bindings.getLocalNameArray(cx, &names);
//	for (size_t i = 0;
//			i
//					< fn->script()->bindings.numArgs()
//							+ fn->script()->bindings.numVars(); i++) {
//		JSAtom *name = names[i].maybeAtom;
//		fprintf(stderr, "%s\n",
//				JS_EncodeString(cx, StringValue(name).toString()));
//
//	}
//					fn->dump();

	char * fun_source = JS_EncodeString(cx,
			StringValue(fun_toStringHelper(cx, obj, 0)).toString());
//	fprintf(stderr, "%s\n", fun_source);

	JSScript * script = fn->script();
	unsigned length = script->length;
	TypesStack typesStack(cx);
	unsigned offset, nextOffset = 0;
	StackEntry *assigned_entry;

	JSOp prevop = JSOP_NOP;
//	fprintf(stderr, "\n");
	while (nextOffset < length) {

		offset = nextOffset;
		jsbytecode *pc = script->code + offset;

		JSOp op = (JSOp) *pc;
		prevop = op;
		JS_ASSERT(op < JSOP_LIMIT);
		/* Immediate successor of this bytecode. */
		unsigned successorOffset = offset + GetBytecodeLength(pc);

		/*
		 * Next bytecode to analyze.  This is either the successor, or is an
		 * earlier bytecode if this bytecode has a loop backedge.
		 */
		nextOffset = successorOffset;

		switch (op) {
		case JSOP_UNDEFINED:
		case JSOP_ZERO:
		case JSOP_ONE:
		case JSOP_NULL:
		case JSOP_FALSE:
		case JSOP_TRUE:
			typesStack.popN(js_CodeSpec[op].nuses);
			typesStack.pushN(CONST, js_CodeSpec[op].ndefs);
			break;
		case JSOP_ARGUMENTS:
		case JSOP_GETARG:
			typesStack.push(ARGUMENT, GET_ARGNO(pc));
			break;
		case JSOP_OBJECT:
		case JSOP_GETLOCAL:
		case JSOP_UINT16:
		case JSOP_NEWARRAY:
		case JSOP_NEWOBJECT:
		case JSOP_INCARG:
		case JSOP_DECARG:
		case JSOP_ARGINC:
		case JSOP_ARGDEC:
		case JSOP_INCLOCAL:
		case JSOP_DECLOCAL:
		case JSOP_LOCALINC:
		case JSOP_LOCALDEC:
		case JSOP_SETLOCAL:
		case JSOP_INT8:
		case JSOP_INT32:
		case JSOP_NOT:
		case JSOP_BITNOT:
		case JSOP_NEG:
		case JSOP_POS:
		case JSOP_TYPEOF:
		case JSOP_VOID:
		case JSOP_BITOR:
		case JSOP_BITXOR:
		case JSOP_BITAND:
		case JSOP_EQ:
		case JSOP_NE:
		case JSOP_LT:
		case JSOP_LE:
		case JSOP_GT:
		case JSOP_GE:
		case JSOP_LSH:
		case JSOP_RSH:
		case JSOP_URSH:
		case JSOP_ADD:
		case JSOP_SUB:
		case JSOP_MUL:
		case JSOP_DIV:
		case JSOP_MOD:
		case JSOP_NEWINIT:
		case JSOP_INITPROP:
			typesStack.popN(js_CodeSpec[op].nuses);
			typesStack.push(LOCAL);
			break;
		case JSOP_NAMEINC:
		case JSOP_SETNAME:
		case JSOP_BINDNAME:
			typesStack.popN(js_CodeSpec[op].nuses);
			typesStack.push(GLOBAL);
			break;
		case JSOP_NAME:
			typesStack.push(GLOBAL);
			break;
		case JSOP_GETELEM:
			typesStack.popN(1);
			assigned_entry = typesStack.pop();
			typesStack.push(assigned_entry->type, assigned_entry->index);
			if (assigned_entry->type == ARGUMENT)
				argIDs[assigned_entry->index] =
						argIDs[assigned_entry->index] == ArgSafe
								|| argIDs[assigned_entry->index]
										== ArgPropertySafe ?
								ArgPropertySafe : ArgUnsafe;
			free(assigned_entry);
			break;
		case JSOP_SETELEM:
			typesStack.popN(js_CodeSpec[op].nuses - 1);
			assigned_entry = typesStack.pop();
			if (assigned_entry->type == GLOBAL) {
				JS_ReportError(cx,
						"%s:%d: Cannot define a property on a parent object",
						script->filename, JS_PCToLineNumber(cx, script, pc));
				free(assigned_entry);
				return false;
			}
			if (assigned_entry->type == ARGUMENT)
				argIDs[assigned_entry->index] =
						argIDs[assigned_entry->index] == ArgSafe
								|| argIDs[assigned_entry->index]
										== ArgContextSafe ?
								ArgContextSafe : ArgUnsafe;
			typesStack.push(assigned_entry->type, assigned_entry->index);
			free(assigned_entry);
			break;
		case JSOP_SETPROP:
			typesStack.pop();
			assigned_entry = typesStack.pop();
			if (assigned_entry->type == GLOBAL) {
				JS_ReportError(cx,
						"%s:%d: Cannot define a property on a parent object",
						script->filename, JS_PCToLineNumber(cx, script, pc));
				free(assigned_entry);
				return false;
			}
			if (assigned_entry->type == ARGUMENT) {
				argIDs[assigned_entry->index] =
						argIDs[assigned_entry->index] == ArgSafe
								|| argIDs[assigned_entry->index]
										== ArgContextSafe ?
								ArgContextSafe : ArgUnsafe;
			}
			typesStack.push(assigned_entry->type, assigned_entry->index);
			free(assigned_entry);
			break;
		case JSOP_GETPROP:
			assigned_entry = typesStack.pop();
			if (assigned_entry->type == ARGUMENT)
				argIDs[assigned_entry->index] = ArgUnsafe;
			typesStack.pushN(assigned_entry->type, js_CodeSpec[op].ndefs,
					assigned_entry->index);
			free(assigned_entry);
			break;
		case JSOP_CALL:
			typesStack.popN(GET_ARGC(pc) + 2);
			typesStack.pushN(UNDEFINED, js_CodeSpec[op].ndefs);
			break;
		case JSOP_DUP:
			assigned_entry = typesStack.pop();
			typesStack.push(assigned_entry->type, assigned_entry->index);
			typesStack.push(assigned_entry->type, assigned_entry->index);
			free(assigned_entry);
			break;
		case JSOP_CALLPROP:
			assigned_entry = typesStack.pop();
			if (assigned_entry->type == ARGUMENT)
				argIDs[assigned_entry->index] = 1;
			typesStack.push(assigned_entry->type, assigned_entry->index);
			free(assigned_entry);
			break;

		default:
			if (op > 0 and op < 228) {
				if (js_CodeSpec[op].nuses >= 0)
					typesStack.popN(js_CodeSpec[op].nuses);
				if (js_CodeSpec[op].ndefs >= 0)
					typesStack.pushN(UNDEFINED, js_CodeSpec[op].ndefs);
				else
					typesStack.pushN(UNDEFINED, GET_ARGC(pc));
				break;
			} else
				fprintf(stderr, "unknown op code: %s\n", js_CodeName[op]);
			break;
		}
//		fprintf(stderr, "%s : %d\n", js_CodeName[prevop], typesStack.length());
	}

	return true;
}

}
