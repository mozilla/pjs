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
 * Portions created by the Initial Developer are Copyright (C) 2012
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
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

#include "ProxyRack.h"
#include "membrane.h"
#include "util.h"

#if 1
#  define DEBUG(...)
#else
#  define DEBUG(...) fprintf(stderr, __VA_ARGS__)
#endif

namespace pjs {

AutoReadOnly::AutoReadOnly(JSContext *cx, bool ro) {
	_cx = cx;
	_v = PJS_SetReadOnly(cx, ro);
}

AutoReadOnly::~AutoReadOnly() {
	PJS_SetReadOnly(_cx, _v);
}

char *ProxyRack::PROXYRACK = "ProxyRack";

static JSObject *wrappedObject(JSObject *obj) {
	return GetProxyPrivate(obj).toObjectOrNull();
}

ProxyRack::ProxyRack(JSContext* cx, JSObject* global) :
		BaseProxyHandler(PROXYRACK), _cx(cx), _global(global),
				_proxyRackLock(PR_NewLock()), _getCount(0) {

}

ProxyRack::~ProxyRack() {
	if (getenv("PJS_STATS") != NULL) {
		fprintf(stderr, "Proxy Read Count: %d\n", _getCount);
	}
}

bool ProxyRack::getPropertyDescriptor(JSContext* cx, JSObject* wrapper, jsid id,
		bool set, PropertyDescriptor* desc) {
	desc->obj = NULL;
	;
	JS_GetPropertyDescriptorById(cx, wrappedObject(wrapper), id,
			JSRESOLVE_QUALIFIED, desc);
	//	PIERCE(cx,
	//			unwrapId(&id),
	//			JS_GetPropertyDescriptorById(cx, wrappedObject(wrapper), id,
	//			JSRESOLVE_QUALIFIED, desc),
	//			wrap(desc));
	return true;
}

bool ProxyRack::getOwnPropertyDescriptor(JSContext* cx, JSObject* wrapper,
		jsid id, bool set, PropertyDescriptor* desc) {
	AutoReadOnly ro(cx);
	//	if (!unwrapId(&id))
	//		return false;
	RootedObject rwrapper(cx, wrappedObject(wrapper));
	RootedId rid(cx, id);
	desc->obj = NULL;
	if (!GetOwnPropertyDescriptor(cx, rwrapper, rid, desc))
		return false;

	//	if (!wrap(desc))
	//		return false;
	return true;
}
bool ProxyRack::defineProperty(JSContext* cx, JSObject* wrapper, jsid id,
		PropertyDescriptor* desc) {
	JSObject* wrappee = wrappedObject(wrapper);
	JS_ReportError(cx, "Cannot define a property on a parent object");
	return false;
}

bool ProxyRack::getOwnPropertyNames(JSContext* cx, JSObject* wrapper,
		AutoIdVector& props) {
	jsid id = JSID_VOID;
	GetPropertyNames(cx, wrappedObject(wrapper), JSITER_OWNONLY | JSITER_HIDDEN,
			&props);
	//	PIERCE(cx,
	//			NOTHING,
	//			GetPropertyNames(cx, wrappedObject(wrapper),
	//			JSITER_OWNONLY | JSITER_HIDDEN, &props),
	//			wrap(props));
	return true;
}

bool ProxyRack::delete_(JSContext* cx, JSObject* wrapper, jsid id, bool* bp) {
	return false;
}

bool ProxyRack::enumerate(JSContext* cx, JSObject* wrapper,
		AutoIdVector& props) {
	GetPropertyNames(cx, wrappedObject(wrapper), 0, &props);
	//	PIERCE(cx,
	//			NOTHING,
	//			GetPropertyNames(cx, wrappedObject(wrapper), 0, &props),
	//			wrap(props));
	return true;
}

bool ProxyRack::fix(JSContext* cx, JSObject* proxy, Value* vp) {
	vp->setUndefined();
	return true;
}

void ProxyRack::trace(JSTracer* trc, JSObject* wrapper) {
}

bool ProxyRack::get(JSContext* cx, JSObject* wrapper, JSObject* receiver,
		jsid id, Value* vp) {
	_getCount++;
	JSObject* wrappee = wrappedObject(wrapper);
	if (wrappee->isArray()) {
		vp->setUndefined(); // default result if we refuse to perform this action
		RootedObject rreceiver(cx, receiver);
		RootedId rid(cx, id);
		return wrappee->getGeneric(cx, rreceiver, rid, vp);
	}
	if (wrappee->isTypedArray()) {
		vp->setUndefined();
		RootedObject rreceiver(cx, receiver);
		RootedId rid(cx, id);
		return wrappee->getGeneric(cx, rreceiver, rid, vp);
	}
	return BaseProxyHandler::get(cx, wrapper, receiver, id, vp);
}

ProxyRack* ProxyRack::create(JSContext* cx, JSObject* global) {
	ProxyRack *proxyRack = new ProxyRack(cx, global);
	if (!proxyRack->_map.init()) {
		delete proxyRack;
		return NULL;
	}
	return proxyRack;
}

void ProxyRack::releaseProxies() {
}

JSObject *ProxyRack::getProxyObject(JSContext* cx, const Value& priv,
		JSObject* proto, JSObject* parent, JSObject* call,
		JSObject* construct) {
	{
		AutoLock hold(_proxyRackLock);
		/* If we already have a proxy for this value, use it. */
		ProxyMap::Ptr p = _map.lookup(priv);
		if (p.found())
			return p->value;

		JSObject *wrapper = NewProxyObject(cx, this, priv, proto, parent, call,
				construct);
		_map.put(priv, wrapper);
		return wrapper;
	}
}

void ProxyRack::finalize(JSFreeOp* fop, JSObject* proxy) {
}

/* namespace pjs */
}

