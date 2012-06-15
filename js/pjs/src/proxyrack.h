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

#ifndef PROXYRACK_H_
#define PROXYRACK_H_

#include <jsapi.h>
#include <jsgc.h>
#include <jsfriendapi.h>
#include <HashTable.h>
#include <jsproxy.h>
#include <jscompartment.h>

namespace pjs {

using namespace js;

//TODO: T at the end of the name should be removed after membrane is dismantled.
class AutoReadOnly {
private:
	JSContext *_cx;
	bool _v;

public:
	AutoReadOnly(JSContext *cx, bool ro = true);
	~AutoReadOnly();
};

struct ProxyHasher
{
    typedef const Value Lookup;

    static HashNumber hash(Lookup &key) {
    	//FIXME total hack 3 is randomly chosen.
        return key.payloadAsRawUint32() | uint32_t(3);
    }

    static bool match(Lookup &l, Lookup &k) {
        return l == k;
    }
};



class ProxyRack: BaseProxyHandler {
private:
	typedef HashMap<Value, JSObject*, ProxyHasher, SystemAllocPolicy> ProxyMap;

	JSContext *_cx;
	JSObject *_global;
	JSNative *_safeNatives;
	static char *PROXYRACK;
	ProxyMap _map;
	ProxyRack(JSContext* cx, JSObject* global);

	PRLock *_proxyRackLock;

public:
	virtual ~ProxyRack();
	static ProxyRack *create(JSContext *parentCx, JSObject *parentGlobal);
	void releaseProxies();

	JSObject *getProxyObject(JSContext* cx, const Value& priv, JSObject* proto,
			JSObject* parent, JSObject* call, JSObject* construct);

	// ______________________________________________________________________

	/* ES5 Harmony fundamental wrapper traps. */
	virtual bool getPropertyDescriptor(JSContext *cx, JSObject *wrapper,
			jsid id, bool set, PropertyDescriptor *desc) MOZ_OVERRIDE;
	virtual bool getOwnPropertyDescriptor(JSContext *cx, JSObject* wrapper,
			jsid id, bool set, PropertyDescriptor *desc) MOZ_OVERRIDE;
	virtual bool defineProperty(JSContext *cx, JSObject *wrapper, jsid id,
			PropertyDescriptor *desc) MOZ_OVERRIDE;
	virtual bool getOwnPropertyNames(JSContext *cx, JSObject *wrapper,
			AutoIdVector &props) MOZ_OVERRIDE;
	virtual bool delete_(JSContext *cx, JSObject *wrapper, jsid id, bool *bp)
			MOZ_OVERRIDE;
	virtual bool enumerate(JSContext *cx, JSObject *wrapper,
			AutoIdVector &props) MOZ_OVERRIDE;
	virtual bool fix(JSContext *cx, JSObject *proxy, Value *vp) MOZ_OVERRIDE;

	virtual void trace(JSTracer *trc, JSObject *wrapper) MOZ_OVERRIDE;

	virtual bool get(JSContext *cx, JSObject *wrapper, JSObject *receiver,
			jsid id, Value *vp) MOZ_OVERRIDE;

	// decrements the refCount of the membrane.
	virtual void finalize(JSFreeOp* fop, JSObject* proxy);

};

} /* namespace pjs */
#endif /* PROXYRACK_H_ */
