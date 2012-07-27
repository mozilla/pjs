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

#ifndef MEMBRANE_H
#define MEMBRANE_H

#include <jsapi.h>
#include <jsgc.h>
#include <jsfriendapi.h>
#include <HashTable.h>
#include <jsproxy.h>
#include <jscompartment.h>
#include "proxyrack.h"

namespace pjs {

using namespace js;
using namespace std;

class Membrane {
private:
	// Maps from proxies in the parent space to wrapper object in
	// child space.
	WrapperMap _map;
	JSContext *_parentCx;
	JSObject *_parentGlobal;
	JSContext *_childCx;
	JSObject *_childGlobal;
	JSCompartment *_childCompartment;
	JSNative *_safeNatives;
	ProxyRack *_proxyRack;

	Membrane(JSContext *parentCx, JSObject *parentGlobal, JSContext *childCx,
			JSObject *childGlobal, JSNative *safeNatives, ProxyRack *proxyRack) :
			_parentCx(parentCx), _parentGlobal(parentGlobal), _childCx(childCx),
					_childGlobal(childGlobal),
					_childCompartment(_childGlobal->compartment()),
					_safeNatives(safeNatives), _proxyRack(proxyRack) {
	}

	bool isSafeNative(JSNative n);

	JSBool put(Value key, Value value);

	bool copyAndWrapProperties(JSObject *from, JSObject *to);


	static char *MEMBRANE;

public:
	static Membrane *create(JSContext *parentCx, JSObject *parentGlobal,
			JSContext* childCx, JSObject *childGlobal, JSNative *safeNatives,
			ProxyRack *proxyRack);
	~Membrane();
	void releaseProxies();

	bool wrap(Value *vp, bool isArg=false);
	bool wraptemp(Value *vp, bool isArg=false);
	bool wrapId(jsid *idp);
	bool unwrap(Value *vp);

	bool unwrapId(jsid *idp);
	bool wrap(AutoIdVector &props);
	bool wrap(PropertyOp *propp);
	bool wrap(StrictPropertyOp *propp);
	bool wrap(PropertyDescriptor *desc);
	bool wrap(JSObject **objp);
	bool wrap(HeapPtrAtom *objp);


	static bool IsCrossThreadWrapper(const JSObject *wrapper);

//	const static int TYPED_ARRAY_NOWRAP_SLOT = 5;
	// ______________________________________________________________________
};

}

#endif
