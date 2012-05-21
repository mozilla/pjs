/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// data implementation header

#ifndef nsDataChannel_h___
#define nsDataChannel_h___

#include "nsBaseChannel.h"
#include "nsIInputStream.h"
#include "nsCOMPtr.h"

class nsDataChannel : public nsBaseChannel {
public:
    nsDataChannel(nsIURI *uri) {
        SetURI(uri);
    }

protected:
    virtual nsresult OpenContentStream(bool async, nsIInputStream **result,
                                       nsIChannel** channel);
};

#endif /* nsDataChannel_h___ */
