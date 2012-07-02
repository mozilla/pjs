/*
 * Copyright 2012 Google Inc.
 *
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 */

#ifndef SkOTTable_post_DEFINED
#define SkOTTable_post_DEFINED

#include "SkEndian.h"
#include "SkOTTableTypes.h"
#include "SkTypedEnum.h"

#pragma pack(push, 1)

struct SkOTTablePostScript {
    struct Format {
        SK_TYPED_ENUM(Value, SK_OT_Fixed,
            ((version1, SkTEndian_SwapBE32(0x00010000)))
            ((version2, SkTEndian_SwapBE32(0x00020000)))
            ((version2_5, SkTEndian_SwapBE32(0x00025000)))
            ((version3, SkTEndian_SwapBE32(0x00030000)))
            ((version4, SkTEndian_SwapBE32(0x00040000)))
            SK_SEQ_END,
        SK_SEQ_END)
        SK_OT_Fixed value;
    } format;
    SK_OT_Fixed italicAngle;
    SK_OT_FWORD underlinePosition;
    SK_OT_FWORD underlineThickness;
    SK_OT_ULONG isFixedPitch;
    SK_OT_ULONG minMemType42;
    SK_OT_ULONG maxMemType42;
    SK_OT_ULONG minMemType1;
    SK_OT_ULONG maxMemType1;
};

#pragma pack(pop)


#include <stddef.h>
SK_COMPILE_ASSERT(offsetof(SkOTTablePostScript, maxMemType1) == 28, SkOTTablePostScript_maxMemType1_not_at_28);
SK_COMPILE_ASSERT(sizeof(SkOTTablePostScript) == 32, sizeof_SkOTTablePostScript_not_32);

#endif
