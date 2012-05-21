/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "unwind_i.h"

static const char *regname[] =
  {
    /* 0.  */
    "r0",  "r1",  "r2",  "r3",  "r4",  "r5",  "r6",  "r7",
    /* 8.  */
    "r8",  "r9",  "r10", "fp",  "ip",  "sp",  "lr",  "pc",
    /* 16.  Obsolete FPA names.  */
    "f0",  "f1",  "f2",  "f3",  "f4",  "f5",  "f6",  "f7",
    /* 24.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 32.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 40.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 48.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 56.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 64.  */
    "s0",  "s1",  "s2",  "s3",  "s4",  "s5",  "s6",  "s7",
    /* 72.  */
    "s8",  "s9",  "s10", "s11", "s12", "s13", "s14", "s15",
    /* 80.  */
    "s16", "s17", "s18", "s19", "s20", "s21", "s22", "s23",
    /* 88.  */
    "s24", "s25", "s26", "s27", "s28", "s29", "s30", "s31",
    /* 96.  */
    "f0",  "f1",  "f2",  "f3",  "f4",  "f5",  "f6",  "f7",
    /* 104.  */
    "wCGR0", "wCGR1", "wCGR2", "wCGR3", "wCGR4", "wCGR5", "wCGR6", "wCGR7",
    /* 112.  */
    "wR0", "wR1", "wR2", "wR3", "wR4", "wR5", "wR6", "wR7",
    /* 128.  */
    "spsr", "spsr_fiq", "spsr_irq", "spsr_abt", "spsr_und", "spsr_svc", 0, 0,
    /* 136.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 144.  */
    "r8_usr", "r9_usr", "r10_usr", "r11_usr", "r12_usr", "r13_usr", "r14_usr",
    /* 151.  */
    "r8_fiq", "r9_fiq", "r10_fiq", "r11_fiq", "r12_fiq", "r13_fiq", "r14_fiq",
    /* 158.  */
    "r13_irq", "r14_irq",
    /* 160.  */
    "r13_abt", "r14_abt",
    /* 162.  */
    "r13_und", "r14_und",
    /* 164.  */
    "r13_svc", "r14_svc", 0, 0,
    /* 168.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 176.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 184.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 192.  */
    "wC0", "wC1", "wC2", "wC3", "wC4", "wC5", "wC6", "wC7",
    /* 200.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 208.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 216.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 224.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 232.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 240.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 248.  */
    0, 0, 0, 0, 0, 0, 0, 0,
    /* 256.  */
    "d0",  "d1",  "d2",  "d3",  "d4",  "d5",  "d6",  "d7",
    /* 264.  */
    "d8",  "d9",  "d10", "d11", "d12", "d13", "d14", "d15",
    /* 272.  */
    "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23",
    /* 280.  */
    "d24", "d25", "d26", "d27", "d28", "d29", "d30", "d31",
  };

PROTECTED const char *
unw_regname (unw_regnum_t reg)
{
  if (reg < (unw_regnum_t) ARRAY_SIZE (regname))
    return regname[reg];
  else
    return "???";
}
