/*
  Copyright (c) 1995-1997 Nick Ing-Simmons. All rights reserved.
  This program is free software; you can redistribute it and/or
  modify it under the same terms as Perl itself.
*/

#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#include "tkGlue.def"

#include "pTk/tkPort.h"
#include "pTk/tkInt.h"
#include "pTk/tkVMacro.h"
#include "tkGlue.h"
#include "tkGlue.m"

DECLARE_VTABLES;


MODULE = Tk::Canvas	PACKAGE = Tk

void
canvas(...)
CODE:
 {
  XSRETURN(XSTkCommand(cv,Tk_CanvasCmd,items,&ST(0)));
 }


PROTOTYPES: DISABLE

BOOT:
 {
  IMPORT_VTABLES;
 }
