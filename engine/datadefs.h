/* Copyright (C) 1996,1997,1998, UPM-CLIP */

#if defined(__STDC__)
#define VOLATILE volatile
#define PROTO(argl) argl
#else
#define VOLATILE
#define PROTO(ignore) ()
#endif

/* # define REGISTER register */

#include "configure.h"
#include "alloc.h"
#include "registers.h"
#include "termdefs.h"
#include "access.h"
#include "locks.h"
#include "objareas.h"
