#if !defined(__CIAO_GLUECODE_H__)
#define __CIAO_GLUECODE_H__

#include "datadefs.h"
#include "support.h"
#if defined(MALLOC_DEBUG)
#include "dmalloc.h"
#endif

#include "debug.h"
#include "initial.h"
#include "threads.h"
#include "task_areas.h"

#include "ciao_prolog.h"

#define DECL_STATE goal_descriptor sstate; ciao_state state;
#define INIT_STATE state = &sstate; state->worker_registers = w;
#define IMPLICIT_STATE ciao_implicit_state = state;

#include <setjmp.h>

/* TODO: decide which exception should be raised when an exception happens in
   the C code */

extern jmp_buf ciao_gluecode_jmpbuf;
#define GLUECODE_TRY(Call) \
    if (setjmp(ciao_gluecode_jmpbuf)) { \
      BUILTIN_ERROR(SYSTEM_ERROR, X(0), 1) \
    } else { \
      Call; \
    }

#endif /* __CIAO_GLUECODE_H__ */

