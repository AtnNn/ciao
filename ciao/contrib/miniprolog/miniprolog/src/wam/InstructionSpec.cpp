/******************************************************************************
 * Warren's Abstract Machine  -  Implementation by Stefan Buettcher
 *
 * developed:   December 2001 until February 2002
 *
 * Instruction.java contains the class Instruction, representing a single line of
 * WAM code, e.g. "true: proceed".
 ******************************************************************************/


#ifdef __BORLANDC__
#pragma hdrstop
#pragma package(smart_init)
#endif

#include "InstructionSpec.h"

using namespace std;
using namespace wam;

# ifndef INLINE
#  include "InstructionSpecImpl.h"
# endif

