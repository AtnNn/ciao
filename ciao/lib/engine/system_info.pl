%%---------------------------------------------------------------------

:- module(system_info, [
        get_arch/1,
        get_os/1,
	get_platform/1,
	get_debug/1,
	get_eng_location/1,
	get_ciao_ext/1,
	get_exec_ext/1,
	get_so_ext/1,
        this_module/1,
        current_module/1,
	ciao_c_headers_dir/1,
        ciao_lib_dir/1],
        [assertions, nortchecks, isomodes]).

%%---------------------------------------------------------------------

:- doc(title,"Gathering some basic internal info").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Carro").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module,"This module provides predicates which return basic
   internal info.").

%%---------------------------------------------------------------------

:- impl_defined([
        get_arch/1,
        get_os/1,
	get_debug/1,
	get_eng_location/1,
	get_ciao_ext/1,
	get_exec_ext/1,
	get_so_ext/1,
        this_module/1,
	ciao_c_headers_dir/1,
        ciao_lib_dir/1]).

%%---------------------------------------------------------------------

:- trust pred get_arch(?ArchDescriptor) => atm #
	"Unifies @var{ArchDescriptor} with a simple atom which describes
         the computer architecture currently executing the predicate.".

:- doc(get_arch/1,
	"This predicate will describe the computer architecture wich
         is currently executing the predicate.

         Computer architectures are identified by a simple atom.
         This atom is implementation-defined, and may suffer any change
         from one Ciao Prolog version to another.

         For example,Ciao Prolog running on an Intel-based machine 
         will retrieve:
@begin{verbatim}
?- get_arch(I).

I = i86 ? ;

no
?- 
@end{verbatim}
	").

%%---------------------------------------------------------------------

:- trust pred get_os(?OsDescriptor) => atm #
	"Unifies @var{OsDescriptor} with a simple atom which describes
         the running Operating System when predicate was called.".

:- doc(get_os/1,
	"This predicate will describe the Operating System which 
         is running on the machine currently executing the Prolog program.

         Operating Systems are identified by a simple atom.
         This atom is implementation-defined, and may suffer any change
         from one Ciao Prolog version to another.

         For example,Ciao Prolog running on Linux will retrieve:
@begin{verbatim}
?- get_os(I).

I = 'LINUX' ? ;

no
?- 
@end{verbatim}
	").

:- trust pred get_platform(?Platform) => atm # "Unifies @var{Platform}
	with an atom that describes the Operating System and the
	computer architecture which is currently executing the
	predicate.".

get_platform(Platform) :-
	get_os(Os),
	get_arch(Arch),
	atom_concat(Os, Arch, Platform).

:- trust pred get_debug(?Debug) => atm # "Unifies @var{Debug} with an
	atom that indicates if the emulator has been compiled with
	debug information".

:- trust pred get_ciao_ext(?Ext) => atm # "Unifies @var{Ext} with an
	atom that indicates the default extension for the executable
	prolog programs.".

:- trust pred get_exec_ext(?Ext) => atm # "Unifies @var{Ext} with an
	atom that indicates the default extension for the binary
	executables.".

:- true pred get_so_ext(?Ext) :: atm # "Unifies @var{Ext} with an atom
	that indicates the default extension for the shared libraries.
	For example, .dll in windows and .so in most unix systems.".

:- true pred get_eng_location(?Ext) :: atm # "Unifies @var{Ext} with
	an atom that indicates if the engine is located in a library
	(dyn) or in an executable (empty).".


%%---------------------------------------------------------------------


:- pred current_module(Module) => internal_module_id + native #
	"Retrieves (on backtracking) all currently loaded modules into
         your application.".

:- doc(current_module/1,
	"This predicate will successively unify its argument with all
	 module names currently loaded. Module names will be simple atoms.

         When called using a free variable as argument, it will
         retrieve on backtracking all modules currently loaded. This is 
         usefull when called from the Ciao @apl{toplevel}.

         When called using a module name as argument it will check whether
         the given module is loaded or not. This is usefull when called
         from user programs.
        ").

current_module(Module) :- '$current_module'(Module).

%%---------------------------------------------------------------------

:- trust pred ciao_lib_dir(CiaoPath) => atm(CiaoPath) #
	"@var{CiaoPath} is the path to the root of the Ciao
	libraries. Inside this directory, there are the directories
	'lib', 'library' and 'contrib', which contain library modules.".

:- trust pred ciao_c_headers_dir(CiaoPath) => atm(CiaoPath) #
	"@var{CiaoPath} is the path to the root of the installed Ciao
	header C files (.h), typically used for interfacing Ciao and
	C.".

%%---------------------------------------------------------------------

:- meta_predicate this_module(addmodule).

this_module(M, M).

:- trust pred this_module(Module) => internal_module_id #
	"@var{Module} is the internal module identifier for current module.".

%%---------------------------------------------------------------------

:- doc(doinclude,internal_module_id/1).

:- doc(internal_module_id/1, "For a user file it is a term user/1
	with an argument different for each user file, for
	other modules is just the name of the module (as an atom).").

:- export(internal_module_id/1).
:- prop internal_module_id(M) + regtype #
	"@var{M} is an internal module identifier".

internal_module_id(user(M)) :-
	atm(M).
internal_module_id(M) :- 
	atm(M).
