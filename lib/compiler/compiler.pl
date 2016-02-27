:- module(compiler, [make_po/1, ensure_loaded/1,
                     use_module/1, use_module/2, use_module/3, unload/1,
                     set_debug_mode/1, set_nodebug_mode/1,
                     set_debug_module/1, set_nodebug_module/1,
		     set_debug_module_source/1,
                     mode_of_module/2, module_of/2
                     ], [assertions]).


:- use_module(library('compiler/c_itf_internal')).


make_po([]) :- !.
make_po([File|Files]) :- !,
        catch(make_po1(File), Error, handle_exc(Error)),
        make_po(Files).
make_po(File) :-
        catch(make_po1(File), Error, handle_exc(Error)).

:- meta_predicate use_module(addmodule).

use_module(Mod,This) :- use_module(Mod,all,This).

:- meta_predicate use_module(+,addmodule).

use_module(File, Imports, ByThisModule) :-
        cleanup_c_itf_data,
        use_mod(File, Imports, ByThisModule),
        check_static_module(File).

ensure_loaded(File) :-
        cleanup_c_itf_data,
	use_mod_user(File),
        check_static_module(File).

check_static_module(File) :-
        base_name(File, Base),
        defines_module(Base, Module),
        static_module(Module), !,
        message(note, ['module ',Module,
                       ' already in executable, just made visible']).
check_static_module(_).

unload(File) :-
	unload_mod(File).

set_debug_mode(File) :-
        absolute_file_name(File, Source),
        (interpret_file(Source), ! ; assertz_fact(interpret_file(Source))).

set_nodebug_mode(File) :-
        absolute_file_name(File, Source),
        retractall_fact(interpret_file(Source)).

set_debug_module(Mod) :- 
        module_pattern(Mod, MPat),
	retractall_fact(interpret_srcdbg(MPat)),
        (   current_fact(interpret_module(MPat)), ! 
	; 
	    assertz_fact(interpret_module(MPat))
	).

set_nodebug_module(Mod) :-
        module_pattern(Mod, MPat),
        retract_fact(interpret_module(MPat)),
	retractall_fact(interpret_srcdbg(MPat)).

set_debug_module_source(Mod) :- 
        module_pattern(Mod, MPat),
	assertz_fact(interpret_module(MPat)),
        (   
	    current_fact(interpret_srcdbg(MPat)), ! 
	; 
	    assertz_fact(interpret_srcdbg(MPat))
	).

module_pattern(user, user(_)) :- !.
module_pattern(Module, Module) :- atom(Module).

module_of(H, M) :- pred_module(H, M).

mode_of_module(Module, Mode) :- module_loaded(Module, _, _, Mode).

% ----------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*7+176,2002/01/14,17:27*00+'CET'), "changed 'module
   already in executable' message.  (Daniel Cabeza Gras)").

:- comment(version(1*7+85,2001/04/05,11:16*53+'CEST'), "Fixed the
   foreign interface bug seen with ensure_loaded/1 (Jose Morales)").

:- comment(version(0*6+6,1998/07/27,15:40*12+'MET DST'), "Changed
   set_debug_module/1 to make it work with 'user' module.  (Daniel
   Cabeza Gras)").

:- comment(version(0*6+4,1998/07/21,16:21*26+'MET DST'), "Added
   mode_of_module/2 to be used by the shell (Daniel Cabeza Gras)").

:- comment(version(0*6+2,1998/07/20,18:44*34+'MET DST'), "Added
   set_debug_module/1 and set_nodebug_module/1 to be used by the shell
   (Daniel Cabeza Gras)").

:- comment(version(0*5+33,1998/06/30,19:00*22+'MET DST'), "Disabled head
   meta-expansion in the case of addmodule (predicate arity changes)
   (Daniel Cabeza Gras)").

:- comment(version(0*5+31,1998/06/30,14:37*08+'MET DST'), "Fixed bug
   which deleted imports/4 data when reloading files (Daniel Cabeza
   Gras)").

:- comment(version(0*5+18,1998/06/19,13:41*22+'MET DST'), "Added
   predicate multifile/1 to be used in the CIAO shell to define
   multifile predicates to be able to invoke them.  (Daniel Cabeza
   Gras)").

:- comment(version(0*5+6,1998/04/17,21:13*03+'MET DST'), "Added support
   for lazy loading (Daniel Cabeza Gras)").

% ----------------------------------------------------------------------------
