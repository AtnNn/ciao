:- module(exemaker,
        [make_exec/2, make_actmod/2, force_lazy/1, undo_force_lazy/1,
         dynamic_search_path/1],
        [assertions]).

:- use_module(library('compiler/c_itf'),
        [handle_exc/1, process_file/7, process_files_from/7, false/1,
         old_file_extension/2, make_po_file/1, get_so_name/2, base_name/2,
         file_data/3, module_error/1, defines_module/2, cleanup_c_itf_data/0,
         processed/2, exports/5, imports_pred/7, def_multifile/4,
         addmodule_inc/3, decl/2]).
:- use_module(library(system),
        [file_exists/1, fmode/2, chmod/2, mktemp/2, delete_file/1]).
:- use_module(library('compiler/pl2wam')).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(file_utils), [file_terms/2, copy_stdout/1]).
:- use_module(library(ctrlcclean), [delete_on_ctrlc/2]).
:- use_module(engine(internals), [module_concat/3]).
:- use_module(library('foreign_interface/build_foreign_interface')). % JFMC
:- use_module(library('compiler/compressed_bytecode')). % OPA

% Extension for ciao executables in Win32
:- include(win_exec_ext).

:- multifile define_flag/3.

define_flag(executables, [static, eagerload, lazyload], eagerload).
define_flag(check_libraries, [on, off], off).
define_flag(selfcontained,atom,none).
define_flag(compressexec,[yes,no],no).

:- data ok_lazy/1.

force_lazy(Module) :- asserta_fact(ok_lazy(Module)).

undo_force_lazy(Module) :- retractall_fact(ok_lazy(Module)).

make_exec(Files, ExecName) :-
        catch(make_exec_prot(Files, ExecName), Error, handle_exc(Error)).

make_actmod(ModuleFile, PublishMod) :-
        % nonvar() below is always true for files
        process_file(ModuleFile, nop, module, false, nonvar, false, false),
        base_name(ModuleFile, Base),
        file_data(Base, PlName, _),
        get_os(OS),
        resolve_execname(ExecName, Base, PlName, OS),
        create_main(Base, PublishMod, MainFile),
        catch(make_exec_prot([MainFile], ExecName), Error, handle_exc(Error)).

make_exec_prot(Files, ExecName) :-
        process_files_from(Files, po, any,
                           treat_file, stopOnlib, skipOnlib, redo_po),
        \+ current_fact(module_error(_)),
        Files = [MainFile|_],
        base_name(MainFile, Base),
        defines_module(Base, Module),
        compute_main_def(Module, Base, MainDef),
        current_prolog_flag(executables, ExecMode),
        compute_objects_loads(ExecMode, ExecFiles, InitLoads),
        create_loader(ExecMode, MainDef, InitLoads, ExecFiles, ExecFiles1),
        create_exec(ExecName, Base, ExecFiles1),
	create_interfaces, % JFMC
        !,
        delete_temp,
        cleanup_c_itf_data.
make_exec_prot(_, _) :-
        message('{Executable generation aborted}'),
	retractall_fact(needs_interface(_, _)), % JFMC
        cleanup_c_itf_data.

% JFMC
treat_file(Base) :-
        treat_so_lib(Base),
	fail.
treat_file(Base) :-
        make_po_file(Base).


stopOnlib(Base) :-
        current_prolog_flag(check_libraries, off),
        current_prolog_flag(executables,Mode), Mode \== static,
        is_lib_no_engine(Base).

skipOnlib(Base) :-
        current_prolog_flag(check_libraries, off),
        is_lib(Base).

is_lib_no_engine(Base) :-
        ciaolibdir(Dir),
        atom_concat(Dir,Name,Base),
        atom_concat('/lib',Inside,Name), % Directories stating by lib in Ciao
        \+ atom_concat('/engine/',_,Inside). % but not in lib/engine

is_lib(Base) :-
        ciaolibdir(Dir),
        atom_concat(Dir,Name,Base),
        atom_concat('/lib',_,Name). % Directories stating by lib in Ciao

% JFMC
redo_po(Base) :-
        treat_so_lib(Base),
	fail.
redo_po(Base) :-
	old_file_extension(Base, '.po').

:- data has_so_file/1, needs_interface/2. % JFMC
        
treat_so_lib(Base) :-
	( findall(X, decl(Base, X), Decls),
	  do_interface(Decls) ->  % JFMC
	    assertz_fact(needs_interface(Base, Decls)),
	    assertz_fact(has_so_file(Base))
	; get_so_name(Base, SoName),
	  file_exists(SoName),
	  assertz_fact(has_so_file(Base))
	).

compute_main_def(user(_), _, void) :- !.
compute_main_def(Module, Base, clause(UserMain, ModMain)) :-
        exports(Base, main, Arity, _, _),
        (Arity = 0 ; Arity = 1), !,
        functor(Main, main, Arity),
        module_concat(user, Main, UserMain),
        module_concat(Module, Main, ModMain).
compute_main_def(Module, _, _) :-
        message(error, ['module ',Module,
                        ' should export main/0 or main/1']), fail.

%%% --- Creating foreign interfaces - JFMC --- %%%

:- data interface_creation_error/0. 

create_interfaces :- 
        retract_fact(needs_interface(Base, Decls)),
          ( build_foreign_interface_explicit_decls(Base, Decls) -> true
          ; set_fact(interface_creation_error)
          ),
        fail.
create_interfaces :-
        \+ retract_fact(interface_creation_error).

%%% --- Computing load type for each module --- %%%

compute_objects_loads(ExecMode, ExecFiles, InitLoads) :-
        findall(Base, processed(Base, po), Bases),
        compute_load_types(ExecMode, Bases),
        compute_exec_data(Bases, ExecFiles, InitLoads).

:- data load_type/2. % BASE should be loaded as TYPE, one of static,
                     % dynamic, eager or lazy (dynamic is used if eagerload).

compute_load_types(_, _) :- retractall_fact(load_type(_,_)), fail.
compute_load_types(static, _) :-
        assertz_fact(load_type(_Base,static)).
compute_load_types(eagerload, _) :-
        ( dynamic_search_path(_) -> Dyn_type = eager ; Dyn_type = dynamic),
        ( processed(Base, po),
            sta_or_dyn_type(Base, Dyn_type),
          fail
        ; true
        ).
compute_load_types(lazyload, Bases) :-
        retractall_fact(requires_file1(_,_)),
        compute_load_deps(Bases),
        sta_eager_lazy(Bases).

:- data dynamic_search_path/1.

sta_or_dyn_type(Base, Dyn_type) :-
        base_name(File, Base), !,
        ( File = library(_) ->
            Type = Dyn_type
        ; dynamic_search_path(Fun), functor(File, Fun, _) ->
            Type = Dyn_type
        ; Type = static
        ),
        asserta_fact(load_type(Base,Type)).

:- data requires_file1/2. % MODULE1 needs MODULE2 to be loaded
                            % (maybe transitively)


compute_load_deps([]).
compute_load_deps([Base|Bases]) :-
        base_name(File, Base), !,
        ( File = library(_), !
        ; dynamic_search_path(Fun), functor(File, Fun, _), !
        ; asserta_fact(load_type(Base,static))
        ),
        compute_required(Base),
        compute_load_deps(Bases).

compute_required(Base) :-
        requires_file(Base, Base1),
          defines_module(Base1, M1),
          \+ ok_lazy(M1),
            add_requires_file1(Base, Base1),
        fail.
compute_required(_).

requires_file(B, B1) :-
        member(Dyn, [dynamic, data, concurrent]),
        imports_pred(B, IF,_F,_A, Dyn, _, EF),
        ( EF = '.' -> base_name(IF, B1) ; base_name(EF, B1) ).
requires_file(B, B1) :-
        def_multifile(B,  F, A, _),
        def_multifile(B1, F, A, _),
        B \== B1.

add_requires_file1(B0, B1) :- % Keep relation transitive
        ( I = B0 ; requires_file1(I, B0) ),
        ( O = B1 ; requires_file1(B1, O) ),
        I \== O,
        \+ current_fact(requires_file1(I, O)),
        asserta_fact(requires_file1(I, O)),
        fail.
add_requires_file1(_,_).

sta_eager_lazy([]).
sta_eager_lazy([B|Bs]) :-
        ( current_fact(load_type(B,static)) -> true
        ; requires_file1(B0, B), load_type(B0, static) ->
            defines_module(B, M),
            message(note, ['module ',M,' will be loaded eagerly.']),
            asserta_fact(load_type(B, eager))
        ; asserta_fact(load_type(B, lazy))
        ),
        sta_eager_lazy(Bs).


%%% --- Compiling changed files, computing info to make executable --- %%%

compute_exec_data([], [], fail).
compute_exec_data([Base|Bases], ExFs, Lds) :- % both .so and .po - JFMC
        defines_module(Base, Module),
        load_type(Base, LdType),
	atom_concat(Base, '.po', PoName),
        ( has_so_file(Base) ->
          ( LdType = static ->
              base_name(File, Base),
              ExFs = [PoName|ExFs_], Lds = (load_so(Module, File), Lds_)
          ; LdType = dynamic ->
              ExFs = ExFs_, Lds = Lds_
          ; LdType = eager ->
              base_name(File, Base),
              ExFs = ExFs_, Lds = (load_so(Module, File), load_po(File), Lds_)
          ; LdType = lazy ->
              make_lo(Module, Base, LoName),
              ExFs = [LoName|ExFs_], Lds = Lds_
          )
        ; ( LdType = static -> 
	    ExFs = [PoName|ExFs_], Lds = Lds_
	  ; LdType = dynamic ->
	    ExFs = ExFs_, Lds = Lds_
	  ; LdType = eager ->
	    base_name(File, Base),
	    ExFs = ExFs_, Lds = (load_po(File), Lds_)
	  ; LdType = lazy ->
	    make_lo(Module, Base, LoName),
	    ExFs = [LoName|ExFs_], Lds = Lds_
	  )
	), !,
        compute_exec_data(Bases, ExFs_, Lds_).

%%% --- Making lazyload files --- %%%

make_lo(Module, Base, LoFile) :-
        base_name(File, Base), !,
        verbose_message(['{Making lazyloader file for ',Module]),
        compute_required_loads(Base, Loads0, Pred),
        Loads = (load_lib_lazy(Module, File), Loads0),
        temp_filename(LoFile),
        delete_on_ctrlc(LoFile, Ref),
        open(LoFile, write, Out),
        Mode = ql(unprofiled),
        set_compiler_mode(Mode),
        set_compiler_out(Out),
        compile_stumps(Base, Module, Loads, Pred),
        cleanup_compilation_data,
        close(Out),
        erase(Ref),
        verbose_message('}').

compute_required_loads(B0, Loads, Pred) :-
        retract_fact(requires_file1(B0,B)), !,
        defines_module(B, M),
        base_name(File, B), !,
        Loads = (load_lib_lazy(M, File), Loads_),
        compute_required_loads(B0, Loads_, Pred).
compute_required_loads(_B0, Pred, Pred). % Incomplete structure

compile_stumps(Base, Module, Loads, Pred) :-
        define_stump_pred,
        exports(Base, F, A,_Def, Meta),
%% This prevents .so libraries to be lazyloaded
%          Def \== implicit,
          addmodule_inc(Meta, A, A1),
          module_concat(Module, F, MF),
          functor(Pred, MF, A1),
          compile_clause('multifile:stump'(Module, Pred), true),
          compile_clause(Pred, Loads),
        fail.
compile_stumps(_, _, _, _).

define_stump_pred :-
        proc_declaration(multifile, 'multifile:stump'(_,_),
                                    'multifile:stump', 2),
        proc_declaration(dynamic,   'multifile:stump'(_,_),
                                    'multifile:stump', 2).


%%% --- Making executable file --- %%%


create_loader(static, void, fail, PoFiles, PoFiles) :- !. % Nothing to do
create_loader(lazyload, void, fail, PoFiles, PoFiles) :- !. % Nothing to do
create_loader(ExecMode, MainDef, Loads, PoFiles, [TmpPoFile|PoFiles]) :-
        temp_filename(TmpPoFile),
        delete_on_ctrlc(TmpPoFile, Ref),
        open(TmpPoFile, write, Out),
        verbose_message(['{Compiling auxiliary file ',TmpPoFile]),
        Mode = ql(unprofiled),
        set_compiler_mode(Mode),
        set_compiler_out(Out),        
        compile_loads(ExecMode, Loads),
        compile_main_def(MainDef),
        cleanup_compilation_data,
        close(Out),
        erase(Ref),
        verbose_message('}').

compile_loads(static, fail) :- !.
compile_loads(lazyload, fail) :- !.
compile_loads(_, _) :-
        proc_declaration(multifile, 'multifile:load_libs',
                                    'multifile:load_libs', 0),
        fail.
compile_loads(eagerload, _):-
	compile_clause('multifile:load_libs', (ldlibs(_), fail)),
	fail.
compile_loads(_, Loads):-
        Loads \== fail, !,
	compile_clause('multifile:load_libs', Loads).
compile_loads(_, _).

compile_main_def(void).
compile_main_def(clause(UserMain, ModMain)) :-
        compile_clause(UserMain, ModMain).

create_exec(ExecName, Base, PoFiles) :-
        file_data(Base, PlName, _),
        get_os(OS),
        resolve_execname(ExecName, Base, PlName, OS),
        current_input(Si),
        current_output(So),
        ( file_exists(ExecName) -> delete_file(ExecName) ; true ),
        delete_on_ctrlc(ExecName, Ref),
	open(ExecName,write,Stream),
        set_output(Stream),
        current_prolog_flag(selfcontained,TargetEng),
	copy_header(TargetEng),
	copy_pos(PoFiles,Stream),
	close(Stream),
        erase(Ref),
        set_input(Si),
        set_output(So),
        fmode(PlName, M0),
        M1 is M0 \/ ((M0 >> 2) /\ 0o111),
        chmod(ExecName, M1).

copy_header(none) :- !, % OPA
	copy_stdout(library('compiler/header')).
copy_header(TargetEng) :- !,
	verbose_message(['{Using engine for ',TargetEng,'}']),
	ciaolibdir(Libdir),
	atom_concat(Libdir,'/engine/ciaoengine.',Dir),
	atom_concat(Dir,TargetEng,Dir2),
	atom_concat(Dir2,'.sta',Engine),
	copy_stdout(Engine).

copy_pos(PoFiles,_) :- % OPA
	current_prolog_flag(compressexec,no), !,
	dump_pos(PoFiles).
copy_pos(PoFiles,Stream) :-
	temp_filename(TmpFile),
	open(TmpFile,write,TmpStreamw),
	set_output(TmpStreamw),
	dump_pos(PoFiles),
	close(TmpStreamw),
	open(TmpFile,read,TmpStreamr),
	set_output(Stream),
	verbose_message(['{Compressing executable}']),
	compressLZ(TmpStreamr),
	close(TmpStreamr).

resolve_execname(ExecName, _, _, _) :- nonvar(ExecName), !.
resolve_execname(ExecName, B, Pl, Os) :-
% Pl file has no .pl extension or we are compiling for Win32
        ( Pl = B ; Os = 'Win32' ; current_prolog_flag(selfcontained,'Win32i86')), !,
        exec_ext(EXT),
        atom_concat(B,EXT,ExecName).
resolve_execname(ExecName, B, _, _) :- ExecName = B.

%% Now done by dist Makefile
% shell_header('Win32', library('compiler/header_win32')) :- !.
% shell_header(_OS, library('compiler/header')).

dump_pos([File|Files]):-
	verbose_message(['{Adding ',File,'}']),
	open(File,read,Stream),
        copyLZ(Stream),
	close(Stream),
	nl,
	dump_pos(Files).
dump_pos([]).

%%% --- Making main file for active modules --- %%%

create_main(Base, PublishMod, MainFile) :-
        findall(exe(Pred,Pred),
                (exports(Base, F, A, _, _), functor(Pred, F, A)),
                 ExeFacts),
        temp_filename(MainFile),
        atom_concat(MainFile, '.itf', ItfFile),
        assertz_fact(tmp_file(ItfFile)),
        atom_concat(MainFile, '.po', PoFile),
        assertz_fact(tmp_file(PoFile)),
        file_terms(MainFile, [
          :-(use_package([])),
          :-(use_module(Base)),
          :-(use_module(library(PublishMod))),
          :-(use_module(library('actmods/actmod_server'), [actmodmain/0])),
          :-(main, actmodmain),
          :-(meta_predicate(exe(?,fact)))
          |ExeFacts]).


:- data tmp_file/1.

temp_filename(File) :-
        mktemp('tmpciaoXXXXXX', File),
        assertz_fact(tmp_file(File)).

delete_temp :-
        retract_fact(tmp_file(File)),
        delete_file(File),
        fail.
delete_temp.

verbose_message(M) :-
        ( current_prolog_flag(verbose_compilation,off), !
        ; message(M)
        ).

% ------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+51,2000/02/10,18:42*31+'CET'), "Fixed a bug which
   prevented .so libraries to be lazyloaded. (Daniel Cabeza Gras)").

:- comment(version(1*5+47,2000/02/07,20:18*32+'CET'), "Fixed a bug
   introduced in version 1*5+38 which made that executables created with
   check_libraries off did not include engine(internals). (Daniel Cabeza
   Gras)").

:- comment(version(1*5+38,2000/02/01,19:05*58+'CET'), "Added prolog_flag
   check_libraries (off by default) to avoid processing Ciao libraries,
   in order to speedup compilation.  (Daniel Cabeza Gras)").

:- comment(version(1*3+81,1999/10/15,18:48*17+'MEST'), "Fixed a bug when
   defining additional dynamic search paths, which prevented the load of
   library modules used only by files in those search paths.  (Daniel
   Cabeza Gras)").

:- comment(version(1*3+48,1999/09/02,18:02*53+'MEST'), "Fixed a bug in
   active module compilation related to generation of exe/2 facts.
   (Daniel Cabeza Gras)").

:- comment(version(0*9+40,1999/04/07,20:34*14+'MEST'), "Fixed a bug when
   using -d option in ciaoc related to dynamic_search_path/1 predicate.
   (Daniel Cabeza Gras)").

:- comment(version(0*9+38,1999/04/06,20:33*52+'MEST'), "Fixed bug which
   created .exe executables files when not needed.  (Daniel Cabeza
   Gras)").

:- comment(version(0*9+27,1999/03/26,19:12*08+'MET'), "When making an
   executable of a file with no .pl extension, add a .exe extension to
   avoid clobbering source file.  (Daniel Cabeza Gras)").

:- comment(version(0*5+39,1998/07/06,20:01*36+'MET DST'), "Added
   dynamic_search_path/1 to provide a way of defining other path aliases
   (in addition to 'library') which can be loaded dynamically (Daniel
   Cabeza Gras)").

:- comment(version(0*5+13,1998/06/04,21:33*23+'MET DST'), "Now the main
   file for an executable can be a module exporting main/0 or main/1.
   (Daniel Cabeza Gras)").
