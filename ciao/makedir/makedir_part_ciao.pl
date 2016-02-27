:- module(_, _, [ciaopaths, dcg, make, fsyntax, hiord, assertions, regtypes, isomodes]).

:- doc(title,  "Ciao Global Compilation/Installation installer").
:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").

:- doc(module, "This file is part of the CiaoDE installation system").

% TODO: refine dependencies, move to makedir_SHARED
:- use_module(library(terms),        [atom_concat/2]).
:- use_module(library(lists),        [append/3]).
:- use_module(library(llists),       [flatten/2, append/2]).
:- use_module(library(aggregates)).
:- use_module(library(write)).
:- use_module(library(streams)).
:- use_module(library(messages)).
:- use_module(library(file_utils)).
:- use_module(library(strings)).

% TODO: when is this generated? load like the bundle list?
:- include(ciaosrc(makedir(platdep_modules))).

% ===========================================================================
% TODO: generalize for any node in the bundletree

install_docs <- :- install_item(docs(ciao)).

% ============================================================================

:- doc(section, "Invoking the Configuration").

configure <- [] # "Invoke configuration" :-
 	restore_ciao_config, % TODO: Necessary? ciao_config_db contains an initialization directive
	configure,
	( \+ silentconfig ->
	    show_what_is_next
	; true
	).

show_what_is_next :-
	normal_message(
"Please check that all the values and messages above are correct. If
not, you can change or customize the configuration using the command
line or --menu configure option.

To continue, execute:
   \"./ciaosetup build\"        to compile, then
   \"./ciaosetup docs\"         to generate the documentation,
                              (if not in distribution), then
   \"./ciaosetup install\"      to install the system.", []),
	!.

% ============================================================================
% Hooks for operations on @regtype{bundletree/1}
%
% OP_hook_delc(X): target @var{X} accepts operation @tt{OP}
% OP_hook(X,...): implementation of operation @tt{OP} for target @var{X}
%
% where OP is one of:
%
%   - 'def': definition as a named node in a bundletree (incompatible
%            with the rest of OP)
%   - 'prebuild': preparation before build
%   - 'build': build
%   - 'install': installation
%   - 'uninstall': uninstallation

:- discontiguous def_hook_decl/1.
:- discontiguous def_hook/2. % def_hook(Node, Def)

:- discontiguous prebuild_hook_decl/1.
:- discontiguous prebuild_hook/1.

:- discontiguous build_hook_decl/1.
:- discontiguous build_hook/1.
%build_hook_decl(_) :- fail.
%build_hook(_) :- fail.

:- discontiguous install_hook_decl/1.
:- discontiguous install_hook/1.

:- discontiguous uninstall_hook_decl/1.
:- discontiguous uninstall_hook/1.

% ============================================================================
% TODO: move to makedir_aux.pl
% TODO: rename makedir_aux.pl by other name
% TODO: include 'groups'
% TODO: add dependencies

:- regtype bundletree(X) # "@var{X} is the symbolic description of the
   contents of a bundle".
% bundletree is the input of the generic install/uninstall operations
bundletree(_).

is_list([]).
is_list([_|_]). % wrong... but faster

:- pred prebuild_item(+X) :: bundletree # "Build @var{X}".
prebuild_item(X) :- is_list(X), !, prebuild_list(X).
prebuild_item(X) :- def_hook_decl(X), !,
	def_hook(X, Y), prebuild_item(Y).
prebuild_item(X) :- prebuild_hook_decl(X), !, prebuild_hook(X).
prebuild_item(group(Name, X)) :-
	bold_message("Preparing build of ~s", [Name]),
	prebuild_item(X).

prebuild_list([]).
prebuild_list([X|Xs]) :- prebuild_item(X), prebuild_list(Xs).

:- pred build_item(+X) :: bundletree # "Build @var{X}".
build_item(X) :- is_list(X), !, build_list(X).
build_item(X) :- def_hook_decl(X), !,
	def_hook(X, Y), build_item(Y).
build_item(X) :- build_hook_decl(X), !, build_hook(X).
build_item(group(Name, X)) :-
	bold_message("Building ~s", [Name]),
	build_item(X).
build_item(standalone_list(Bundle, Path, List)) :-
	build_standalone_list(Bundle, Path, List).

build_list([]).
build_list([X|Xs]) :- build_item(X), build_list(Xs).

:- pred install_item(+X) :: bundletree # "Install @var{X}".
install_item(X) :- is_list(X), !, install_list(X).
install_item(X) :- def_hook_decl(X), !,
	def_hook(X, Y), install_item(Y).
install_item(X) :- install_hook_decl(X), !, install_hook(X).
install_item(only_global_ins(X)) :-
	% install X only if installation kind is global
	( ~instype = global ->
	    install_item(X)
	; true
	).
install_item(small_group(Name, X)) :-
	normal_message("Installing ~s", [Name]),
	install_item(X).
install_item(group(Name, X)) :-
	bold_message("Installing ~s", [Name]),
	install_item(X).
install_item(big_group(Name, X)) :-
	% like group, but show when the operation has finished
	bold_message("Installing ~s", [Name]),
	install_item(X),
	bold_message("~s installation completed", [Name]).
install_item(dir(Path)) :- install_item(dir(Path, [])).
install_item(dir(Path, Props)) :-
	b_install_dir(~fsR(Path)), % perms?
	( member(files_from(SrcDir), Props) ->
	    % copy contents from Dir
	    b_install_dir_rec(SrcDir, ~fsR(Path))
	; member(files_from(SrcDir, Pattern), Props) ->
	    % copy contents from Dir (as specified by Pattern)
	    ( % (failure-driven loop)
		member(File, ~ls(~fsR(SrcDir), Pattern)),
		b_install_file_noexec(~fsR(SrcDir/(File)),
	                              ~fsR(Path/(File))),
	        fail
	    ; true
	    )
	; true
	).
install_item(lib(L)) :- install_lib(L).
install_item(src(L)) :- install_src(L).
install_item(docs(Bundle)) :- % (for instype=global and instype=local)
	bold_message("Installing documentation for '~w'", [Bundle]),
	bundle_install_docs(Bundle).
install_item(standalone_list(Bundle, _, List)) :-
	install_standalone_list(Bundle, List).
install_item(lib_file_list(Path, List)) :-
	install_lib_file_list(List, Path).
install_item(bin_copy_and_link(K, Bundle, File, Props)) :-
	b_install_copy_and_link(K, Bundle, File),
	( member(link_as(Link), Props) ->
	    b_install_link_as(K, Bundle, File, Link)
	; true
	).

install_list([]).
install_list([X|Xs]) :- install_item(X), install_list(Xs).

install_lib_file_list([], _Path).
install_lib_file_list([X|Xs], Path) :-
	install_lib_file_item(X, Path),
	install_lib_file_list(Xs, Path).

install_lib_file_item(File-Props, Path) :- !,
	( member(copy_and_link, Props) ->
	    rl_install_copy_and_link(Path, File)
	; true
	),
	( member(link_as(Link), Props) ->
	    rl_install_link_as(File, Link)
	; true
	),
	( member(mid(Mid), Props) -> % TODO: why?
	    % TODO: incompatible with any other prop
	    r_install_as(Path/File, Mid),
	    r_abslink_as(Mid, ~lib_dir/(File))
	; true
	).

:- pred uninstall_item(+X) :: bundletree # "Uninstall @var{X}".
uninstall_item(X) :- is_list(X), !, uninstall_list(X).
uninstall_item(X) :- def_hook_decl(X), !,
	def_hook(X, Y), uninstall_item(Y).
uninstall_item(X) :- uninstall_hook_decl(X), !, uninstall_hook(X).
uninstall_item(only_global_ins(X)) :-
	% install X only if installation kind is global
	( ~instype = global ->
	    uninstall_item(X)
	; true
	).
uninstall_item(small_group(Name, X)) :-
	normal_message("Uninstalling ~s", [Name]),
	uninstall_item(X).
uninstall_item(group(Name, X)) :-
	bold_message("Uninstalling ~s", [Name]),
	uninstall_item(X).
uninstall_item(big_group(Name, X)) :-
	% like group, but show when the operation has finished
	bold_message("Uninstalling ~s", [Name]),
	uninstall_item(X),
	bold_message("~s uninstallation completed", [Name]).
uninstall_item(dir(Path)) :-
	uninstall_item(dir(Path, [])).
uninstall_item(dir(Path, Props)) :-
	( member(del_rec, Props) ->
	    % on uninstall, remove contents recursively
	    % TODO: remove also the directory?
	    b_uninstall_dir_rec(~fsR(Path))
	; member(del_if_empty, Props) ->
	    % delete if the directory is empty
	    % TODO: only if it is empty? what dirs should have this prop?
	    b_uninstall_dir_if_empty(~lib_dir)
	; member(do_not_del, Props) ->
	    % do not delete on uninstall
	    true
	; b_uninstall_dir(~fsR(Path))
	).
uninstall_item(lib(L)) :- uninstall_lib(L).
uninstall_item(src(L)) :- uninstall_src(L).
uninstall_item(docs(Bundle)) :- % (for instype=global and instype=local)
	bold_message("Uninstalling documentation for '~w'", [Bundle]),
	bundle_uninstall_docs(Bundle).
uninstall_item(standalone_list(Bundle, _, List)) :-
	uninstall_standalone_list(Bundle, List).
uninstall_item(lib_file_list(Path, List)) :-
	uninstall_lib_file_list(List, Path).
uninstall_item(file(Path)) :-
	b_uninstall_file(Path).
uninstall_item(bin_copy_and_link(K, Bundle, File, Props)) :-
	( member(link_as(Link), Props) ->
	    b_uninstall_link(K, Link)
	; true
	),
	b_uninstall_copy_and_link(K, Bundle, File).

% TODO: we would need dependencies here
uninstall_list(Xs) :-
	% uninstall is done in reverse order
	uninstall_list_(~reverse(Xs)).

uninstall_list_([]).
uninstall_list_([X|Xs]) :- uninstall_item(X), uninstall_list_(Xs).

uninstall_lib_file_list([], _Path).
uninstall_lib_file_list([X|Xs], Path) :-
	uninstall_lib_file_item(X, Path),
	uninstall_lib_file_list(Xs, Path).

uninstall_lib_file_item(File-Props, _Path) :- !,
	( member(link_as(Link), Props) ->
	    rl_uninstall_link(Link)
	; true
	),
	( member(copy_and_link, Props) ->
	    rl_uninstall_copy_and_link(File)
	; true
	),
	( member(mid(Mid), Props) -> % TODO: why?
	    % TODO: incompatible with any other prop
	    b_uninstall_file(Mid),
	    rl_uninstall_copy_and_link(File)
	; true
	).

:- use_module(library(lists), [reverse/2]).

% ============================================================================

:- include(library(lpdist('makedir_SHARE'))).
bundle_id(ciao).
bundle_dname('Ciao').
%
bundle_readme_dir := ~fsR(bundle_src(ciao)/'doc'/'common').
bundle_manual_dir := ~fsR(bundle_src(ciao)/'doc'/'reference').
%
bundle_readme('INSTALLATION_CIAO').
bundle_readme('INSTALLATION_CIAO_Win32').
bundle_readme('README_CIAO').
bundle_readme('NewUser'-[libroot = ~ciaolib_root, lpdocdir = ~docdir]).
%
bundle_ins_reg :- fail. % TODO: document (register the bundle on ins?)

% ============================================================================

:- doc(section, "Build").
% (engine, libraries, and compiler)

% (hook)
bundle_build <- [] :-
	%% # do_boot_scan_bundles # TODO: why repeat that?
	%% do_build bootstrap_lpmake WHY??? WE ARE ALREADY RUNNING THIS! skip
	build_item(ciaoc),
	build_item(shell),
	build_item(optim_comp),
	%
	build_etc,
	%
	build_libraries,
	%
	build_item(emacs_mode),
	build_item(header_for_win32).

build_hook_decl(optim_comp).
build_hook(optim_comp) :-
	( optimizing_compiler(yes) ->
	    bold_message("Building 'optim_comp'", []),
	    do(['cd ', ~fsR(bundle_src(ciaode)/'optim_comp'), '; ',
	    './ciaotool build'], [])
	; bold_message("Not bundling 'optim_comp'", [])
	).

% (invoked from 'makedir', for each bundle)
build_libraries <- [] :- build_libraries.
build_libraries :-
        prebuild_libraries,
	build_library.

% (invoked from 'makedir', for each bundle)
build_nolibs <- [] :-
	prebuild_libraries,
	build_platdep.

% Libraries that require customized installation before they are built
% (such as automatically generated code, foreign interfaces, etc.)
prebuild_libraries :-
	prebuild_item([pillow, mathlibs, ppl, persdb_mysql, timingmodel]).

% (invoked from 'makedir', for each bundle)
build_platdep <- [] :- build_platdep.
build_platdep :-
	build_etc,
	build_tabling, % TODO: in build_library too, why?
	build_item(emacs_mode),
	build_item(header_for_win32),
	build_platdep_libs.

build_platdep_libs :-
	compile_platdep_modules(~compiling_options(ciao)),
	% BUG: This is fail prone, we update the .so list to ensure the
	% next time the libraries containing externals are compiled,
	% if the list has changed -- EMM.
	update_so_list.

% ============================================================================

% (used only from 'ciaosetup')
build_ciaoc <- [] :- build_ciaoc.
build_ciaoc :- build_item(ciaoc).

def_hook_decl(ciaoc).
def_hook(ciaoc) :=
        standalone_list(ciao, bundle_src(ciao)/ciaoc, [
          'ciaoc'-[
            name="Standalone compiler",
            plexe,
	    bootstrap_ciaoc, static
          ]
        ]).
	
% (used only from 'ciaosetup')
build_shell <- [] :- build_shell.
build_shell :- build_item(shell).

def_hook_decl(shell).
def_hook(shell) :=
	standalone_list(ciao, bundle_src(ciao)/shell, [
          'ciaosh'-[plexe, name="Interactive toplevel", ciaoc],
          'ciao-shell'-[plexe, name="ciao-script runtime", ciaoc, static]
        ]).

% ===========================================================================

:- doc(section, "Windows-specific").

% TODO: review, this should not be necessary
% 'environment' is required by runwin32.bat 
environment <- [] :-
	build_item(emacs_mode),
	install_item(emacs_mode), % (put ciao-mode-init.el in place)
	build_item(header_for_win32).

:- doc(subsection, "Wrappers for Executables").

% Note: Invoked from makedir/runwin32.bat
windows_bats <- [] :- windows_bats.
windows_bats :-
	bold_message("Creating .bat files for the top-level and compiler", []),
	( % (failure-driven loop)
	  win_cmd_and_opts(BatCmd, EOpts, Opts, OrigCmd),
	    bat_file(BatCmd, OrigCmd, EOpts, Opts, BatFile, Tail),
	    %
	    open_output(BatFile, Out),
	    display('@"'),
	    % TODO: Automatically transform to Win notation
	    %       this should be ~local_engine, using win32 notation
	    %       (try to use winpath/2 on the full path)
	    display(~winpath(~fsR(bundle_src(ciaode)))), % TODO: is this path correct?
	    display('\\build\\objs\\'), % TODO: hardwired here
	    display(~get_platform), % TODO: should this be get_platformdeb?
	    display('\\ciaoengine.exe'),
	    display('"'),
	    ( getenvstr('OS', "Windows_NT") ->
	        AllArgs = ' %*'
	    ; AllArgs = ' %1 %2 %3 %4 %5 %6 %7 %8 %9'
	    ),
	    display(AllArgs),
	    display(Tail),
	    nl,
	    close_output(Out),
	    fail
	; true
	).

% TODO: I do not know why '-i' is not necessary in 'ciaoc'
% TODO: why ciao_extra_commands?
win_cmd_and_opts(ciaosh, '', '-i', ciaosh).
win_cmd_and_opts(ciaoc, '', '', ciaoc).
win_cmd_and_opts(ciao, ~codes_atom(~ciao_extra_commands), '-i', ciaosh).

bat_file(BatCmd, OrigCmd, EOpts, Opts, BatFile, Tail) :-
	BatFile = ~fsR(concat_verk(ciao, ext('.bat'), ~buildbin_dir/(BatCmd))),
	OrigFile = ~fsR(concat_ver(ciao, ~buildbin_dir/(OrigCmd))),
	( EOpts = '' -> EOptsS = '' ; EOptsS = ' ' ),
	( Opts = '' -> OptsS = '' ; OptsS = ' ' ),
	Tail = ~atom_concat([' ', EOpts, EOptsS,
	                     '-C ', Opts, OptsS,
			     '-b \"', OrigFile, '\"']).

:- doc(subsection, "Special header for Windows").
% TODO: merge with do_exe_header code?

build_hook_decl(header_for_win32).
build_hook(header_for_win32) :-
	( get_os('Win32') ->
	    make_header_win32
	; true
	).

make_header_win32 :-
	% TODO: Why not the installed engine?
	Eng = ~fsR(concat_k(exec, ~build_dir/'objs'/(~get_platformdeb)/'ciaoengine')),
	bold_message("Creating executable header for engine ~w", [Eng]),
	HeaderPath = ~fsR(bundle_src(ciaode)/'ciao'/'lib'/'compiler'/'header'),
	%
	open_output(HeaderPath, Out),
	display('#!/bin/sh\n'),
	display_list(['INSTENGINE=\"', Eng, '\"\n']),
	display('ENGINE=${CIAOENGINE:-${INSTENGINE}}\n'),
	display('exec "$ENGINE" "$@" -C -b $0\n\^L\n'),
	close_output(Out).

% ===========================================================================

:- doc(section, "Compilation of Libraries").

% TODO: Kludge: defaulttype is always dyn, see ENGINE_LINK_TYPE variable in
%       config-sysdep.sh

% TODO: document why it is collected (and when)
update_so_list :-
	output_to_file(dump_platdep_modules, 'makedir/platdep_modules.pl').

modulesdir(lib).
modulesdir(library).
modulesdir(contrib).

:- use_module(library(sort)).

dump_platdep_modules :-
	% Show the platform dependent modules
	% TODO: kludge?
	% TODO: why portray?
	findall(platdep_module(Dir, File, FileName),
	    (
		modulesdir(LibDir),
		current_platdep_module(LibDir, Dir, File, FileName)
	    ),
	    PML0),
	sort(PML0, PML),
	list(PML, portray_clause).

current_platdep_module(BaseDir, Dir, File, FileName) :-
	% TODO: coupled with the foreign interface...
	current_dir_module(BaseDir, Dir, File, FileName),
	atom_concat(FileBase, '.pl', FileName),
	FileSO = ~fsR(concat_k(ext(~get_so_ext), ~atom_concat([FileBase, '_', ~get_platform]))), % TODO: full path?
	file_exists(FileSO).

:- data platdep_module/3.
:- meta_predicate compile_platdep_modules(list(pred(1))).
compile_platdep_modules(Options) :-
	compile_modules('', do_platdep_module, Options).

do_platdep_module(Dir, File, FileName) :-
	platdep_module(Dir, File, FileName),
	file_exists(FileName).

:- meta_predicate proc_if_dir_exists(?, goal).
proc_if_dir_exists(Dir, Goal) :-
	NCDir = ~fsR(Dir/'NOCOMPILE'), % TODO: full path?
	( file_exists(Dir), \+ file_exists(NCDir) -> call(Goal)
	; true
	).

build_mods(BaseDir) :-
	set_configured_flags,
	build_mods(BaseDir, ~compiling_options(ciao)).

build_contrib :-
	proc_if_dir_exists(~fsR(bundle_src(ciao)/'contrib'/'chat80'), makechat80),
	build_mods('contrib').

makechat80 :-
	% TODO: Generalize as a mechanism to specify compilation options for subbundles
	lpmake_subdir('contrib/chat80', '../Makefile', ''). % TODO: Use bundle_invoke_lpmake

build_library :-
	build_mods('lib'),
	build_tabling, % TODO: in build_platdep too, why?
	%
	build_mods('library/clpq'),
	build_mods('library/clpr'),
%	build_chr,
	build_javalibs,
	%
	build_mods('library'),
	%
	Dir = 'library/toplevel/',
	FileName = 'library/toplevel/toplevel__scope.pl',
	compile_module_list([m(_, _, FileName)], Dir, ~compiling_options(ciao)),
	%
	build_contrib.

% (called from 'ciaosetup' (for ciaobot))
build_profiler <- [] :- build_profiler.
build_profiler :- build_mods('contrib/profiler').

build_chr <- :- build_chr.
build_chr :-
	( with_chr(yes) ->
	    bold_message("Compiling CHR Libraries"),
	    invoke_customsh('library/chr', 'do_bootstrap')
%	do(['cd library/chr ; ', ~setlocalciao, ' CIAOSH=\"', ~ciaosh, ' -f \" ./do_bootstrap >> ', ~build_log], ~command_option).
	; true
        ).

build_javalibs :-
	( with_java_interface(yes) ->
	    invoke_gmake('library/javall', all)
	; true
	).

build_tabling :-
	( % We can not compile this if tabled execution is disabled
	  tabled_execution(no) ->
	    true
	; build_mods('contrib/tabling')
	).

% ---------------------------------------------------------------------------

:- doc(section, "timingmodel bundle").

timingmodel_dir := bundle_src(ciao)/contrib/timingmodel.

prebuild_hook_decl(timingmodel).
prebuild_hook(timingmodel) :-
	proc_if_dir_exists(~fsR(~timingmodel_dir), do_timingmodel),
 	% TODO: is this call necessary? (do_timingmodel does it)
	copy_mp_auto.

% (called from 'ciaosetup')
build_timingmodel <- :- do_timingmodel.

timingmodel_cmd := bench|estimate.

do_timingmodel :-
	% TODO: redundant check?
	( file_exists(~fsR(~timingmodel_dir)) -> do_timingmodel_ ; true ).

do_timingmodel_ :-
	bold_message("Compiling mini prolog engine"),
	invoke_gmake_miniprolog(all),
	copy_file(~fsR(~timingmodel_dir/'miniprolog'/'bin'/'timingmodel_auto.pl'),
	          ~fsR(~timingmodel_dir/'timingmodel_pre.pl'),
		  [overwrite]),
	%
	bold_message("Generating timing model for mini prolog"),
	( % (failure-driven loop)
	  timingmodel_cmd(Cmd),
	    invoke_gmake_timingmodel(Cmd),
	    fail
	; true
	).

invoke_gmake_miniprolog(Cmd) :-
	invoke_gmake(~fsR(~timingmodel_dir/'miniprolog'),
	             ~atom_concat(['-s -j1 ',
		                   'MPARCH=', ~get_platform, ' ',
				   Cmd])).

invoke_gmake_timingmodel(Cmd) :-
	invoke_gmake(~fsR(~timingmodel_dir),
	             ~atom_concat(['-s -j1 ',
		                   'MPARCH=', ~get_platform, ' ',
				   'CIAOC=', ~ciaoc, ' ',
		                   'CIAODESRC=', ~fsR(bundle_src(ciaode)), ' ',
				   Cmd])).

% This extra step is to ensure the generation of timingmodel_auto.pl
% even if miniprolog has not been configured
copy_mp_auto :-
	Orig = ~fsR(~timingmodel_dir/'timingmodel_pre.pl'),
	( file_exists(Orig) ->
	    copy_file(Orig,
	              ~fsR(~timingmodel_dir/'timingmodel_auto.pl'),
		      [overwrite])
	; true
	).

% TODO: Document entry point?
benchmp <- :- benchmp.

benchmp :-
	bold_message("Running timing model benchmarks"),
	invoke_gmake_timingmodel('bench').

% TODO: Document entry point?
estimatemp <- :-
	estimatemp.

estimatemp :-
	bold_message("Running timing model estimate"),
	invoke_gmake_timingmodel('estimate').

% ---------------------------------------------------------------------------

:- doc(section, "MySQL bundle").

prebuild_hook_decl(persdb_mysql).
prebuild_hook(persdb_mysql) :-
	( with_mysql(yes) ->
	    bold_message("Configuring MySQL Libraries"),
	    wr_template(origin,
	        bundle_src(ciao)/'library'/(~mysql_directory),
	        'linker_opts_auto.pl',
	        ['where_mysql_client_lives' = ~mysql_client_directory]),
	    % TODO: why?
	    ( file_exists(~fsR(bundle_src(ciao)/library/(~mysql_directory_op))) ->
	        wr_template(origin,
		    bundle_src(ciao)/'library'/(~mysql_directory_op),
		    'linker_opts_auto.pl',
		    ['where_mysql_client_lives' = ~mysql_client_directory])
	    ; true
	    )
	; true
	).

mysql_directory := 'persdb_mysql'.
mysql_directory_op := 'persdb_mysql_op'.

% ---------------------------------------------------------------------------

:- doc(section, "PiLLoW bundle").

pillow_dir := bundle_src(ciao)/'library'/'pillow'.

prebuild_hook_decl(pillow).
prebuild_hook(pillow) :-
	icon_address_auto.

icon_address_auto :-
	try_finally(
	    open(~fsR(~pillow_dir/'icon_address_auto.pl'), write, OS),
	    portray_clause(OS, icon_base_address(~webimagesurl)),
	    close(OS)
	).

def_hook_decl(pillow_images).
def_hook(pillow_images) :=
  group("PiLLoW images", 
    dir(~webimagespath, [do_not_del, files_from(~pillow_dir/images)])).

:- doc(bug, "The current installation method do not uninstall the
   installed PiLLoW images. Fix.").

% ---------------------------------------------------------------------------

:- doc(section, "GSL bundle").
% TODO: Generalize for other libraries

gsl_dir := bundle_src(ciao)/'contrib'/'gsl_imports'.

% TODO: rename by 'gsl' (no mathlibs)?
prebuild_hook_decl(mathlibs).
prebuild_hook(mathlibs) :-
	( with_gsl(yes) ->
	    bold_message("Configuring GSL Library"),
	    S = ":- use_module(library(gsl_imports)).\n",
 	    foreign_config_var(gsl, 'cflags', CompilerOpts),
 	    foreign_config_var(gsl, 'libs', LinkerOpts0),
	    fix_linker_opts(LinkerOpts0, LinkerOpts),
	    T = ~flatten([
		    ":- extra_compiler_opts(\'"||CompilerOpts, "\').\n"||
		    ":- extra_linker_opts(\'"||LinkerOpts, "\').\n"]),
	    string_to_file(T, ~fsR(~gsl_dir/'gsl_imports_decl_auto.pl')),
	    % List of static libraries from GSL
	    % TODO: generalize for any other library
	    M = ~flatten(["GSL_STAT_LIBS=\'"||LinkerOpts, "\'\n"])
	;
	    LinkerOpts = "",
	    bold_message("Ignoring GSL Library"),
	    S = ":- use_module(library(gsl_imports(gsl_imports_dummy))).\n",
	    M = ""
	),
	% TODO: Simplify
	% TODO: Options of CONFIG_GSL are included in
        %       the final ~ciao_build_dir/'CONFIG_mkf' file
        % TODO: Those options are not immediately used, correct?
	string_to_file(M, ~fsR(sys_dir(build)/'CONFIG_GSL')),
	string_to_file(S, ~fsR(~gsl_dir/'gsl_imports_auto.pl')).

% Remove the -L option, hack that allows to run in LINUXi86_64 --EMM:
fix_linker_opts(LinkerOpts0, LinkerOpts) :-
	get_platform('LINUXi86_64'),
	append("-L"||_, " "||LinkerOpts, LinkerOpts0),
	!.
fix_linker_opts(LinkerOpts, LinkerOpts).

% ---------------------------------------------------------------------------

:- doc(section, "PPL (Parma Polyhedra Library) bundle").
% TODO: There is even more code in the makedir_part_ciaopp.pl files!

ppl_dir := bundle_src(ciao)/'contrib'/'ppl'.

prebuild_hook_decl(ppl).
prebuild_hook(ppl) :-
	( with_ppl(yes) ->
	    bold_message("Configuring PPL Interface"),
 	    foreign_config_var(ppl, 'cflags', CompilerOpts1),
 	    foreign_config_var(ppl, 'cppflags', CompilerOpts2),
 	    foreign_config_var(ppl, 'ldflags', LinkerOpts),
 	    append(CompilerOpts1, " ",  Tmp),
	    append(Tmp, CompilerOpts2, CompilerOpts),
	    T = ~flatten(["%Do not edit generated automatically\n\n",
		    ":- extra_compiler_opts('", CompilerOpts, "').\n",
		    ":- extra_linker_opts('", LinkerOpts, "').\n"]),
	    % TODO: generalize, share with GSL
	    string_to_file(T, ~fsR(~ppl_dir/'ppl_decl_auto.pl')),
	    Version = ~ppl_version,
	    ( Version @< [0, 9] ->
		fail
	    ; Version @< [0, 10] ->
		ppl_interface_version("0_9"),
		string_to_file("", ~fsR(~ppl_dir/'0_10'/'NOCOMPILE'))
	    ; ppl_interface_version("0_10"),
	      string_to_file("", ~fsR(~ppl_dir/'0_9'/'NOCOMPILE'))
	    )
	;
	    string_to_file(
		":- module(ppl_auto, []).\n"||
		":- initialization(error('PPL library not installed')).",
		~fsR(~ppl_dir/'ppl_auto.pl')),
	    string_to_file("", ~fsR(~ppl_dir/'0_9'/'NOCOMPILE')),
	    string_to_file("", ~fsR(~ppl_dir/'0_10'/'NOCOMPILE'))
	).

% This selects one of the two versions
% TODO: maybe we can use the package 'condcomp' to make this simpler?
ppl_interface_version(StrVer) :-
	atom_codes(AtmVer, StrVer),
	S = ~flatten([
           "%Do not edit generated automatically.\n\n"||
           ":- module('ppl_auto', _).\n"||
           ":- reexport(library('ppl/", StrVer, "/ppl_ciao')).\n"]),
 	string_to_file(S, ~fsR(~ppl_dir/'ppl_auto.pl')),
 	del_file_nofail(~fsR(~ppl_dir/(AtmVer)/'NOCOMPILE')).

% ============================================================================

:- doc(section, "Documentation").

docs <- [] # "Creates documentation files" :-
	docs_emacs, % (generates a .lpdoc with the emacs documentation)
	docs_readmes,
	docs_manuals.

% ============================================================================

:- doc(section, "Register in the System").
% TODO: Should this be just a subtask in the installation? Users
%       should not invoke it...
% TODO: This should be done for each anchor (bash, csh, emacs, etc.)
%       and each bundle

% Modifies the .bashrc/.cshrc/.emacs files to let Ciao run from the
% installed lib files.
bundle_register_hook :-
        register_bashrc,
	register_cshrc,
	register_emacs_mode.

% Leaves the .bashrc/.cshrc/.emacs file in its original state.
bundle_unregister_hook :-
        unregister_bashrc,
	unregister_cshrc,
	unregister_emacs_mode.

% ============================================================================

:- doc(section, "Installation").

bundle_install <- [] :-
	install_item(~ciao_desc).

bundle_uninstall <- :-
	uninstall_item(~ciao_desc).

ciao_desc := 
  big_group("Ciao", [
    only_global_ins([
      dir(bundle_insbaselib(ciao), [del_if_empty]),
      dir(bundle_inslib(ciao), [del_rec]), % why not del_if_empty?
      % TODO: use bundle_registry_base module
      dir(bundle_inslib(ciao)/lib/bundle_registry/bundles, [do_not_del]),
      %
      engine,
      ciaoc,
      shell,
      etc,
      emacs_mode,
      %
      lib('lib'),
      lib('library'),
      pillow_images,
      lib('contrib'),
      src('examples')
    ]),
    docs(ciao)
  ]).

% ===========================================================================
% Engine installation/uninstallation

name_ciaoengine := 'ciaoengine'.
name_ciaoengine_arch := ~atom_concat(['ciaoengine', '.', ~get_platform]).

% TODO: (hook)
% (only for instype=global)
install_hook_decl(engine).
install_hook(engine) :-
	bold_message("Installing engine and C header files for ~w", [~get_platform]),
	%
	install_item([
          dir(~fsR(bundle_inslib(ciao))),
	  dir(~enginedir) % set_perms or set_exec_perms?
        ]),
	b_install_file_exec(~fsR(concat_k(exec, ~fsR(~build_dir/'objs'/(~get_platformdeb)/(~name_ciaoengine)))), ~fsR(concat_k(exec, ~enginedir/(~name_ciaoengine_arch)))),
	b_install_file_link_as(concat_k(exec, (~name_ciaoengine_arch)), ~fsR(concat_k(exec, ~enginedir/(~name_ciaoengine)))),
	%
	install_c_headers.

install_c_headers :-
	b_install_dir(~fsR(bundle_inslib(ciao)/'include')), % perms?
	SrcDir = ~build_dir/'include'/(~get_platformdeb),
	install_item([
          dir(bundle_inslib(ciao)/'include'/(~get_platformdeb), [files_from(SrcDir, '*'), del_rec])
        ]),
	% include_root
	% TODO: This does not allow several versions installed at the same time
	b_install_dir(~include_root),
	b_install_file_link_as(~fsR(bundle_inslib(ciao)/'include'/(~get_platformdeb)/'ciao_prolog.h'),
	                       ~include_root/'ciao_prolog.h').

	% unused? rm -f ${ROOTPREFIX} ~m_installed_ciaoengine_arch_sta,
	% unused? rm -f ${ROOTPREFIX} ~m_installed_ciaoengine_sta,

% TODO: (hook)
% (only for instype=global)
uninstall_hook_decl(engine).
uninstall_hook(engine) :-
	bold_message("Uninstalling engine and C header files for ~w", [~get_platform]),
	uninstall_item([
	  dir(bundle_inslib(ciao)/'include'/(~get_platformdeb), [del_rec]),
	  file(~include_root/'ciao_prolog.h'),
	  %
	  file(~fsR(concat_k(exec, ~enginedir/(~name_ciaoengine_arch)))),
	  file(~fsR(concat_k(exec, ~enginedir/(~name_ciaoengine))))
        ]).
%	rm -f ${ROOTPREFIX} ~m_installed_ciaoengine_sta,
%	rm -f ${ROOTPREFIX} ~m_installed_ciaoengine_arch_sta.

% ===========================================================================

:- doc(section, "Shell Script Configuration").

% The configuration of shell scripts defines the necessary environment
% variables to make the system locate the installed version of Ciao
% code and binaries (executables, libraries, documentation, etc.) in
% Unix-like systems.

register_bashrc :-
	( update_bashrc(yes) ->
	    (-register_in_script(~dotbashrc, "#", ~bashrc_lines))
	; true
	).
register_cshrc :-
	( update_cshrc(yes) ->
	    (-register_in_script(~dotcshrc, "#", ~cshrc_lines))
	; true
	).

unregister_bashrc :-
	( update_bashrc(yes) ->
	    (-unregister_from_script(~dotbashrc, "#"))
	; true
	).
unregister_cshrc :-
	( update_cshrc(yes) ->
	    (-unregister_from_script(~dotcshrc, "#"))
	; true
	).

bashrc_lines(S) :-
	ciaolibetcsrc(LibRoot),
	shell_config_code(bash, LibRoot, S, []).
cshrc_lines(S) :-
	ciaolibetcsrc(LibRoot),
	shell_config_code(csh, LibRoot, S, []).

% TODO: Obtain the path from the code that installs the configuration
%       files!
ciaolibetcsrc(LibRoot) :-
	( instype(local) ->
	    LibRoot = ~fsR(bundle_src(ciao)/'etc')
	; LibRoot = ~fsR(~ciaolib_root/'ciao')
	).

% Configuration code for the shell script interpreters
shell_config_code(bash, LibRoot) -->
	"if [ -f ", emit_atom(LibRoot), "/DOTprofile ] ; then\n"||
	"  . ", emit_atom(LibRoot), "/DOTprofile\n"||
	"fi\n".
shell_config_code(csh, LibRoot) -->
	"if ( -e ", emit_atom(LibRoot), "/DOTcshrc ) then\n"||
	"  source ", emit_atom(LibRoot), "/DOTcshrc\n"||
	"endif\n".

% (emit an atom codes in a DCG)
emit_atom(X, S, S0) :-
	atom_codes(X, Codes),
	append(Codes, S0, S).

% ===========================================================================

:- doc(section, "Tests and Benchmarks").

% TODO: Isolate this code in other module: it uses dynamic use_module
% TODO: Launch from a different process (so that interference with other code is minimized)

:- use_module(library(compiler), [use_module/2]).
:- use_module(library(unittest)).

runtests <- [unittests, isotests, ciaotests] :- true.

unittests <- [] # "Run Ciao unit tests" :-
	bold_message("Running Ciao tests"),
	run_test_dir(~fsR(bundle_src(ciao)/'lib'),     [rtc_entry]),
	run_test_dir(~fsR(bundle_src(ciao)/'library'), [rtc_entry]),
	run_test_dir(~fsR(bundle_src(ciao)/'contrib'), [rtc_entry]).

ciaotests <- [] # "Run Ciao tests" :-
	proc_if_dir_exists(~fsR(bundle_src(ciao)/'tests'), do_ciaotests).

do_ciaotests :-
	working_directory(ThisDir, ThisDir),
	working_directory(_,       tests),
	RunTests = 'run_tests',
	use_module(ciaosrc(tests(RunTests)), [run_tests/0]),
	RunTests:run_tests,
	working_directory(_, ThisDir).

isotests <- [] # "Run ISO-prolog tests" :-
	IsoTestsDir = ~fsR(bundle_src(ciao)/'contrib'/'iso_tests'),
	proc_if_dir_exists(IsoTestsDir, do_isotests(IsoTestsDir)).

do_isotests(IsoTestsDir) :-
	bold_message("Running ISO-prolog tests"),
	run_test_dir(IsoTestsDir, [rtc_entry]).

runbenchmarks <- [] # "Run Benchmarks" :-
	ECRC = 'ecrc',
	use_module(ciaosrc(library(benchmarks(ECRC))), [main/1]),
	ECRC:main([]).
% TODO: also include (or merge) these:
%   optim_comp/testsuite/multisystemtests/quick_test.sh
%   ciao/examples/misc/ (see Makefile) -- too small, need at least scaling

% ============================================================================

:- doc(section, "Emacs Mode").

emacsmode_dir := bundle_src(ciaode)/'emacs-mode'.

ciaoreallib_dir := ~ciaoreallib_dir_(~instype).
ciaoreallib_dir_(local) := ~fsR(bundle_src(ciao)).
ciaoreallib_dir_(global) := ~fsR(bundle_inslib(ciao)).

emacsstylepath('Win32', Dir, Path) :- !,
	atom_codes(Dir, SDir),
	cyg2win(SDir, Path, noswap).
emacsstylepath(_, Dir, Path) :- !,
	atom_codes(Dir, Path).

emacs_type(EmacsType) :-
	% TODO: emacs_type is set to 'Win32' only from makedir/runwin32.bat
	name_value(emacs_type, EmacsType),
	!.
emacs_type(posix).

% TODO: Why? 
script_extension('Win32', '.bat') :- !.
script_extension(_,       '').

% Here is how this all works: 
% 
% - During installation ('ciaosetup build emacs_mode' and 'ciaosetup
%   install_emacs_mode'):
%
%   * The ciao-config.el.skel file is filled with configuration
%     parameters for the installed system to produce ciao-config.el,
%     which is installed in the libraries. The current Ciao version is
%     also included in ciao-config.el at this time.
%
%   * All .el files in the libraries are byte-compiled.
%    
% - Generating the documentation ('ciaosetup docs'):
%
%   * CiaoMode.lpdoc is generated from ciao-documentation.el using
%     emacs (contains and extracts the documentation for all the elisp
%     functions). CiaoMode.pl is included as a chapter in the Ciao
%     manual.

% TODO: Move to a specific installation for emacs-mode
% (used only from 'ciaosetup')
build_emacs_mode <- :- build_item(emacs_mode).
install_emacs_mode <- :-
        install_item(~emacs_mode_desc).
uninstall_emacs_mode <- :-
        uninstall_item(~emacs_mode_desc).

emacs_mode_desc := 
  big_group("Emacs mode for Ciao", [
    only_global_ins([
      emacs_mode
    ])
  ]).

% TODO: Each bundle should be able to register parts of the CiaoMode
%       That will solve @bug{lpdoclibdir_emacs_mode}
build_hook_decl(emacs_mode).
build_hook(emacs_mode) :-
	build_emacs_mode.

% ---------------------------------------------------------------------------
% (in order to share code, make the emacs kind a parameter)

with_emacs_mode_k(emacs) :- with_emacs_mode(yes).
with_emacs_mode_k(xemacs) :- with_xemacs_mode(yes).

update_dotemacs_k(emacs) :- update_dotemacs(yes).
update_dotemacs_k(xemacs) :- update_dotxemacs(yes).

dotemacs_k(emacs) := ~dotemacs.
dotemacs_k(xemacs) := ~dotxemacs.

with_any_emacs_mode :-
        ( with_emacs_mode_k(emacs) -> true
	; with_emacs_mode_k(xemacs)
	).

emacs_site_start_k(emacs) := ~emacs_site_start.
emacs_site_start_k(xemacs) := ~xemacs_site_start.

% ---------------------------------------------------------------------------

% TODO: I need to change lpdist/skip_settings:skip_raw_files/1 if more
%       files are added.
build_emacs_mode :-
	\+ with_any_emacs_mode,
	!,
	note_message("Emacs support will not be installed").
build_emacs_mode :-
	% TODO: Use better log names (append them?)
	bold_message("Building the Emacs mode for Ciao"),
	% First, generate 'ciao-config.el' from 'ciao-config.el.skel'
	generate_emacs_config,
	% Generate autoloads automatically with 'batch-update-autoloads'
	Dir = ~emacsmode_dir,
	Init = ~fsR(Dir/'ciao-site-file.el'),
	emacs_update_autoloads(Dir, 'emacs_mode3', Init),
	% Compile to elisp bytecode the .el files
        EL = ~ciao_mode_el_files,
        ELC = ~ciao_mode_elc_files,
	emacs_batch_byte_compile(Dir, 'emacs_mode', EL),
	set_perms(~fsRlist_rel(Dir, ELC), ~perms),
	bold_message("Emacs mode for Ciao build completed").

% The path of alias 'lpdoclib'
% TODO: @begin{bug}{lpdoclibdir_emacs_mode}
%   Duplicated in makedir_part_lpdoc.pl. This should not be necessary if
%   the emacs mode calls LPdoc, or if the emacs-mode can be extended with
%   parts from other bundles dynamically.
% @end{bug}
my_lpdoclib_dir := ~my_lpdoclib_dir_(~instype).
my_lpdoclib_dir_(local) := ~fsR(bundle_src(lpdoc)/lib).
my_lpdoclib_dir_(global) := ~fsR(concat_ver(lpdoc, ~ciaolib_root/lpdoc/('lpdoc'))).

get_bindir_for_emacs(EmacsType, EmacsDir) :-
	( EmacsType = 'Win32' ->
	    % TODO: Why?
	    Dir = ~buildbin_dir
	; Dir = ~ciaobin_dir
	),
	get_dir_for_emacs(EmacsType, Dir, EmacsDir).

% TODO: use ciao-root-dir for all EmacsType?
get_dir_for_emacs('MacOSBundle', Dir, EmacsDir) :- !,
	flatten(["(concat ciao-root-dir \"", ~atom_codes(Dir), "\")"], EmacsDir).
get_dir_for_emacs(EmacsType, Dir, EmacsDir) :-
	flatten(["\"", ~emacsstylepath(EmacsType, Dir), "\""], EmacsDir).

get_app_for_emacs(EmacsType, App, Bundle, Kind, AppShell) :-
	registered_bundle(Bundle),
	get_app_for_emacs_(EmacsType, App, Bundle, Kind, AppShell),
	!.
% Avoid failure if the App is not being installed:
get_app_for_emacs(_, App, _Version, _, AppShell) :-
	flatten(["\"", ~atom_codes(App), "\""], AppShell).

get_app_for_emacs_(EmacsType, App, Bundle, Kind, AppShell) :-
	( Kind = plexe -> K = plexe
	; Kind = script -> K = ext(~script_extension(EmacsType))
	),
	App2 = ~fsR(concat_verk(Bundle, K, App)),
	AppShell = ~flatten(["(concat ciao-bin-dir \"/", ~atom_codes(App2), "\")"]).

% Avoid failure if the bundle is not there
get_bundle_manual_base_for_emacs(Bundle, NameVersion):-
	registered_bundle(Bundle),
	bundle_manual_base(Bundle, NameVersion), 
	!.
get_bundle_manual_base_for_emacs(Bundle, Bundle).

% ---------------------------------------------------------------------------
% Generate ciao-config.el from ciao-config.el.skel

generate_emacs_config :-
	emacs_type(EmacsType),
	EmacsModeDir = ~emacsmode_dir,
	%
	% TODO: 'EmacsType' probably should not be necessary
	emacs_type_specific(EmacsType, String, Tail3),
	%
	% TODO: this should not be necessary; the emacs-mode should
	% call lpdoc to do things such as generation of SETTINGS.pl
	load_template(EmacsModeDir/'ciao-config.el', [
            'CIAODE_VERSION' = ~bundle_version_patch(ciaode),
	    % Paths
	    'CIAOBINDIR' = ~get_bindir_for_emacs(EmacsType),
	    'CIAOREALLIBDIR' = ~get_dir_for_emacs(EmacsType, ~ciaoreallib_dir),
	    'LPDOCDIR' = ~get_dir_for_emacs(EmacsType, ~docdir),
	    'LPDOCLIBDIR' = ~get_dir_for_emacs(EmacsType, ~my_lpdoclib_dir),
	    % Manuals
            'CIAO_MANUAL_BASE' = ~get_bundle_manual_base_for_emacs('ciao'),
            'CIAOPP_MANUAL_BASE' = ~get_bundle_manual_base_for_emacs('ciaopp'),
            'LPDOC_MANUAL_BASE' = ~get_bundle_manual_base_for_emacs('lpdoc'),
	    % Binaries
	    'PLINDENT' = ~get_app_for_emacs(EmacsType, 'plindent', ciao, plexe),
	    'CIAOSHELL' = ~get_app_for_emacs(EmacsType, 'ciao', ciao, script),
	    'CIAOPPSHELL' = ~get_app_for_emacs(EmacsType, 'ciaopp', ciaopp, script),
	    'LPDOCEXEC' = ~get_app_for_emacs(EmacsType, 'lpdoc', lpdoc, plexe)
        ], Tail3, ""),
	%
	string_to_file(String, ~fsR(EmacsModeDir/'ciao-config.el')).

% TODO: use ciao-root-dir for all EmacsType?
emacs_type_specific('MacOSBundle', String, Tail) :- !,
	% TODO: do not use a hardwired value for Root!
	Root = '/usr/lib/ciao',
	% TODO: do not use a hardwired value for Engine!
	Lib = ~fsR(concat_ver(ciao, Root/'ciao')),
	Engine = ~fsR(Lib/'engine'/'ciaoengine.DARWINi86'),
	append([
	    ";; Beginning of the specific part to MacOS Application Bundle\n"||
	    "(defvar ciao-root-dir "||
	    "\"/Applications/Emacs.app/Contents/Resources\"\n"||
	    "        \"path of Emacs Application Bundle\")\n\n"||
	    "(setenv \"CIAOENGINE\" (concat ciao-root-dir \"", ~atom_codes(Engine), "\"))\n"||
	    "(setenv \"CIAOLIB\" (concat ciao-root-dir "||
	    "\"", ~atom_codes(Lib), "/\"))\n\n"||
	    ";; End of the specific part to MacOS Application Bundle\n\n", Tail],
	    String).
emacs_type_specific('Win32', String, Tail) :- !,
	emacsstylepath('Win32', ~build_doc_dir, SDirS),
	append([
		";; Specific to Windows installation:\n"||
		";; Location of info manuals\n"||
		"(setq Info-default-directory-list  (cons \n"||
		"      \""|| SDirS, "\" \n"||
		"      Info-default-directory-list))\n"||
%%% Should put this in a separate file in SRC/emacs-mode and cat it now:
		";; Make things nicer (but check if you are already doing it)\n",
		"(global-font-lock-mode)\n"||
		"(transient-mark-mode t)\n"||
		";; Help for using the Windows command.com as your shell\n"||
		";; (comment out if you use bash, etc.):\n"||
		"(setq process-coding-system-alist \n"||
		"'((\"cmdproxy\" . (raw-text-dos . raw-text-dos))))\n"||
		";; Preventing ctrln-m's from being printed in the shell\n"||
		"(add-hook 'comint-output-filter-functions "||
		"  'shell-strip-ctrl-m nil t)\n"||
		"; -----"||
                "----------------------------------------------------------------\n\n"||
		Tail],
	    String).
emacs_type_specific(_, T, T).

% ---------------------------------------------------------------------------

% (called from 'ciaosetup' (for testing))
register_emacs_mode <- [] :- register_emacs_mode.
% (called from 'ciaosetup' (for testing))
unregister_emacs_mode <- [] :- unregister_emacs_mode.

register_emacs_mode :-
	register_emacs(emacs),
	register_emacs(xemacs).

register_emacs(EmacsKind) :-
	( with_emacs_mode_k(EmacsKind) ->
	    ( emacs_init_file(EmacsKind, InitFile) ->
	        (-register_in_script(InitFile, ";", ~emacs_config))
	    ; % Do not register
	      true
	    )
	; true
	).

unregister_emacs_mode :-
	unregister_emacs(emacs),
	unregister_emacs(xemacs).

unregister_emacs(EmacsKind) :-
	( with_emacs_mode_k(EmacsKind) ->
	    ( emacs_init_file(EmacsKind, InitFile) ->
	        (-unregister_from_script(InitFile, ";"))
	    ; true
	    )
	; true
	).

emacs_config(S) :-
	Lib = ~ciaolibemacs,
	emacs_type(EmacsType),
	Lib2S = ~emacsstylepath(EmacsType, Lib),
	atom_codes(Lib2, Lib2S),
	emacs_config_(Lib2, S, []).

emacs_config_(Lib) -->
	"(if (file-exists-p \"", emit_atom(Lib), "\")\n"||
	"(load-file \"", emit_atom(Lib), "\")\n"||
	")\n".

% The absolute path for the 'ciao-site-file.el' file
ciaolibemacs(LibEmacs) :-
	( instype(local) ->
	    LibEmacs = ~fsR(~emacsmode_dir/'ciao-site-file.el')
	; % TODO: Place the version in the right place automatically?
	  % TODO: Verify that the rest of .el files are in the correct directory.
	  LibEmacs = ~fsR(concat_ver(ciao, ~ciaolib_root/ciao/('ciao'))/'ciao-site-file.el')
	).

% ---------------------------------------------------------------------------

% TODO: (hook)
% TODO: docs always must be done after build, not before!
% Creation of LPdoc documentation files for emacs-mode

% TODO: (temporal hook before emacs mode is separated in its own bundle)
emacs_mode_docs <- [] :- docs_emacs.

docs_emacs :-
	mkf_emacs_docs.

mkf_emacs_docs :-
	EmacsModeDir = ~emacsmode_dir,
	emacs_batch_call(EmacsModeDir, 'emacs_mode2', % TODO: right log name?
	  ['--eval \'(setq load-path (cons "." load-path))\'',
	   ' -l ciao-documentation.el',
	   ' -f ciao-mode-documentation']),
	%
	set_perms(~fsR(EmacsModeDir/'CiaoMode.lpdoc'), ~perms),
	touch(~fsR(EmacsModeDir/'CiaoMode.pl')).

icon_dir := ~fsR(bundle_inslib(ciao)/'icons').

%-----------------------------------------------------------------------------

% TODO: (hook)
% (only for instype=global)
install_hook_decl(emacs_mode).
install_hook(emacs_mode) :-
	bold_message("Installing Ciao emacs-mode (Emacs based IDE)"),
        % (only for global installation)
	Dir = ~emacsmode_dir,
	Loader = ~fsR(Dir/'ciao-mode-init.el'),
        string_to_file(~emacs_config, Loader),
	( with_any_emacs_mode ->
	    mkf_emacs_install
	; true
	).

% TODO: (hook)
% (only for instype=global)
uninstall_hook_decl(emacs_mode).
uninstall_hook(emacs_mode) :-
	bold_message("Uninstalling Ciao emacs-mode (Emacs based IDE)"),
	( with_any_emacs_mode ->
	     mkf_emacs_uninstall
	; true
	).

mkf_emacs_install :-
	install_item(~mkf_emacs_mode_desc).

mkf_emacs_uninstall :-
	uninstall_item(~mkf_emacs_mode_desc).

mkf_emacs_mode_desc := [
	  dir(~fsR(bundle_inslib(ciao)), [do_not_del]),
	  dir(~icon_dir, [files_from(~emacsmode_dir/icons), del_rec]),
	  ~ciao_mode_lisp_desc,
	  ~ciao_mode_init_desc(xemacs),
	  ~ciao_mode_init_desc(emacs)
	].

% TODO: Remember to 'update ciao/library/lpdist/win32/CiaoDE.iss.skel'
%       if the files here are modified. Ideally, that file should not
%       contain any hardwired list of files.

% TODO: Merge this part with register. Make emacsinitfile a particular
% case.
ciao_mode_init_desc(EmacsKind) := Desc :-
	with_emacs_mode_k(EmacsKind),
        MidDir = ~emacs_site_start_k(EmacsKind),
        is_site_start_d(MidDir),
	!,
        Mid = MidDir/(~emacsinitfile),
	Desc = [
          dir(MidDir, [do_not_del]),
          lib_file_list(~emacsmode_dir, [
            'ciao-mode-init.el'-[mid(Mid)] % TODO: why?
          ])
        ].
ciao_mode_init_desc(_) := [].

% Obtain the appropriate configuration file for this system or
% installation (.emacs or site-start.el). This predicate fails if no
% change is required, because the mode is installed through the
% site-start.d/ directory (Debian only?), or because the Ciao Emacs
% Mode is disabled.
emacs_init_file(EmacsKind) := InitFile :-
        ( % Local installation, register in your .emacs file
	  instype(local), update_dotemacs_k(EmacsKind) ->
	    InitFile = ~dotemacs_k(EmacsKind)
	; % Register in the site-start file (just in case that)
	  % the site-start.d directory was not found; see
	  % `ciao_mode_init_desc/2`)
	  Dir = ~emacs_site_start_k(EmacsKind),
	  \+ is_site_start_d(Dir) ->
	    InitFile = ~fsR(Dir/'site-start.el')
	; % No init file has to be modified
	  fail
	).

% Check that Dir is a site-start.d directory
is_site_start_d(Dir) :-
        atom_concat(_, '/site-start.d', Dir).

ciao_mode_lisp_desc :=
	lib_file_list(~emacsmode_dir, 
          ~append(
            ~addprops(~ciao_mode_el_files, [copy_and_link]),
            ~addprops(~ciao_mode_elc_files, [copy_and_link]))).

ciao_mode_lisp_files := [
	'word-help',
	'ciao-help',
	'ciao-faces',
	'ciao-syntax',
	'ciao-parsing',
	'ciao-aux',
	'ciao-font-lock',
	'ciao-vc',
	'ciao-scratchpad',
	'ciao-process',
	'ciao-compile',
	'ciao-loading',
	'ciao-testing',
	'ciao-debugger',
	'ciao-lpdoc',
	'ciao-ciaopp',
	'java-ciaopp',
	'ciao-bundle',
	'ciao-optim-comp',
	'ciao-org',
	'ciao-widgets',
	'ciao-common',
	'ciao-config',
	'ciao-splash',
	'ciao-bindings',
	'ciao'].

ciao_mode_el_files := ~add_suffix(~ciao_mode_lisp_files, '.el').
ciao_mode_elc_files := ~add_suffix(~ciao_mode_lisp_files, '.elc').

% ----------------------------------------------------------------------------

clean_emacs_mode <- :- clean_emacs_mode.
clean_emacs_mode :-
	bold_message("Cleaning up emacs directory...", []),
	EmacsModeDir = ~emacsmode_dir,
	% TODO: necessary? repeated?
	del_file_nofail(~fsR(EmacsModeDir/'ciao-site-file.el')),
	% clean log files
	clean_log(EmacsModeDir, 'emacs_mode'),
	clean_log(EmacsModeDir, 'emacs_mode2'),
	clean_log(EmacsModeDir, 'emacs_mode3').
%	del_file_nofail(~fsR(EmacsModeDir/'*~' pattern?)).

distclean_emacs_mode <- :-
	clean_emacs_mode,
	EmacsModeDir = ~emacsmode_dir,
	% (automatically generated files)
	del_file_nofail(~fsR(EmacsModeDir/'CiaoMode.lpdoc')),
	del_file_nofail(~fsR(EmacsModeDir/'ciao-config.el')),
	del_files_nofail(~add_prefix(~ls(~fsR(EmacsModeDir), '*.itf|*.po|*.asr|*.elc'),
	                             ~atom_concat(~fsR(EmacsModeDir), '/'))).

% ---------------------------------------------------------------------------
% Interface to Emacs batch functions

% Invoke the Emacs 'batch-update-autoloads' function to generate the
% autoload file AutoloadEL.
emacs_update_autoloads(Dir, Log, AutoloadEL) :-
	emacs_type(EmacsType),
	AutoloadEL2S = ~emacsstylepath(EmacsType, AutoloadEL),
	atom_codes(AutoloadEL2, AutoloadEL2S),
	%
	emacs_batch_call(Dir, Log,
	                 ~atom_concat([
		           '--eval \'(setq generated-autoload-file "', AutoloadEL2, '")\'',
			   ' -f batch-update-autoloads "."'])).

% Invoke the Emacs 'batch-byte-compile' function to byte compile the
% specified EL_Files elisp files.
emacs_batch_byte_compile(Dir, Log, EL_Files) :-
	emacs_batch_call(Dir, Log,
	  ['--eval \'(setq load-path (cons "." load-path))\'',
	   ' -f batch-byte-compile '|
           (~listsep(EL_Files, ' '))]).

% ---------------------------------------------------------------------------
% (Auxiliary predicates)

% From [X1,...Xn], get [X1, Sep, ..., Sep, Xn]
listsep([], _Sep) := [] :- !.
listsep([X], _Sep) := [X] :- !.
listsep([X|Xs], Sep) := [X, Sep | ~listsep(Xs, Sep)].

% Add the property list Prop to each of Xs (for bundle description)
addprops([], _Props) := [].
addprops([X|Xs], Props) := [X-Props | ~addprops(Xs, Props)].

% Resolve a list of files, relative to Dir
fsRlist_rel(Dir, Xs) := ~fsRlist_rel_(Xs, Dir).

fsRlist_rel_([], _) := [].
fsRlist_rel_([X|Xs], Dir) := [Y| ~fsRlist_rel_(Xs, Dir)] :-
	Y = ~fsR(Dir/X).

% ===========================================================================
% Enumeration of the standalone utilities in etc/ and etc_contrib/
% TODO: Generalize and split

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms)).

etc_dir := bundle_src(ciao)/etc.
etc_contrib_dir := bundle_src(ciao)/etc_contrib.

def_hook_decl(etc_utilities).
def_hook(etc_utilities) :=
	group("etc/ standalone utilities",
          standalone_list(ciao, ~etc_dir, ~etc_utilities)).

etc_utilities := ~findall(B-[K], etc_utility(B, K)).

etc_utility(B, K) :-
	( plutility(B), K = plexe
	; shscript(B), K = shscript
	).

plutility :=
	'fileinfo'|
	'get_deps'|
	'pldiff'|
	'viewpo'|
	'lpmake'|
	'plindent'|
	'show_asr'|
	'show_deps'|
	'compiler_output'|
	'checkline'.

% TODO: strange... enumerated for installation, add a table of exec+kind instead
shscript :=
	'ciao_get_arch'|
        'ciao'.

def_hook_decl(etc_contrib_utilities).
def_hook(etc_contrib_utilities) :=
	group("etc_contrib/ standalone utilities",
	  standalone_list(ciao, ~etc_contrib_dir, ~etc_contrib_utilities)).

etc_contrib_utilities := ~findall(B-[K], etc_contrib_utility(B, K)).

etc_contrib_utility(B, K) :-
	plutility_contrib(B), K = plexe.

% TODO: distribute those utils?
plutility_contrib :=
	'synch_actions'|
	'cleandirs'.

% ============================================================================

% (hook)
build_etc :-
	build_item(dot_shell(csh)),
	build_item(dot_shell(sh)),
	build_item(etc_utilities),
	build_item(etc_contrib_utilities),
	build_item(ciao_get_arch),
	build_item(ciaocl).

% Generate shell initialization files
build_hook_decl(dot_shell(_)).
build_hook(dot_shell(Sh)) :-
	normal_message("Creating ~w", [~dot_shell_file(Sh)]),
	wr_template(origin, ~etc_dir, ~dot_shell_file(Sh), [
	    'CiaoDocDir' = ~docdir,
	    'CiaoBinDir' = ~ciaobin_dir,
	    'ConfigOptimizingCompiler' = ~optimizing_compiler_string(Sh)
        ]).

dot_shell_file(csh) := 'DOTcshrc'.
dot_shell_file(sh) := 'DOTprofile'.

:- use_module(library(llists), [flatten/2]).

% TODO: Merge
optimizing_compiler_string(Shell) :=
	~optimizing_compiler_string_(Shell, ~optimizing_compiler).
optimizing_compiler_string_(_, no) := "" :- !.
optimizing_compiler_string_(csh, yes) :=
	~flatten(["eval `", ~atom_codes(~ciao_lib_dir), "/optim_comp/ciaotool csh-env`"]).
optimizing_compiler_string_(sh, yes) :=
	~flatten(["eval $(", ~atom_codes(~ciao_lib_dir), "/optim_comp/ciaotool bash-env)"]).

% Generate 'ciao' startup script
build_hook_decl(ciaocl).
build_hook(ciaocl) :-
	normal_message("Building '~w' (command line)", ['ciao']),
	wr_template(kver(shscript, ciao), ~etc_dir, 'ciao', [
	    'ExtraCommands' = ~ciao_extra_commands,
	    'CiaoVersion' = ~bundle_version(ciao),
	    'CiaoSuffix' = ~get_ciao_ext,
	    'CiaoBinDir' = ~ciaobin_dir
        ]),
	b_link(shscript, ciao, 'ciao'),
 	( install_prolog_name(yes) ->
 	    b_link_as(shscript, ciao, 'ciao', 'prolog')
 	; true
 	).

build_hook_decl(ciao_get_arch).
build_hook(ciao_get_arch) :-
	normal_message("Building '~w' (command line)", ['ciao_get_arch']),
	% TODO: we build nothing here (just copy) but the user does not want to know
	b_copy(shscript, ciao, ~etc_dir, 'ciao_get_arch'),
	b_link(shscript, ciao, 'ciao_get_arch').

% ============================================================================

def_hook_decl(dot_shell_).
def_hook(dot_shell_) :=
  small_group("shell initialization scripts",
    lib_file_list(~etc_dir, [
      'DOTprofile'-[copy_and_link],
      'DOTcshrc'-[copy_and_link]
    ])).

def_hook_decl(ciaocl_).
def_hook(ciaocl_) := R :-
	( install_prolog_name(yes) ->
	    Opts = [link_as('prolog')]
	; Opts = []
	),
	R = small_group("'ciao' (command line)", 
	      bin_copy_and_link(shscript, ciao, 'ciao', Opts)).

% (only for instype=global)
install_hook_decl(etc).
install_hook(etc) :-
	install_item(~etc_desc).

% (only for instype=global)
uninstall_hook_decl(etc).
uninstall_hook(etc) :-
	uninstall_item(~etc_desc).

etc_desc := [
	dir(~ciaobin_dir, [do_not_del]),
	dot_shell_,
	ciaocl_,
	etc_utilities,
	etc_contrib_utilities
        ].

% TODO: 'NewUser' (like the rest of README files) is not
%       installed together with the documentation. Is it right?
%%	b_uninstall_file(~lib_dir/'NewUser'),




