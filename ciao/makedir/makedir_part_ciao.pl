% ===========================================================================
:- module(_, _, [dcg, make, fsyntax, hiord]).
% ===========================================================================
:- doc(title,  "Ciao Global Compilation/Installation installer").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system").
% ===========================================================================

:- use_module(library(terms),        [atom_concat/2]).
:- use_module(library(lists),        [append/3]).
:- use_module(library(llists),       [flatten/2, append/2]).
:- use_module(library(compiler),     [use_module/2]).
:- use_module(library(gen_asr_file), [gpo/1]).
:- use_module(library(aggregates)).
:- use_module(library(write)).
:- use_module(library(streams)).
:- use_module(library(messages)).
:- use_module(library(file_utils)).
:- use_module(library(make(system_extra))).
:- use_module(library(make(make_rt))).
:- use_module(library(autoconfig)).
:- use_module(library(unittest)).
:- use_module(library(strings)).
:- use_module(library(distutils)).
:- use_module(library(distutils(readme_generator))).
:- use_module(library(distutils(skip_settings))).
:- use_module(ciaodesrc(makedir('ConfigMenu'))).
:- use_module(ciaodesrc(makedir('ConfigValues'))).
:- use_module(ciaodesrc(makedir('MenuOptions'))).
:- use_module(ciaodesrc(makedir('DOCCOMMON'))).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).

:- use_module(engine(system_info), [get_ciao_ext/1, get_exec_ext/1]).

:- include(ciaosrc(makedir(platdep_modules))).

% ============================================================================
% CONSTANTS
% ============================================================================

tags := ~atom_concat(~component_src(ciao), '/TAGS').

defaults <- [] # "Preventing lpmake from being called without target" :- true.

show_info <- :-
	display_list(['ciaodesrc=', ~component_src(ciaode), '\n']).

% ============================================================================
% CONFIGURATION MENU
% ============================================================================

ciaosettings <- [] # "Load initial Ciao Settings" :-
	initvals.

menuconfig <- [ciaosettings] # "Ciao configuration" :-
	menuconfig.

silentconfig <- [ciaosettings] # "Ciao configuration (silent)" :-
	silentconfig.

% ============================================================================
% COMPILATION
% ============================================================================

environment <- [emacs_support, make_header] :- true.

emacs_support <- :-
	(
	    ( install_emacs_support(yes)
	    ; install_xemacs_support(yes)
	    ) -> emacs_mode_compile
	;
	    note_message("Emacs support will not be installed")
	).


emacs_mode_compile <- [] :-
	emacs_mode_compile.

emacs_mode_compile :-
	bold_message("Compiling emacs library files"),
	do_emacs_mode_compile.

% TODO: wrong name, only for the emacs mode
% (see also emacs-mode/Makefile)
ciao_lib_dir_(src) := ~atom_concat(~component_src(ciaode), '/emacs-mode').
ciao_lib_dir_(ins) := ~libdir.

ins_ciao_lib_dir := ~ciao_lib_dir_(~instype).

ciaoreallibdir_(src) := ~component_src(ciao).
ciaoreallibdir_(ins) := ~reallibdir.

ciaoreallibdir := ~ciaoreallibdir_(~instype).

emacsstylepath('Win32', Dir, Path) :- !,
	atom_codes(Dir, SDir),
	cyg2win(SDir, Path, noswap).
emacsstylepath(_, Dir, Path) :- !,
	atom_codes(Dir, Path).


emacs_path(Emacs) :-
	name_value(emacs_path, Emacs0),
	winpath(Emacs, Emacs0),
	!.
emacs_path(Emacs) :-
	% find_emacs(Emacs),
	emacs_for_ciao(Emacs),
	!.
emacs_path(XEmacs) :-
	% find_xemacs(XEmacs).
	xemacs_for_ciao(XEmacs).

emacs_type(EmacsType) :-
	name_value(emacs_type, EmacsType),
	!.
emacs_type(posix).

script_extension('Win32', ".bat") :- !.
script_extension(_,       "").

do_emacs_mode_compile :-
	emacs_type(EmacsType),
	get_dir_for_emacs(EmacsType, ins_ciao_lib_dir, CiaoLibDir),
	get_dir_for_emacs(EmacsType, docdir, DocDir),
	replace_strings_in_file([
		["<v>CIAOLIBDIR</v>", CiaoLibDir],
		["<v>LPDOCDIR</v>", DocDir]],
	    '../emacs-mode/ciao-mode-init.el.skel',
	    '../emacs-mode/ciao-mode-init.el'),
	replace_strings_in_file([["\n", "\n;;"]],
	    '../emacs-mode/ciao-mode-init.el',
	    '../emacs-mode/ciao-mode-init.tmp'),
	replace_strings_in_file([["\n", "\n;;"]],
	    '../emacs-mode/CiaoMode.pl',
	    '../emacs-mode/CiaoMode.pl.tmp'),
	set_perms(['../emacs-mode/ciao-mode-init.el'], ~perms),
	file_to_string('../emacs-mode/ciao.el.header', String, ";;"|| Tail0),
	file_to_string('../emacs-mode/ciao-mode-init.tmp', Tail0, Tail1),
	file_to_string('../emacs-mode/CiaoMode.pl.tmp',    Tail1, Tail2),
	emacs_type_specific(EmacsType, Tail2, Tail3),
	% Generation of ciao.el.tmp
	get_dir_for_emacs(EmacsType, ciaobindir,     CiaoBinDir),
	get_dir_for_emacs(EmacsType, ciaoreallibdir, CiaoRealLibDir),
	get_app_for_emacs(EmacsType, "plindent", ciao, ~atom_codes(~get_ciao_ext), PlIndent),
	script_extension(EmacsType, ScriptExt),
	get_app_for_emacs(EmacsType, "ciao",   ciao,   ScriptExt, CiaoShell),
	get_app_for_emacs(EmacsType, "ciaopp", ciaopp, ScriptExt, CiaoPPShell),
	get_dir_for_emacs(EmacsType, get_lpdoclibdir, LpdocLibDir),
	replace_strings_in_file([["<v>DEVELOPMENT_VERSION</v>",
		    ~atom_codes(~componentversion(ciaode))],
		["<v>CIAOBINDIR</v>",     CiaoBinDir],
		["<v>CIAOREALLIBDIR</v>", CiaoRealLibDir],
		["<v>LPDOCDIR</v>",       DocDir],
		["<v>PLINDENT</v>",       PlIndent],
		["<v>CIAOSHELL</v>",      CiaoShell],
		["<v>CIAOPPSHELL</v>",    CiaoPPShell],
		["<v>LPDOCLIBDIR</v>",    LpdocLibDir]],
	    '../emacs-mode/ciao.el.skel', '../emacs-mode/ciao.el.tmp'),
	%
	file_to_string('../emacs-mode/ciao.el.tmp', Tail3, ""),
	string_to_file(String, '../emacs-mode/ciao.el'),
	do(['cd ../emacs-mode ; \"', ~emacs_path,
		'\" -batch -l word-help.el -l ciao.el -f compile-ciao-mode'],
	    '../emacs-mode/emacs_mode.log', '../emacs-mode/emacs_mode.err',
	    [show_error_on_error, show_output_on_error, nofail]),
	del_files_nofail(['../emacs-mode/CiaoMode.pl.tmp',
		'../emacs-mode/ciao-mode-init.tmp',
		'../emacs-mode/ciao.el.tmp']),
	set_perms(['../emacs-mode/ciao.elc', '../emacs-mode/word-help.elc'], ~perms).

% Note: This part should be revised: need more integration with common
% installation process - EMM

make_header <- [] :-
	get_os('Win32') ->
	make_header(~component_src(ciaode))
    ;
	true.

make_header(CiaoDEPath) :-
	get_platform(Platform),
	get_exec_ext(Ext),
	bold_message("Building header to ~w/build/objs/~w/ciaoengine~w",
	    [CiaoDEPath, Platform, Ext]),
	atom_concat(CiaoDEPath, '/ciao/lib/compiler/header', HeaderPath),
	open_output(HeaderPath, Out),
	display('#!/bin/sh\n'),
	display('INSTENGINE=\"'),
	display_list([CiaoDEPath, '/build/objs/', Platform,
		'/ciaoengine', Ext, '\"\n']),
	display('ENGINE=${CIAOENGINE:-${INSTENGINE}}\n'),
	display('exec "$ENGINE" "$@" -C -b $0\n\^L\n'),
	close_output(Out).

windows_bats <- [] :-
	windows_bats(~windows_engine).

windows_engine := ~flatten(["""", ~atom_codes(~winpath(~component_src(ciaode))),
		"\\build\\objs\\", ~atom_codes(~get_platform),
		"\\ciaoengine.exe"""]).

windows_bats(Engine) :-
	bold_message("Building prototype .bat files pointing to ~s", [Engine]),
	( getenvstr('OS', "Windows_NT") ->
	    AllArgs = ' %*'
	;
	    AllArgs = ' %1 %2 %3 %4 %5 %6 %7 %8 %9'
	),
	bat_file(BatFile, Head, Tail),
	open_output(BatFile, Out),
	display(Head),
	display_string("@"|| Engine),
	display(AllArgs),
	display(Tail),
	nl,
	close_output(Out),
	fail
    ;
	true.

bat_file(~atom_concat(~component_src(ciaode), '/build/bin/ciaosh-1.13.bat'),
	    '', ~atom_concat([' -C -i -b \"', ~component_ins(ciaode),
		    '/build/bin/ciaosh-1.13\"'])).
bat_file(~atom_concat(~component_src(ciaode), '/build/bin/ciaoc-1.13.bat'),
	    '', ~atom_concat([' -C -b \"', ~component_ins(ciaode),
		    '/build/bin/ciaoc-1.13\"'])).
bat_file(~atom_concat(~component_src(ciaode), '/build/bin/ciao-1.13.bat'),
	    '', ~atom_concat([' ', ~codes_atom(~ciao_extra_commands),
		    ' -C -i -b \"', ~component_ins(ciaode),
		    '/build/bin/ciaosh-1.13\"'])).

get_dir_for_emacs('Win32', ciaobindir, EmacsDir) :- !,
	flatten(["\"", ~emacsstylepath('Win32', ~component_src(ciaode)),
		 "/build/bin\""], EmacsDir).
get_dir_for_emacs('MacOSBundle', Dir, EmacsDir) :- !,
	flatten(["(concat ciao-root-dir \"",
		~atom_codes(~ Dir), "\")"], EmacsDir).
get_dir_for_emacs(EmacsType, Dir, EmacsDir) :-
	flatten(["\"", ~emacsstylepath(EmacsType, ~ Dir), "\""], EmacsDir).



get_app_for_emacs(EmacsType, App, Component, Ext, AppShell) :-
	component(Component),
	vers(Component, Version),
	get_app_for_emacs_(EmacsType, App, Version, Ext, AppShell),
	!.
% Avoid failure if the App is not being installed:
get_app_for_emacs(_, App, _Version, _, AppShell) :-
	flatten(["\"", App, "\""], AppShell).

get_app_for_emacs_('Win32', App, Version, Ext, AppShell) :-
	!,
	flatten(["(concat ciao-bin-dir \"/", App, "-",
		~atom_codes(Version), Ext, "\")"], AppShell).
get_app_for_emacs_(_, App, Version, Ext) :=
	~flatten(["(concat ciao-bin-dir \"/", App,
		"-", ~atom_codes(Version), Ext, "\")"]).

emacs_type_specific('MacOSBundle',
	    ";; Beginning of the specific to  MacOS Application Bundle\n"||
	    "(defvar ciao-root-dir "||
	    "\"/Applications/Emacs.app/Contents/Resources\"\n"||
	    "        \"path of Emacs Application Bundle\")\n\n"||
	    "(setenv \"CIAOENGINE\" (concat ciao-root-dir "||
	    "\"/usr/lib/ciao/ciao-1.13/engine/ciaoengine.DARWINi86\"))\n"||
	    "(setenv \"CIAOLIB\" (concat ciao-root-dir "||
	    "\"/usr/lib/ciao/ciao-1.13/\"))\n\n"||
	    ";; End of the specific part to  MacOS Application Bundle\n\n"||
	    Tail, Tail) :- !.
emacs_type_specific('Win32', String, Tail) :- !,
	!,
	emacsstylepath('Win32', ~build_doc_dir, SDirS),
% 	atom_codes(~vers(ciaode), Vers),
	append([
		";; Specific to Windows installation:\n"||
% 	   ";; Location of Ciao shell\n",
% 	   "(setq ciao-system (convert-standard-filename \n",
% 	   "      \"",SDirS,"/build/bin/ciaosh-",Vers,".bat\"))\n"||
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

% ============================================================================
% COMPILATION
% ============================================================================

all <- [allgmake, alletc, optimizing_compiler, libraries, environment] :-
	true.

only_eng_compiler <- [allgmake] :-
	true.

platdep <- [alletc, ciao_so_libs, environment, platdep_libs] :- true.

allnolibs <- [automodules, platdep] :- true.

platdep_libs <- [] :- platdep_libs.

platdep_libs :-
	compile_platdep_modules(~compiling_options).

% All except the engine, due to it is being used
allgmake <- :-
	make_subdir(~lpmake, ~gmake, '.', '', platdep, ~command_option).

alletc <- :-
	lpmake_subdir(etc, makedir_part_ciao_etc, all).

optimizing_compiler <- :-
	make_subdir(~lpmake, ~gmake, '.', '',
	    optimizing_compiler, ~command_option).

tags <- [~tags] # "Creation of TAGS for use with the find-tag command "||
	"(ESC-.) in emacs" :- true.

~tags <- ['Makefile.pl'] :-
	tags(Tags),
	component_src(ciao, Ciaosrc),
	del_file_nofail(Tags),
	list_filter_files_rec(Ciaosrc, '*.pl', '', '', '', [], List),
	etags(List, Tags).

deltags <- [] # "Deletion of TAGS file" :-
	del_file_nofail(~tags).

% Kludge: always defaulttype is dyn, see DEFAULTYPE variable in
% makefile-sysdep/... - EMM

defaulttype(dyn).

libraries <- [automodules, alllib, ~atom_concat(alllibrary, ~defaulttype),
	    allcontrib] :-
	update_so_list.

update_so_list :-
	output_to_file(dump_platdep_modules, 'makedir/platdep_modules.pl').

modulesdir(lib).
modulesdir(library).
modulesdir(contrib).

:- use_module(library(sort)).

dump_platdep_modules :-
	findall(platdep_module(Dir, File, FileName),
	    (
		modulesdir(LibDir),
		current_platdep_module(LibDir, Dir, File, FileName)
	    ),
	    PML0),
	sort(PML0, PML),
	list(PML, portray_clause).

current_platdep_module(BaseDir, Dir, File, FileName) :-
	current_dir_module(BaseDir, Dir, File, FileName),
	atom_concat(FileBase, '.pl', FileName),
	atom_concat([FileBase, '_', ~get_platform, ~get_so_ext], FileSO),
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
	atom_concat(Dir, '/NOCOMPILE', NCDir),
	( file_exists(Dir), \+ file_exists(NCDir) -> call(Goal)
	; true ).

automodules <- :-
	icon_address_auto,
	mathlibs,
	ppl_interface,
	mysqllibs,
	miniprolog.

miniprolog :-
	proc_if_dir_exists(~atom_concat(~component_src(ciao),
		'/contrib/miniprolog'), do_miniprolog),
	copy_mp_auto.

:- meta_predicate compiling_options_(list(pred(1)), list(pred(1))).
compiling_options_ -->
	{gen_ciao_asr(GCA)},
	asr_option(GCA).

:- meta_predicate compiling_options(list(pred(1))).
compiling_options(C) :- compiling_options_(C, [gpo]).

do_autolibs(BaseDir) :-
	autolibs(BaseDir, ~compiling_options).

autolibs(BaseDir) :-
	set_configured_flags,
	do_autolibs(BaseDir).

allcontrib <- :-
	proc_if_dir_exists(~atom_concat(~component_src(ciao),
		'/contrib/chat80'), makechat80),
	autolibs('contrib/').

makechat80 :-
	lpmake_subdir('contrib/chat80', '../Makefile', '').

alllib <- :-
	autolibs('lib/').

ciao_so_libs <- :-
	tabling.

alllibrary <- :-
	BaseDir = 'library/',
	autolibs(BaseDir),
	atom_concat(BaseDir, 'toplevel/',                   Dir),
	atom_concat(BaseDir, 'toplevel/toplevel__scope.pl', FileName),
	compile_module_list([m(_, _, FileName)], Dir, ~compiling_options).

clplibs <- :-
	clplibs.

clplibs :-
	autolibs('library/clpq/'),
	autolibs('library/clpr/').

miniprolog <- :-
	do_miniprolog.

miniprolog_cmd := all|bench|estimate.

do_miniprolog :-
	bold_message("Compiling mini prolog engine"),
	command_option(CommandOption),
	get_platform(Platform),
	(
	    miniprolog_cmd(Cmd),
	    make_subdir(~lpmake, ~gmake, 'contrib/miniprolog/miniprolog',
		'', ~atom_concat(['-s -j1 MPARCH=', Platform, ' ', Cmd]),
		CommandOption),
	    fail
	;
	    true
	),
	copy_file('contrib/miniprolog/miniprolog/bin/miniprolog_auto.pl',
	    'contrib/miniprolog/miniprolog_pre.pl', [overwrite]),
	copy_mp_auto.

profiler <- :- profiler.

profiler :- autolibs('contrib/profiler/').

% This extra step is to ensure the generation of miniprolog_auto.pl
% even if miniprolog has not been configured
copy_mp_auto :-
	copy_file('contrib/miniprolog/miniprolog_pre.pl',
	    'contrib/miniprolog/miniprolog_auto.pl', [overwrite]).


benchmp <- :- benchmp.

benchmp :-
	bold_message("Running mini prolog benchmarks"),
	make_subdir(~lpmake, ~gmake, 'contrib/miniprolog/miniprolog', '',
	    ~atom_concat(['-s MPARCH=', ~get_platform, ' bench']),
	    ~command_option).

estimatemp <- :-
	estimatemp.

estimatemp :-
	bold_message("Running mini prolog benchmarks"),
	make_subdir(~lpmake, ~gmake, 'contrib/miniprolog/miniprolog', '',
	    ~atom_concat(['-s MPARCH=', ~get_platform, ' estimate']),
	    ~command_option).

chr <- :- chr.
chr :-
	bold_message("Compiling CHR Libraries"),
	do(['cd library/chr ; ', ~setlocalciao, ' CIAOSH=\"', ~ciaosh,
		' -f \" ./do_bootstrap >> ', ~install_log], ~command_option).

alllibrarydyn <- [ciao_so_libs, ciao_special_libs, alllibrary] :-
	true.
alllibrarystat <- [alllibrarydyn] :-
	true.

ciao_special_libs <- :-
	clplibs,
%	chr,
	javalibs.

javalibs <- :- javalibs.

javalibs :-
	(
	    with_java_interface(yes) ->
	    make_subdir(~lpmake, ~gmake, 'library/javall', '', all,
		~command_option)
	;
	    true
	).

tabling <- :- tabling.

tabling :-
	(
%	    We can not compile this if tabled execution is disabled
	    tabled_execution(no) ->
	    true
	;
	    autolibs('contrib/tabling/')
	).


mysql_directory := 'persdb_mysql'.
mysql_directory_op := 'persdb_mysql_op'.

mysqllibs <- :- mysqllibs.
mysqllibs :-
	( with_mysql(yes) ->
	    bold_message("Configuring MySQL Libraries"),
	    replace_strings_in_file([["where_mysql_client_lives",
			~atom_codes(~mysql_client_directory)]],
		~atom_concat(['library/', ~mysql_directory,
			'/linker_opts.pl.skel']),
		~atom_concat(['library/', ~mysql_directory,
			'/linker_opts_auto.pl'])),
	    ( file_exists(~atom_concat(['library/', ~mysql_directory_op])) ->
		replace_strings_in_file([["where_mysql_client_lives",
			    ~atom_codes(~mysql_client_directory)]],
		    ~atom_concat(['library/', ~mysql_directory_op,
			    '/linker_opts.pl.skel']),
		    ~atom_concat(['library/', ~mysql_directory_op,
			    '/linker_opts_auto.pl']))
	    ; true
	    )
	; true
	).

icon_address_auto :-
	try_finally(
	    open(~atom_concat(~component_src(ciao),
		    '/library/pillow/icon_address_auto.pl'),
		write, OS),
	    portray_clause(OS, icon_base_address(~webimagesurl)),
	    close(OS)
	).

% Remove the -L option, hack that allows to run in LINUXi86_64 --EMM:

fix_linker_opts(LinkerOpts0, LinkerOpts) :-
	get_platform('LINUXi86_64'),
	append("-L"||_, " "||LinkerOpts, LinkerOpts0),
	!.
fix_linker_opts(LinkerOpts, LinkerOpts).

mathlibs <- :- mathlibs.
mathlibs :-
	(
	    with_gsl(yes) ->
	    bold_message("Configuring Math library (Using GSL)"),
	    S = ":- use_module(library(math(gsl_imports))).\n",
	    do_str([~gsl_config_base, ' --cflags'],
		~command_option, CompilerOptsN),
	    append(CompilerOpts, "\n", CompilerOptsN),
	    do_str([~gsl_config_base, ' --libs'],
		~command_option, LinkerOptsN),
	    append(LinkerOpts0, "\n", LinkerOptsN),
	    fix_linker_opts(LinkerOpts0, LinkerOpts),
	    T = ~flatten([
		    ":- extra_compiler_opts(\'"||CompilerOpts, "\').\n"||
		    ":- extra_linker_opts(\'"||LinkerOpts, "\').\n"]),
	    string_to_file(T, ~atom_concat(~component_src(ciao),
		    '/contrib/math/gsl_imports_decl_auto.pl')),
	    M = ~flatten(["STAT_LIBS="||LinkerOpts, "\n"])
	;
	    LinkerOpts = "",
	    bold_message("Configuring Math library (Ignoring GSL)"),
	    S = ":- use_module(library(math(gsl_imports_dummy))).\n",
	    M = ""
	),
	string_to_file(~flatten("STAT_LIBS="||LinkerOpts),
	    ~atom_concat(~component_src(ciao), '/SETTINGS_GSL')),
	string_to_file(S,
	    ~atom_concat(~component_src(ciao),
		'/contrib/math/gsl_imports_auto.pl')).


ppl_interface <- :- ppl_interface.
ppl_interface :-
	(
	    with_ppl(yes) ->
	    bold_message("Configuring PPL Interface"),
	    do_str([~ppl_config_base, ' --cflags'],
		~command_option, CompilerOptsN1),
	    append(CompilerOpts1, "\n", CompilerOptsN1),
	    do_str([~ppl_config_base, ' --cppflags'],
		~command_option, CompilerOptsN2),
	    append(CompilerOpts2, "\n",          CompilerOptsN2),
	    append(CompilerOpts1, " ",           Tmp),
	    append(Tmp,           CompilerOpts2, CompilerOpts),
	    do_str([~ppl_config_base, ' --ldflags'],
		~command_option, LinkerOptsN),
	    append(LinkerOpts, "\n", LinkerOptsN),
	    T = ~flatten(["%Do not edit generated automatically\n\n",
		    ":- extra_compiler_opts('", CompilerOpts, "').\n",
		    ":- extra_linker_opts('", LinkerOpts, "').\n"]),
	    string_to_file(T, ~atom_concat(~component_src(ciao),
		    '/contrib/ppl/ppl_decl_auto.pl')),
	    Version = ~ppl_version,
	    (
		Version @< [0, 9] ->
		fail
	    ;
		Version @< [0, 10] ->
		ppl_interface_version("0_9"),
		string_to_file("", ~atom_concat(~component_src(ciao),
			'/contrib/ppl/0_10/NOCOMPILE'))
	    ;
		ppl_interface_version("0_10"),
		string_to_file("", ~atom_concat(~component_src(ciao),
			'/contrib/ppl/0_9/NOCOMPILE'))
	    )
	;
	    string_to_file(
		":- module(ppl_auto, []).\n"||
		":-initialization(error('PPL library not installed')).",
		~atom_concat(
		    ~component_src(ciao), '/contrib/ppl/ppl_auto.pl')),
	    string_to_file("", ~atom_concat(~component_src(ciao),
		    '/contrib/ppl/0_9/NOCOMPILE')),
	    string_to_file("", ~atom_concat(~component_src(ciao),
		    '/contrib/ppl/0_10/NOCOMPILE'))
	).

ppl_interface_version(StrVer) :-
	atom_codes(AtmVer, StrVer),
	S = ~flatten([
"%Do not edit generated automatically.\n\n:- module('ppl_auto', _).\n:- reexport(library('ppl/",
		StrVer, "/ppl_ciao')).\n"]
	),
	string_to_file(S, ~atom_concat(~component_src(ciao),
		'/contrib/ppl/ppl_auto.pl')),
	del_file_nofail(~atom_concat(~component_src(ciao),
		~atom_concat('/contrib/ppl/',
		    ~atom_concat(AtmVer, '/NOCOMPILE')))).


% ============================================================================
% GENERATION OF DOCUMENTATION
% ============================================================================

docs <- [] # "Creates documentation files" :-
	docsemacs,
	docsreadmes,
	docsreference.

docsemacs <- [emacs_mode_compile] # "Creation of emacs documentation files" :-
	docsemacs.

docsemacs :-
	make_subdir(~lpmake, ~gmake, '../emacs-mode', ~setlocalciao, docs,
	    ~command_option).

docsreference <- [] # "Creates the reference documentation" :- docsreference.

docsreference :-
	invoke_lpdoc(~atom_concat(~component_src(ciao), '/doc/reference/SETTINGS'), all).

docsreadmes <- [] # "Creation of readme files" :- docsreadmes.

docsreadmes :-
	SrcDir = ~atom_concat([~component_src(ciao), '/doc/common']),
	BuildDocDir = ~build_doc_dir,
	Files = ['INSTALLATION_CIAO',
	         'INSTALLATION_CIAO_Win32',
		 'README_CIAO',
		 'NewUser'-[libroot = ~ciaolibroot, lpdocdir = ~docdir]],
	generate_readme_files(Files, SrcDir, BuildDocDir).

reconfigure <- [bashrc, cshrc, emacs, xemacs] # "Modifies the "||
	".bashrc/.cshrc/.emacs files to let Ciao run from the installed "||
	"lib files" :- true.

unreconfigure <- [uninstall_bashrc, uninstall_cshrc, uninstall_emacs] #
	"Leaves the .bashrc/.cshrc/.emacs file in its original state" :-
	true.

% ============================================================================
% INSTALLATION
% ============================================================================

install <- [justinstall, reconfigure] # "Ciao Installation" :- true.
uninstall <- [justuninstall, unreconfigure] # "Ciao Uninstallation" :- true.

justinstall <- [] :-
	justinstall(~instype).

justinstall(src) :-
	bold_message("Skipping copy of Ciao files"),
	installdoc.
justinstall(ins) :-
	bold_message("Installing Ciao"),
	( mkdir_perm(~buildreallibdir, ~perms)
	-> true
	; show_message(error, "Could not create ~w", [~buildreallibdir]),
	    fail
	),
	installeng,
	installincludes,
	installciaoc,
	installshell,
	installetc,
	installemacsmode,
	installdoc,
	installlib('lib/'),
	-mkdir_perm(~atom_concat([~build_root, ~reallibdir,
		    '/lib/autoconfig/components']), ~perms),
	installlib('library/'),
	installpillow,
	installlib('contrib/'),
	installsrc('examples/', '*.po|*.itf|*~'),
	bold_message("Ciao installation completed").

justuninstall <- :-
	justuninstall(~instype).

justuninstall(src) :-
	bold_message("Skipping deletion of Ciao files"),
	uninstalldoc.
justuninstall(ins) :-
	bold_message("Uninstalling Ciao"),
	uninstalllib('lib/'),
	uninstalllib('library/'),
	uninstalllib('contrib/'),
	uninstalllib('examples/'),
	uninstallciaoc,
	uninstallshell,
	uninstalletc,
	uninstalldoc,
	uninstallemacsmode,
	uninstallincludes,
	delete_dir_rec(~buildreallibdir),
	--delete_directory(~atom_concat(~build_root, ~libdir)),
	bold_message("Ciao uninstallation completed").

% (for instype=ins and instype=src)
installdoc <- :- installdoc.
installdoc :-
	bold_message("Installation of documentation files"),
	install_docdir(ciao).

% (for instype=ins and instype=src)
uninstalldoc :-
	bold_message("Uninstallation of documentation files"),
	uninstall_docdir(ciao).

installlib(LibName) :-
	installsrc(LibName, '*~').

installsrc(DirName, Exclude) :-
	build_root(BuildRoot),
	atom_concat([~reallibdir, '/', DirName], StdLibDir),
	atom_concat(BuildRoot, StdLibDir, BuildStdLibDir),
	bold_message("Installing ~w ~w libraries in ~w",
	    [~basemain, DirName, StdLibDir]),
	copy_dir_rec(DirName, BuildStdLibDir, ~perms, '*', Exclude, '.svn',
	    '', ~noinstall_dirs, [overwrite, timestamp]).

uninstalllib(LibName) :-
	atom_concat([~buildreallibdir, '/', LibName], BuildStdLibDir),
	delete_dir_rec(BuildStdLibDir).

% (only for instype=ins)
installeng :-
	make_subdir(~lpmake, ~gmake, '.', ~setlocalciao, installeng,
	    ~command_option).

% (only for instype=ins)
installincludes :-
	make_subdir(~lpmake, ~gmake, '.', ~setlocalciao, installincludes,
	    ~command_option).

% (only for instype=ins)
uninstallincludes :-
	bold_message("Uninstallation of C include files for ~w",
	    [~get_platform]),
	delete_dir_rec(~atom_concat(~build_root, ~installedincludedir)),
	del_file_nofail(~atom_concat([~build_root, ~includeroot,
		    '/ciao_prolog.h'])).

% (only for instype=ins)
installciaoc :-
	bold_message("Installation of the compiler"),
	make_subdir(~lpmake, ~gmake, ciaoc, ~setlocalciao, install,
	    ~command_option).

% (only for instype=ins)
uninstallciaoc :-
	bold_message("Uninstallation of the compiler"),
	make_subdir(~lpmake, ~gmake, ciaoc, ~setlocalciao, uninstall,
	    ~command_option).

% (only for instype=ins)
installshell :-
	bold_message("Installation of the top level shell"),
	make_subdir(~lpmake, ~gmake, shell, ~setlocalciao, install,
	    ~command_option).

% (only for instype=ins)
uninstallshell :-
	bold_message("Uninstallation of the top level shell"),
	make_subdir(~lpmake, ~gmake, shell, ~setlocalciao, uninstall,
	    ~command_option).

% (only for instype=ins)
installetc :-
	bold_message("Installation of applications in etc"),
	lpmake_subdir(etc, makedir_part_ciao_etc, install).

% (only for instype=ins)
uninstalletc :-
	bold_message("Uninstallation of applications in etc"),
	lpmake_subdir(etc, makedir_part_ciao_etc, uninstall).

% (only for instype=ins)
installemacsmode :-
	bold_message("Installation of graphical env (emacs mode)"),
	( (install_emacs_support(yes) ; install_xemacs_support(yes)) ->
	    make_subdir(~lpmake, ~gmake, '../emacs-mode', '', install,
		~command_option)
	; true
	).

% (only for instype=ins)
uninstallemacsmode :-
	bold_message("Uninstallation of the graphical env (emacs mode)"),
	make_subdir(~lpmake, ~gmake, '../emacs-mode', '', 'uninstall',
	    ~command_option).

:- doc(bug, "The current installation method cannot uninstall the
   installed pillow images").

% (only for instype=ins)
installpillow :-
	webimagespath(WebImagesPath),
	build_root(BuildRoot),
	atom_concat(BuildRoot, WebImagesPath, BuildWebImagesPath),
	bold_message("Installing PiLLoW images in ~w", [WebImagesPath]),
	mkdir_perm(BuildWebImagesPath, ~perms),
	copy_dir_rec('library/pillow/images/', BuildWebImagesPath, ~perms,
	    '*', '*~', '.svn', '', ~noinstall_dirs, [overwrite, timestamp]).

% ============================================================================
% SCRIPT CONFIGURATION
% ============================================================================

% ----------------------------------------------------------------------------
% BASH
% ----------------------------------------------------------------------------
bashrc <- :-
	update_bashrc(yes) ->
	(-configure_script(~dotbashrc, "#", bashrc_lines))
    ; true.

uninstall_bashrc <- :-
	update_bashrc(yes) ->
	(-uninstall_script(~dotbashrc, "#"))
    ; true.

bashrc_lines(Key) -->
	start_message("#", Key),
	bashrc_content,
	end_message("#", Key).

bashrc_content -->
	"if [ -f ", libroot, "/DOTprofile ] ; then\n"||
	"  . ", libroot, "/DOTprofile\n"||
	"fi\n".
% ----------------------------------------------------------------------------
% CSH
% ----------------------------------------------------------------------------
cshrc <- :-
	( update_cshrc(yes) ->
	    (-configure_script(~dotcshrc, "#", cshrc_lines))
	; true
	).

uninstall_cshrc <- :-
	( update_cshrc(yes) ->
	    (-uninstall_script(~dotcshrc, "#"))
	; true
	).

cshrc_lines(Key) -->
	start_message("#", Key),
	cshrc_content,
	end_message("#", Key).

cshrc_content -->
	"if ( -e ", libroot, "/DOTcshrc ) then\n"||
	"  source ", libroot, "/DOTcshrc\n"||
	"endif\n".
% ----------------------------------------------------------------------------
% EMACS
% ----------------------------------------------------------------------------
emacs <- :-
	( install_emacs_support(yes), update_dotemacs(yes) ->
	    (-configure_script(~dotemacs, ";", emacs_lines))
	; true
	).

uninstall_emacs <- :-
	( install_emacs_support(yes), update_dotemacs(yes) ->
	    (-uninstall_script(~dotemacs, ";"))
	; true
	).

xemacs <- :-
	( install_xemacs_support(yes), update_dotxemacs(yes) ->
	    (-configure_script(~dotxemacs, ";", emacs_lines))
	; true
	).

emacs_lines(Key) -->
	start_message(";", Key),
	emacs_content,
	end_message(";", Key).

emacs_content -->
	"(if (file-exists-p \"", libemacs, "\")\n"||
	"(load-file \"", libemacs, "\")\n"||
	")\n".
% ----------------------------------------------------------------------------
% COMMON STRINGS
% ----------------------------------------------------------------------------
libroot(S, T) :-
	append(~atom_codes(~ciaolibsrc), T, S).

libemacs(S, T) :-
	append(~atom_codes(~ciaolibemacs), T, S).

ciaolibsrc(LibRoot) :-
	( instype(src) ->
	    atom_concat(~component_src(ciao), '/etc', LibRoot)
	; atom_concat(~ciaolibroot, '/ciao', LibRoot)
	).

ciaolibemacs(LibEmacs) :-
	( instype(src) ->
	    atom_concat(~component_src(ciaode),
		'/emacs-mode/ciao-mode-init.el', LibEmacs)
	; atom_concat([~ciaolibroot, '/ciao/', ~versionmain,
		    '/ciao-mode-init.el'], LibEmacs)
	).

runtests <- [unittests, isotests, ciaotests] :- true.

unittests <- [] # "Run Ciao unit tests" :-
	bold_message("Running Ciao tests"),
	run_test_dir(~atom_concat(~component_src(ciao), '/lib'),     []),
	run_test_dir(~atom_concat(~component_src(ciao), '/library'), []),
	run_test_dir(~atom_concat(~component_src(ciao), '/contrib'), []).

ciaotests <- [] # "Run Ciao tests" :-
	proc_if_dir_exists(~atom_concat(~component_src(ciao), '/tests'),
	    do_ciaotests).

do_ciaotests :-
	working_directory(ThisDir, ThisDir),
	working_directory(_,       tests),
	use_module(ciaosrc(tests(run_tests)), [run_tests/0]),
	_:run_tests,
	working_directory(_, ThisDir).

isotests <- [] # "Run ISO-prolog tests" :-
	atom_concat(~component_src(ciao), '/contrib/iso_tests', IsoTestsDir),
	proc_if_dir_exists(IsoTestsDir, do_isotests(IsoTestsDir)).

do_isotests(IsoTestsDir) :-
	bold_message("Running ISO-prolog tests"),
	run_test_dir(IsoTestsDir, []).

ecrc := ecrc.

runbenchmarks <- [] # "Run Benchmarks" :-
	use_module(ciaosrc(library(benchmarks(~ecrc))), [main/1]),
	~ecrc:main([]).
