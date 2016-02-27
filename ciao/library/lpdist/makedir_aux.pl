:- module(_, [], [assertions, fsyntax, hiord]).

:- doc(title,  "Auxiliary Definitions for makedir").
:- doc(author, "Edison Mera (original author)").
:- doc(author, "Jos@'{e} F. Morales").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lpdist(distutils)), [make_subdir/5]).
:- use_module(library(lpdist(ciao_bundle_db))).
:- use_module(library(lpdist(ciao_config_options)), [
	perms/1, ciaolib_root/1, buildbin_dir/1, buildbin_lpdoc/1, gmake/1,
	setlocalciao/1, command_option/1, stop_if_error/1, rootprefix/1,
	docformat/1, docformatdir/2]).
:- use_module(library(bundle_registry(bundle_registry_load)), [bundle_src/2, bundle_ins/2]).

% ===========================================================================

:- doc(section, "Symbolic File Names (for build and installation)").

:- export(fsR/2).
:- pred fsR(Path, AbsFile) # "@var{AbsFile} is the (absolute) file
   system path assigned to the symbolic file or directory name
   @var{Path}".

fsR(A) := A :- atom(A), !.
fsR(A) := _ :- var(A), !, throw(bad_fsR(A)). % TODO: fix exception
fsR(sys_dir(build)) := R :- !,
	R = ~fsR(bundle_src(ciaode)/build).
fsR(rootprefix(A)) := R :- !,
	R = ~atom_concat(~rootprefix, ~fsR(A)).
fsR(bundle_src(Bundle)) := R :- !,
	R = ~bundle_src(Bundle).
fsR(bundle_ins(Bundle)) := R :- !,
	R = ~bundle_ins(Bundle).
fsR(concat_ver(Bundle, B)) := R :- !,
	R = ~concat_ver(Bundle, ~fsR(B)).
fsR(concat_verk(Bundle, Kind, B)) := R :- !,
	R = ~concat_k(Kind, ~fsR(concat_ver(Bundle, B))).
fsR(concat_k(Kind, B)) := R :- !,
	R = ~concat_k(Kind, ~fsR(B)).
%
% (only for 'ins' installation type)
fsR(bundle_inslib(Bundle)) := R :- !,
	R = ~fsR(concat_ver(Bundle, ~ciaolib_root/(Bundle)/(Bundle))).
% (only for 'ins' installation type)
fsR(bundle_insbaselib(Bundle)) := R :- !,
	R = ~fsR(~ciaolib_root/(Bundle)).
%
fsR(A/B) := ~concat_dir(~fsR(A), B) :- atom(B), !.
fsR(A) := _ :- throw(bad_fsR(A)). % TODO: fix exception

concat_dir(Dir, '') := Dir :- !. % TODO: document
concat_dir(Dir, File) := ~atom_concat([Dir, '/', File]).

% Obtain 'A-Ver' where Ver is the version of Bundle.
% TODO: bundle_version is very SLOW, fix it (e.g., cache its value)
concat_ver(Bundle, A) := ~atom_concat([A, '-', ~bundle_version(Bundle)]).

% Obtain 'A'+'Ext' where Ext is the default extension (For the current
% architecture) for each kind:
%
%  - 'plexe': ciao executables
%  - 'exec': native executables
%  - 'shscript': shell scripts
%  - 'ext(Ext)': custom extension 'Ext'
%
concat_k(plexe, X) := ~atom_concat(X, ~get_ciao_ext).
concat_k(exec, X) := ~atom_concat(X, ~get_exec_ext).
concat_k(shscript, X) := X.
concat_k(ext(Ext), X) := ~atom_concat(X, Ext).

:- export(fsRconcat/3).
:- pred fsRconcat(A, B, C)
   # "@var{C} is the concatenation of base path @var{A} and relative @var{B}.".
fsRconcat(A, '.') := A :- !.
fsRconcat(A, B/C) := ~fsRconcat(A, B)/C :- !.

% ===========================================================================

:- doc(section, "Invokation of Some Tools as Separate Processes").

:- export(invoke_lpdoc/2).
invoke_lpdoc(ConfigFile, Options) :-
	invoke_lpdoc(~atom_concat([' -d stop_if_error=', ~stop_if_error,
		    ' -f ', ConfigFile, ' ', Options])).

:- export(invoke_lpdoc/1).
invoke_lpdoc(Options) :-
	make_subdir(~buildbin_lpdoc, ~build_doc_dir, ~setlocalciao, Options, ~command_option).

:- use_module(library(lpdist(ciao_config_options)), [build_doc_dir/1]).

:- export(invoke_gmake/2).
invoke_gmake(Dir, Target) :-
	make_subdir(~gmake, Dir, '', Target, ~command_option).

% TODO: required?
:- export(invoke_gmake_localciao/2).
% (like invoke_gmake, but with the enviroment set for the local ciao)
invoke_gmake_localciao(Dir, Target) :-
	make_subdir(~gmake, Dir, ~setlocalciao, Target, ~command_option).

% ---------------------------------------------------------------------------
% Invocation of external/nested build/installation code

:- use_module(library(lpdist(ciao_config_options)), [gmake/1, build_log/1, ciaosh/1]).
:- use_module(library(system_extra), [do/2]).

% % custom shell command with environment predefined for ciaosh execution
% % TODO: for CHR bootstrap, really necessary?
% :- export(invoke_customsh/2).
% invoke_customsh(Dir, Cmd) :-
% 	do(['cd ', Dir, '; ',
% 	    ~setlocalciao, ' CIAOSH=\"', ~ciaosh, ' -f \" ',
% 	    './', Cmd, ' >> ', ~build_log], ~command_option).

:- export(invoke_ciaosh/1).
invoke_ciaosh(Input) :-
	do([~setlocalciao, ' ', ~ciaosh, ' -f < ', Input, ' >> ',
	    ~build_log], ~command_option).

% ---------------------------------------------------------------------------

:- use_module(library(lpdist(ciao_config_options)), [emacs_path/1]).
:- use_module(library(system_extra), [do/4]).

% TODO: Use 'append' creation mode for logs? (use a single log
:- export(emacs_batch_call/3).
emacs_batch_call(Dir, LogName, Args) :-
	do(['unset SHELL ; ',
 	    'unset EMACSLOADPATH ; ',
 	    'unset EMACSDOC ; ',
	    'cd \"', ~fsR(Dir), '\" ;',
	    '\"', ~emacs_path, '\" -batch ', Args],
	    ~fsR(concat_k(ext('.log'), Dir/LogName)),
	    ~fsR(concat_k(ext('.err'), Dir/LogName)),
	    [show_error_on_error, show_output_on_error, nofail]).

% ---------------------------------------------------------------------------

:- export(clean_log/2).
% Clean log files
clean_log(Dir, LogName) :-
	del_file_nofail(~fsR(concat_k(ext('.log'), Dir/LogName))),
	del_file_nofail(~fsR(concat_k(ext('.err'), Dir/LogName))).

% ===========================================================================

:- doc(section, "Generation of TAGS (for find-tag emacs command)").

:- use_module(library(lpdist(distutils)), [list_filter_files_rec/7]).
:- use_module(library(system_extra), [etags/2]).
:- use_module(library(system), [copy_file/2]).

% TAGS file for the given Bundle
tags_file(Bundle) := ~fsR(bundle_src(Bundle)/'TAGS').

% Creation of TAGS for use with the find-tag command (ESC-.) in emacs
:- export(create_tags_file/1).
create_tags_file(Bundle) :-
	TagsFile = ~tags_file(Bundle),
	del_file_nofail(TagsFile),
	% Combine the TAGS file with that of other bundles
	% TODO: why is necessary? should this be part of the emacs mode?
	% TODO: generalize to follow bundle dependencies?
	( Bundle = ciaopp ->
	    % (for Ciaopp, copy tags from Ciao)
	    CiaoTags = ~tags_file(ciao),
            ( file_exists(CiaoTags) -> (-(copy_file(CiaoTags, TagsFile))) ; true )
        ; true
        ),
	%
 	BundleSrc = ~fsR(bundle_src(Bundle)),
	list_filter_files_rec(BundleSrc, '*.pl', '', '', '', [], List),
	etags(List, TagsFile).

:- export(delete_tags_file/1).
delete_tags_file(Bundle) :-
	del_file_nofail(~tags_file(Bundle)).

% ===========================================================================

:- export(codes_atom/2).
codes_atom(A, B) :- atom_codes(B, A).

% ===========================================================================

:- use_module(library(system_extra), [mkdir_perm/2]).

:- export(prepare_doc_dirs/0).
% Prepare the build subdirectory where docs are placed during the
% build process.
prepare_doc_dirs :-
 	mkdir_perm(~fsR(sys_dir(build)), ~perms),
 	mkdir_perm(~fsR(sys_dir(build)/doc), ~perms).

% ===========================================================================

:- doc(subsection, "Installing the Documentation of a Bundle").

:- use_module(library(system_extra), [make_dirpath/1]).
:- use_module(library(dirutils)).

:- export(bundle_install_docs/1).
bundle_install_docs(Bundle) :-
	( % (failure-driven loop)
	  docformat(DocFormat),
	    docformatdir(DocFormat, TargetDir),
	    FileName = ~atom_concat([~bundle_manual_base(Bundle), '.', DocFormat]),
	    Source = ~fsR(~build_doc_dir/(FileName)),
	    ( file_exists(Source) ->
		Target = ~atom_concat(TargetDir, FileName),
		( Source == Target ->
		    note(['Skipping copy of ', Target])
		; make_dirpath(TargetDir),
		  copy_file_or_dir(Source, TargetDir, [overwrite, timestamp])
		),
		bundle_install_docs_hook(DocFormat, Target)
	    ;
		warning(['File ', Source, ' not generated yet. Skipping copy'])
	    ),
	    fail
	; true
	).

:- export(bundle_uninstall_docs/1).
bundle_uninstall_docs(Bundle) :-
	( % (failure-driven loop)
	  docformat(DocFormat),
	    docformatdir(DocFormat, TargetDir),
	    FileName = ~atom_concat([~bundle_manual_base(Bundle), '.', DocFormat]),
	    Target = ~atom_concat(TargetDir, FileName),
	    bundle_uninstall_docs_hook(DocFormat, Target),
	    delete_dir_rec(Target),
	    fail
	; true
	).

% These predicates install the 'info' files in info dir.

:- use_module(library(lpdist(info_installer))).

bundle_install_docs_hook(info, Target) :- !,
	dirfile_install_info(~build_doc_dir, Target).
bundle_install_docs_hook(_, _).

bundle_uninstall_docs_hook(info, Target) :- !,
	dirfile_uninstall_info(~build_doc_dir, Target).
bundle_uninstall_docs_hook(_, _).

:- use_module(library(lpdist(ciao_config_options)), [build_doc_dir/1]).

% (THIS CAN BE MOVED TO A MODULE)

:- doc(section, "Operations on Build/Installation Files").

:- use_module(library(system_extra),
	[copy_file/3, del_file_nofail/1, do/2, '-'/1, '--'/1]).
:- use_module(library(dirutils), [delete_dir_rec/1]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Building Files").

:- export(b_copy/4).
b_copy(Kind, Bundle, FromDir, File) :-
	copy_file(~fsR(FromDir/File),
	          ~fsR(concat_verk(Bundle, Kind, ~buildbin_dir/(File))),
		  [overwrite]).

:- export(b_filebuild/4).
b_filebuild(Kind, Bundle, Base, FileBuild) :-
	FileBuild = ~fsR(concat_verk(Bundle, Kind, ~buildbin_dir/(Base))).

:- export(b_link/3).
% 'File' will point to 'File-Ver'
b_link(Kind, Bundle, File) :-
	b_link_as(Kind, Bundle, File, File).

:- export(b_link_as/4).
% 'Dest' will point to 'Src-Ver'
b_link_as(Kind, Bundle, Src, Dest) :-
	From = ~fsR(concat_verk(Bundle, Kind, Src)),
	% TODO: previously Kind was ignored here... verify that it works (i.e., in Win)
	To = ~fsR(concat_k(Kind, ~buildbin_dir/(Dest))),
	create_link(From, To).

% TODO: If installation type is 'src', b_clean_* will also remove the
%       binary files! Are we sure about it?

:- export(b_clean_link/2).
b_clean_link(Kind, File) :-
	del_file_nofail(~fsR(concat_k(Kind, ~buildbin_dir/(File)))).

:- export(b_clean_copy/3).
b_clean_copy(Kind, Bundle, File) :-
	del_file_nofail(~fsR(concat_verk(Bundle, Kind, ~buildbin_dir/(File)))).

:- use_module(library(compiler(exemaker)), [make_exec/2]).
:- use_module(library(lpdist(ciao_config_options)), [set_configured_flags/0,
	ciaoc/1, bootstrap_ciaoc/1]).

% TODO: Do not export
:- export(b_make_exec/4).
% Build the executable for InDir/InFile, store in the build directory
% and select the current version (add a symbolic link).
%
% @var{Opts} indicate the compilation options. The options indicate
% the kind of compiler that is used:
%
%  - 'bootstrap_ciaoc': external bootstrap ciaoc process 
%  - 'ciaoc': external ciaoc process
%  - (default): compiler code built into this process (make_exec/3)
%
% Other options are:
%
%  - 'static': build a static executable (self-contained, so that
%     changes in the compiled system libraries does not affect it)
%
% TODO: CIAOCOPTS is missing (it contains some information about
%       runtime checks)
b_make_exec(Bundle, InDir, InFile, Opts) :-
	b_filebuild(plexe, Bundle, InFile, FileBuild),
	( member(static, Opts) ->
	    CiaoOpts = ' -s'
	; CiaoOpts = ''
	),
	( member(ciaoc, Opts) ->
	    BuildDir = ~fsR(sys_dir(build)),
	    Dir = ~fsR(InDir),
	    In = ~fsR(concat_k(ext('.pl'), InFile)),
	    do(['cd ', Dir, '; ',
	        ~setlocalciao, ' ',
		~ciaoc, ' -x', CiaoOpts, ' -o ', FileBuild, ' ', In], [])
	; member(bootstrap_ciaoc, Opts) ->
	    BuildDir = ~fsR(sys_dir(build)),
	    Dir = ~fsR(InDir),
	    In = ~fsR(concat_k(ext('.pl'), InFile)),
	    do(['cd ', Dir, '; ',
	        ~setlocalciao, ' ',
		~bootstrap_ciaoc, ' -x', CiaoOpts, ' -o ', FileBuild, ' ', In], [])
	; % member(same_process, Opts) ->
	  % Otherwise, build in the same process using make_exec/3
	  set_configured_flags,
	  In = ~fsR(concat_k(ext('.pl'), InDir/InFile)),
	  make_exec([In], FileBuild)
%	; throw(bad_opts_in_b_make_exec(Opts))
	),
	b_link(plexe, Bundle, InFile).

% TODO: we have a problem here: we build some libraries with the
%       bootstrap compiler and others with the compiler that is built
%       into 'bootlpmake'. That may give us a lot of problems.
%
%       If we manage to have .po/.itf in a separate directory, we
%       could just have a toplevel as our bootstrap (that is, a system
%       where dynamically loaded code is possible).

%%     eval ${SETLOCALCIAO} ${BOOTSTRAP_CIAOC} -s -x ${CIAOCOPTS} -o ${CIAOC} ${CIAOCNAME}

% ---------------------------------------------------------------------------

:- doc(subsection, "Lib Installation").

:- use_module(library(lpdist(ciao_config_options)),
	[lib_dir/1, relreallib_dir/1]).

:- export(rl_install_copy_and_link/2).
rl_install_copy_and_link(FromDir, File) :-
	rl_install_copy(FromDir, File),
	rl_install_link(File).

% Install a copy of File in ~fsR(rootprefix(bundle_inslib(ciao)))
:- export(rl_install_copy/2).
rl_install_copy(FromDir, File) :-
	From = ~fsR(FromDir/File),
	To = ~fsR(rootprefix(bundle_inslib(ciao)/(File))),
	-copy_file(From, To, [overwrite]),
	-set_perms(To, ~perms).

% Install a link to File in rootprefix(~lib_dir)
:- export(rl_install_link/1).
rl_install_link(File) :-
	rl_install_link_as(File, File).

% Install a link to Src as Dest in rootprefix(~lib_dir)
:- export(rl_install_link_as/2).
rl_install_link_as(Src, Dest) :-
	From = ~fsR(~compose_relreallib_dir(Src)),
	To = ~fsR(rootprefix(~lib_dir/(Dest))),
	create_link(From, To).

% TODO: This is just for a symbolic link; find a better way (e.g.,
%       computing the difference of absolute path names)
compose_relreallib_dir(X0) := X :-
	Rel = ~relreallib_dir,
	( Rel = '' ->
	    X = X0
	; X = Rel/X0
	).

:- export(rl_uninstall_link/1).
rl_uninstall_link(File) :-
	b_uninstall_file(~lib_dir/(File)).

:- export(rl_uninstall_copy/1).
rl_uninstall_copy(File) :-
	b_uninstall_file(~fsR(bundle_inslib(ciao)/(File))).

:- export(rl_uninstall_copy_and_link/1).
rl_uninstall_copy_and_link(File) :-
	rl_uninstall_link(File),
	rl_uninstall_copy(File).

:- export(r_install_as/2).
r_install_as(File0, Dest0) :-
	File = ~fsR(File0),
	Dest = ~fsR(rootprefix(Dest0)),
	-copy_file(File, Dest, [overwrite]),
	-set_perms(Dest, ~perms).

:- export(r_abslink_as/2).
r_abslink_as(Src0, Dest0) :-
	% TODO: remove line if correct, share
%	Src = ~fsR(rootprefix(Src0)),
	Src = ~fsR(Src0),
	Dest = ~fsR(rootprefix(Dest0)),
	create_link(Src, Dest).

% ---------------------------------------------------------------------------

:- doc(subsection, "Installation from the Build Staging Area").

:- use_module(library(lpdist(skip_settings))).
:- use_module(library(lpdist(ciao_config_options)), [ciaobin_dir/1]).
:- use_module(library(dirutils), [copy_dir_rec/9]).
:- use_module(library(system), [delete_directory/1]).
:- use_module(library(messages), [show_message/3]).

% Creates a directory in the installation area
:- export(b_install_dir/1).
b_install_dir(Dir0) :-
	Dir = ~fsR(rootprefix(Dir0)),
	( mkdir_perm(Dir, ~perms) ->
	    true
	; show_message(error, "Could not create ~w", [Dir]),
	  fail
	).

% (copy all)
:- export(b_install_dir_rec/2).
b_install_dir_rec(FromDir, ToDir) :-
	copy_dir_rec(~fsR(FromDir),
	             ~fsR(rootprefix(ToDir)),
		     ~perms,
		     '*', '*~', '.svn', '', ~noinstall_dirs, [overwrite, timestamp]).

% (copy all except .po and .itf)
:- export(b_installsrc_dir_rec/2).
b_installsrc_dir_rec(FromDir, ToDir) :-
	copy_dir_rec(~fsR(FromDir),
	             ~fsR(rootprefix(ToDir)),
		     ~perms,
		     '*', '*.po|*.itf|*~', '.svn', '', ~noinstall_dirs, [overwrite, timestamp]).

:- export(b_install_copy_and_link/3).
b_install_copy_and_link(Kind, Bundle, File) :-
	b_install_copy(Kind, Bundle, File),
	b_install_link_as(Kind, Bundle, File, File).

:- export(b_install_copy/3).
b_install_copy(Kind, Bundle, File) :-
	From = ~fsR(concat_verk(Bundle, Kind, ~buildbin_dir/(File))),
	To = ~fsR(rootprefix(concat_verk(Bundle, Kind, ~ciaobin_dir/(File)))),
	b_install_dir(~ciaobin_dir),
	del_file_nofail(To),
	copy_file(From, To, [overwrite]),
	-set_exec_perms(To, ~perms).

:- export(b_install_file_exec/2).
b_install_file_exec(From0, To0) :-
	From = ~fsR(From0),
	To = ~fsR(rootprefix(To0)),
	del_file_nofail(To),
	copy_file(From, To, [overwrite]),
	-set_exec_perms(To, ~perms).

:- export(b_install_file_noexec/2).
b_install_file_noexec(From0, To0) :- % TODO: add Kind...
	From = ~fsR(From0),
	To = ~fsR(rootprefix(To0)),
	del_file_nofail(To),
	copy_file(From, To, [overwrite]),
	-set_perms(To, ~perms).

:- export(b_install_link_as/4).
b_install_link_as(Kind, Bundle, Src, Dest) :-
	From = ~fsR(concat_verk(Bundle, Kind, Src)),
	% TODO: previously Kind was ignored in @var{To}, verify that it works (i.e., in Win)
	To = ~fsR(rootprefix(concat_k(Kind, ~ciaobin_dir/(Dest)))),
	create_link(From, To).

:- export(b_install_file_link_as/2).
b_install_file_link_as(From0, To0) :-
	From = ~fsR(From0),
	To = ~fsR(rootprefix(To0)),
	create_link(From, To).

:- export(b_uninstall_link/2).
b_uninstall_link(Kind, File) :-
	b_uninstall_file(concat_k(Kind, ~ciaobin_dir/(File))).

:- export(b_uninstall_copy/3).
b_uninstall_copy(Kind, Bundle, File) :-
	b_uninstall_file(concat_verk(Bundle, Kind, ~ciaobin_dir/(File))).

:- export(b_uninstall_copy_and_link/3).
b_uninstall_copy_and_link(Kind, Bundle, File) :-
	b_uninstall_link(Kind, File),
	b_uninstall_copy(Kind, Bundle, File).

:- export(b_uninstall_file/1).
b_uninstall_file(File) :-
	del_file_nofail(~fsR(rootprefix(File))).

:- export(b_uninstall_dir_rec/1).
b_uninstall_dir_rec(Dir) :-
	% TODO: Add some sanity check here to avoid filesystem havoc
	delete_dir_rec(~fsR(rootprefix(Dir))).

:- export(b_uninstallsrc_dir_rec/1).
b_uninstallsrc_dir_rec(Dir) :-
	% TODO: Add some sanity check here to avoid filesystem havoc
	delete_dir_rec(~fsR(rootprefix(Dir))).

:- export(b_uninstall_dir/1).
b_uninstall_dir(Dir) :-
	-delete_directory(~fsR(rootprefix(Dir))).

:- export(b_uninstall_dir_if_empty/1).
b_uninstall_dir_if_empty(Dir) :-
	--delete_directory(~fsR(rootprefix(Dir))).

% ===========================================================================

:- doc(section, "Instantiating Template Files with Parameters").

:- use_module(library(file_utils), [file_to_string/3]).
:- use_module(library(system_extra),
	[replace_params_in_file/3, del_file_nofail/1, '-'/1]).
:- use_module(library(system_extra), [set_perms/2, set_exec_perms/2]).

% (like write_template, but load in a string)
:- export(load_template/4).
load_template(File, Repl, String, String0) :-
	Temp = ~fsR(concat_k(ext('.tmp'), File)), % TODO: Use a temporal file
	replace_params_in_file(
               Repl,
	       ~fsR(concat_k(ext('.skel'), File)),
	       Temp),
	file_to_string(Temp, String, String0),
	del_file_nofail(Temp).

:- export(write_template/2).
write_template(File, Repl) :-
	replace_params_in_file(Repl,
	    ~fsR(concat_k(ext('.skel'), File)),
	    ~fsR(File)),
	set_perms([~fsR(File)], ~perms).

:- export(wr_template/4).
% TODO: reorder the arguments of this predicate
wr_template(cwd, Dir, File, Repl) :- % TODO: correct?
	replace_params_in_file(Repl,
	    ~fsR(concat_k(ext('.skel'), Dir/File)),
	    File).
wr_template(at(OutDir), Dir, File, Repl) :-
	replace_params_in_file(Repl,
	    ~fsR(concat_k(ext('.skel'), Dir/File)),
	    ~fsR(OutDir/File)).
wr_template(origin, Dir, File, Repl) :-
	replace_params_in_file(Repl,
	    ~fsR(concat_k(ext('.skel'), Dir/File)),
	    ~fsR(Dir/File)).
wr_template(kver(Kind, Bundle), Dir, File, Repl) :-
	replace_params_in_file(Repl,
	    ~fsR(concat_k(ext('.skel'), Dir/File)),
	    ~fsR(concat_verk(Bundle, Kind, ~buildbin_dir/(File)))),
	( kind_exec_perms(Kind) ->
	    -set_exec_perms(~fsR(concat_verk(Bundle, Kind, ~buildbin_dir/(File))), ~perms)
	; true
	).
wr_template(inslib(Bundle), Dir, File, Repl) :-
	replace_params_in_file(Repl,
	    ~fsR(concat_k(ext('.skel'), Dir/File)),
	    ~fsR(rootprefix(bundle_inslib(Bundle)/File))),
	-set_perms(~fsR(rootprefix(bundle_inslib(Bundle)/File)), ~perms).

kind_exec_perms(shscript).

% ===========================================================================

:- use_module(library(make(make_rt)), [normal_message/2]).

:- doc(section, "Tools to Build and (Un)Install Standalone Utilities").
% TODO: build_standalone_list/3 does not receive the kind of utility (K)
%       It can only build Prolog applications. Add plug-ins? Or rewrite
%       the special applications in some that that 'b_make_exec' understand?
%       (see 'Use packages to generate script files' in my TODO list)

% TODO: show the same kind of messages that are used when compiling libraries
:- export(build_standalone_list/3).
build_standalone_list(Bundle, Dir, Ps) :-
	( % (failure-driven loop)
	  member(P0, Ps),
	  n_and_props(P0, P, Props),
	    n_name(Props, Name),
	    % TODO: abstract...
	    ( member(static, Props) ->
	        % make static executable
	        Opts = [static]
            ; Opts = []
	    ),
	    ( member(shscript, Props) ->
	        true % TODO: invoke custom build predicate
	    ; member(bootstrap_ciaoc, Props) ->
	        % use bootstrap ciaoc
	        normal_message("Building '~w' (~s) using bootstrap compiler", [P, Name]),
	        b_make_exec(Bundle, Dir, P, [bootstrap_ciaoc|Opts])
	    ; member(ciaoc, Props) ->
	        % use ciaoc
	        normal_message("Building '~w' (~s)", [P, Name]),
	        b_make_exec(Bundle, Dir, P, [ciaoc|Opts])
	    ; % use make_exec in the same process
	      normal_message("Building '~w' (~s)", [P, Name]),
	      b_make_exec(Bundle, Dir, P, same_process)
	    ),
	    fail
	; true
	).

% TODO: show the same kind of messages that are used when compiling libraries
:- export(install_standalone_list/2).
install_standalone_list(Bundle, Ps) :-
	( % (failure-driven loop)
	  member(P0, Ps),
	  n_and_props(P0, P, Props),
	    n_name(Props, Name),
	    n_kind(Props, K),
	    normal_message("Installing '~w' (~s)", [P, Name]),
	    b_install_copy_and_link(K, Bundle, P),
	    fail
	; true
	).

% TODO: show the same kind of messages that are used when compiling libraries
:- export(uninstall_standalone_list/2).
uninstall_standalone_list(Bundle, Ps) :-
	( % (failure-driven loop)
	  member(P0, Ps),
	  n_and_props(P0, P, Props),
	    n_name(Props, Name),
	    n_kind(Props, K),
	    normal_message("Uninstalling '~w' (~s)", [P, Name]),
	    b_uninstall_copy_and_link(K, Bundle, P),
	    fail
	; true
	).

n_and_props(P0, P, Props) :-
	( P0 = P-Props -> true ; P = P0, Props = [] ).

n_name(Props, Name) :-
	( member(name=Name, Props) ->
	    true
	; Name = "stand-alone utility" % default name
	).

n_kind(Props, Kind) :-
	( member(kind=Kind, Props) -> true ; Kind=plexe ).

% ===========================================================================

% (shared)
%:- export(create_link/2).
create_link(From, To) :-
	del_file_nofail(To),
	--copy_file(From, To, [overwrite, symlink]).
        % TODO: do not set perms on a symbolic link (the source may
        %       not exist, at it happens in RPM generation)
%	-set_perms(To, ~perms).

