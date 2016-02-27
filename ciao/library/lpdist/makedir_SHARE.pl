% makedir_SHARE.pl (include file)

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(make(make_rt))).
:- use_module(library(system_extra)).
:- use_module(library(dirutils)).

:- use_module(library(lpdist(readme_generator))).
:- use_module(library(lpdist(makedir_aux))).
:- use_module(library(lpdist(ciao_bundle_db))).
:- use_module(library(lpdist(ciao_config_db))).
:- use_module(library(lpdist(ciao_config_options))).
:- use_module(library(lpdist(ciao_configure))).
:- use_module(library(lpdist(distutils)), [
% 	bundle_make/2,
	make_subdir/7,
	lpmake_subdir/3,
 	current_dir_module/4, build_mods/2,
 	register_in_script/3, unregister_from_script/2,
 	compile_module_list/3,
	list_filter_files_rec/7,
	compile_modules/3]).

% Temporal solution to share code from bundle's makedir

% INTERFACE OF THIS TEMPLATE

% (Rule)
%   bundle_build <- ... # "bundle 'build' command"
% 
% bundle_id(Id) # "@var{Id} is the identifier of the bundle".
% bundle_dname(X) # "@var{X} is the textual name of the bundle".
% bundle_readme_dir(D) # "@var{D} is the subdirectory where the
%   READMEs are found (in lpdoc format)".
% bundle_readme(X) # "@var{X} is a file description (see code for
%   more details) of each README".
% bundle_manual_dir(D) # "@var{D} is the subdirectory where each
%   manual is found (containing a SETTINGS.pl file)".

% ---------------------------------------------------------------------------

% TODO: Is this necessary? (JFMC)
defaults <- [] # "Preventing lpmake from being called without target" :- true.

% ---------------------------------------------------------------------------
% Bundle registry
% TODO: post-installation? activation?

:- use_module(library(bundle_registry(bundle_scan)),
	[install_ins_bundle_cfg/3, uninstall_ins_bundle_cfg/2]).

% hooks:
%   bundle_register_hook/0
%   bundle_unregister_hook/0

bundle_register <- [] :-
	bundle_register_ins_reg(~instype),
	bundle_register_hook.

bundle_unregister <- [] :-
	bundle_unregister_hook,
	bundle_unregister_ins_reg(~instype).

bundle_register_ins_reg(local).
bundle_register_ins_reg(global) :-
	( bundle_ins_reg ->
	    % (bundle cfg, containing alias paths)
	    bundle_id(Bundle),
	    install_ins_bundle_cfg(Bundle,
	                           ~fsR(rootprefix(bundle_inslib(ciao))),
				   ~fsR(bundle_inslib(Bundle)))
	; true
	).

bundle_unregister_ins_reg(local).
bundle_unregister_ins_reg(global) :-
	( bundle_ins_reg ->
	    % (bundle cfg, containing alias paths)
	    bundle_id(Bundle),
	    uninstall_ins_bundle_cfg(Bundle, ~fsR(rootprefix(bundle_inslib(ciao))))
	; true
	).

% ---------------------------------------------------------------------------

% Install the module collection under DirName (along compiled files)
install_lib(DirName) :-
	bundle_id(Bundle),
	bold_message("Installing (~w) '~w' libraries", [Bundle, DirName]),
	b_install_dir_rec(DirName, ~fsR(bundle_inslib(Bundle)/(DirName))).

% Uninstall the previously installed module collection DirName
uninstall_lib(DirName) :-
	bundle_id(Bundle),
	b_uninstall_dir_rec(~fsR(bundle_inslib(Bundle)/(DirName))).

% Install the module collection under DirName (just source, e.g., for examples)
install_src(DirName) :-
	bundle_id(Bundle),
	bold_message("Installing (~w) '~w' source", [Bundle, DirName]),
	b_installsrc_dir_rec(DirName, ~fsR(bundle_inslib(Bundle)/(DirName))).

% Uninstall the previously installed source-only module collection DirName
uninstall_src(DirName) :-
	bundle_id(Bundle),
	b_uninstallsrc_dir_rec(~fsR(bundle_inslib(Bundle)/(DirName))).

% ---------------------------------------------------------------------------
% Compilation 
% TODO: and more...

build <- [build_message,
	  bundle_build,
	  build_done_message] :- true.

build_message <- :-
	bundle_dname(Bundle),
	bold_message("Building ~w", [Bundle]).

build_done_message <- :-
	bundle_dname(Bundle),
	bold_message("~w build completed", [Bundle]).

% ---------------------------------------------------------------------------
% Installation

install <- [bundle_install, bundle_register] :- true.
uninstall <- [bundle_uninstall, bundle_unregister] :- true.

% ---------------------------------------------------------------------------
% Documentation

docs_readmes <- [] # "Creation of README files" :- docs_readmes.
docs_readmes :-
	bundle_readme_dir(SrcDir), !,
	BuildDocDir = ~build_doc_dir,
	findall(File, bundle_readme(File), Files),
	generate_readme_files(Files, SrcDir, BuildDocDir).
docs_readmes. % no readmes

:- use_module(library(lpdist(ciao_config_options)), [build_doc_dir/1]).

docs_manuals <- [] # "Creates the manuals." :-
	docs_manuals.
docs_manuals :-
	( % (failure-driven loop)
	  bundle_manual_dir(SrcDir),
	    invoke_lpdoc(~fsR(SrcDir/'SETTINGS'), all),
	    fail
	; true
	).

% TODO: already covered by fsR?
compose_dir(Base, Subdir, Dir) :-
	( Subdir = '' ->
	    Dir = Base
	; Dir = ~fsR(Base/Subdir)
	).

% ---------------------------------------------------------------------------

%:- doc(section, "Generation of TAGS (for find-tag emacs command)").
% TODO: It should not generate tags for the 'whole' bundle

% TODO: Make sure that it works
tags <- [] # "Creation of TAGS for use with the find-tag command "||
	"(ESC-.) in emacs" :-
        create_tags_file(~bundle_id).

deltags <- [] # "Deletion of TAGS file" :-
	delete_tags_file(~bundle_id).

% ===========================================================================
