:- module(_, _, [make, fsyntax]).

:- use_module(library(messages)).
:- use_module(library(system)).
:- use_module(library(make(system_extra))).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(distutils(package_generator))).
:- use_module(library(distutils(package_common))).
:- use_module(library(distutils(package_installer))).
:- use_module(library(distutils)).
:- use_module(library(autoconfig)).

%% ----------------------------------------------------------------------------
:- comment(title, "Default installation makefile for use with lpdoc
   generated doc/manuals").

:- comment(author, "Manuel Hermenegildo").
:- comment(author, "Edison Mera").

:- comment(module, "This module provides the basic options for installing
   documentation that has been generated with lpdoc2.
").

get_package_dir(OutputDir) :-
	check_var_exists(package_root_dir),
	atom_concat(~get_value(package_root_dir), 'package/', OutputDir).

install_website_if_necessary :-
% Website must install a website _only_ if we dont find
% generate.pl
	(
% 	    file_exists(~atom_concat(~get_value(htmldir),
% 		    '/tmpl/generate.pl')) ->
% 	    true
% 	;
	    verbose_message("Installing fresh copy of website templates",
		[]),
% pack_build_root points to the place where the website is defined
	    get_value(pack_build_root, BR),
	    ( atom_concat(BR, 'site', Site), file_exists(Site) ->
		Restore = false
	    ;
		verbose_message("No web-templates found. Installing " ||
		    "default lpdoc templates"),
		Restore = true,
		check_var_exists(lpdoclib),
		set_name_value(pack_build_root, ~get_value(lpdoclib))
	    ),
	    make([install_website]),
	    (Restore = true -> set_name_value(pack_build_root, BR) ; true)
	).

install_website <- [] # "Installs a website skeleton (fresh
	copy). Install_package and generate_website should be executed
	after." :-
	check_var_exists(pack_build_root),
	check_var_exists(htmldir),
	TMPL_basedir = ~atom_concat([~get_value(pack_build_root), 'site/']),
	DistDir = ~get_value(htmldir),
	Owner = ~get_value_or_use_def(owner),
	Group = ~get_value_or_use_def(group),
	Perms = ~get_value_or_use_def(perms),
	install_website(TMPL_basedir, DistDir, Perms, Owner, Group).

install_manuals <- []
# "Install the manuals in the path specified by htmldir" :-
	install_website_if_necessary,

% Kludge: we define this internal variable to "pass" 
% the argument to generate_package
% add_name_value( '$gen_src' , no ),

% pack_build_root points to the place where the package dir is defined
	make([install_package, generate_website]).

generate_website <- [] # "Generates a website for a given template directory"
	:- generate_website.

generate_website :-
	check_var_exists(htmldir),
	check_var_exists(htmlurl),
	TMPL_basedir = ~path_name(~get_value(htmldir)),
	TMPL_dir0 = ~atom_concat(TMPL_basedir, 'tmpl/'),
	( file_exists(TMPL_dir0) ->
	    TMPL_dir = TMPL_dir0
	;
	    TMPL_dir = ~atom_concat([~component_ins(lpdoc),
		    '/lib/site/tmpl/']),
	    verbose_message("no templates were found in htmldir or htmldir "
		|| "not found! Reading template information from " ||
		"lpdoc libs: ~w",
		[TMPL_dir])
	),
	!,
	Owner = ~get_value_or_use_def(owner),
	Group = ~get_value_or_use_def(group),
	HTML_URL = ~path_name(~get_value(htmlurl)),
	DirImages = ~atom_concat(HTML_URL, 'images/'),
	Perms = ~get_value_or_use_def(perms),
	( get_value(htmlurl, WebURL) ->
	    true
	;
	    verbose_message("ATENTION: Assigning htmldir value to " ||
		"htmlurl variable. Webpage would be only " ||
		"referenced locally"),
	    WebURL = TMPL_basedir
	),

	(get_value(extra_tmpl_info, EV) -> true ; EV = []),
	generate_website([fileurl = TMPL_basedir, weburl = WebURL, dirImage =
		DirImages, wr_permisions = Perms, wr_owner = grp(Owner, Group)
		|EV
	    ], TMPL_dir).

uninstall_manuals <- [uninstall_package, generate_website]
# "Uninstall manuals and indices in the path specified by htmldir" :-
	true.

install_docs <- [] # "Install docs in install area" :-
	check_var_exists(docdir),
	check_var_exists(mainfile),
	get_value(mainfile,        MainFile),
	get_value(pack_build_root, BuildRoot),
	Owner = ~get_value_or_use_def(owner),
	Group = ~get_value_or_use_def(group),
	Perms = ~get_value_or_use_def(perms),
	TargetDir = ~atom_concat(BuildRoot, 'package/'),
	( file_exists(TargetDir),
	    file_property(TargetDir, type(directory)) ->
	    install_docs(MainFile, BuildRoot, Perms, Owner, Group)
	;
	    show_message(warning,
		"Documentation has not been generated yet in ~w.",
		[TargetDir])
	).

uninstall_docs <- [] # "Uninstall docs in install area" :-
	check_var_exists(docdir),
	check_var_exists(mainfile),
	get_value(mainfile,        MainFile),
	get_value(pack_build_root, BuildRoot),
	TargetDir = ~atom_concat(BuildRoot, 'package/'),
	( file_exists(TargetDir),
	    file_property(TargetDir, type(directory)) ->
	    uninstall_docs(MainFile, BuildRoot)
	;
	    show_message(warning,
		"Documentation has not been generated yet in ~w.",
		[TargetDir])
	).

install_docs(MainFile, BuildRoot, Perms, Owner, Group) :-
	(
	    get_value(docformat, Doc),
% Currently there is no way of saying that if I install
% index I want to instal infoindex too :(
	    (Doc = info -> (Doc1 = Doc ; Doc1 = infoindex) ; Doc1 = Doc),
	    install_release_file(MainFile, Doc1, BuildRoot,
		Perms, Owner, Group),
	    fail
	;
	    true
	).

uninstall_docs(MainFile, BuildRoot) :-
	(
	    get_value(docformat, Doc),
% Currently there is no way of saying that if I install
% index I want to instal infoindex too :(
	    (Doc = info -> (Doc1 = Doc ; Doc1 = infoindex) ; Doc1 = Doc),
	    uninstall_release_file(MainFile, Doc1, BuildRoot),
	    fail
	;
	    true
	).

%% ----------------------------------------------------------------------------

install_package <- [] # "Install package into a given template directory." :-
	check_var_exists(pack_build_root),
	check_var_exists(htmldir),
	Package_file = ~get_package_dir,
	Owner = ~get_value_or_use_def(owner),
	Group = ~get_value_or_use_def(group),
	Perms = ~get_value_or_use_def(perms),
	HtmlDir = ~get_value(htmldir),
	install_package(Package_file, HtmlDir, Perms, Owner, Group),
	generate_redirect_files(Package_file, HtmlDir, Perms, Owner, Group).

uninstall_package <- []
# "Uninstall package from a given template directory" :-
	check_var_exists(pack_build_root),
	check_var_exists(htmldir),
	Package_file = ~get_package_dir,
	HtmlDir = ~atom_concat(~get_value(htmldir), '/'),
	uninstall_package(Package_file, HtmlDir).

%% ----------------------------------------------------------------------------

install <- [install_docs, install_manuals] #
	"Install manual and indices in install area" :- true.

uninstall <- [uninstall_docs, uninstall_manuals] #
	"Uninstall manual and indices from install area" :- true.

/*
install <- [] # "Install manual and indices in install area" :-
	get_value(pack_build_root, BuildRoot),
	TargetDir = ~atom_concat(BuildRoot, 'package/'),
	( file_exists(TargetDir),
	    file_property(TargetDir, type(directory)) ->
	    make([install_docs, install_manuals])
	;
	    warning(['Documentation has not been generated yet (in ',
		    TargetDir, ').'])
	).

uninstall <- [] # "Uninstall manual and indices from install area" :-
	get_value(pack_build_root, BuildRoot),
	TargetDir = ~atom_concat(BuildRoot, 'package/'),
	( file_exists(TargetDir),
	    file_property(TargetDir, type(directory)) ->
	    make([uninstall_docs, uninstall_manuals])
	;
	    warning(['Documentation has not been generated yet (in ',
		    TargetDir, ').'])
	).
*/
