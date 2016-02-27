:- module(_, _, [ciaopaths, fsyntax, assertions, iso, hiord, define_flag]).

:- doc(title, "Customizable Ciao Options").
:- doc(author, "Edison Mera").

:- doc(module, "This module contains definitions for all customizable
   CIAO options.").

% :- ml(120).
:- use_module(library(terms),         [atom_concat/2]).
:- use_module(library(system)).
:- use_module(library(distutils(dirutils))).
:- use_module(library(make(system_extra)), [do_str/3]).
:- use_module(library(component_registry), [component_src/2]).
:- use_module(library(lists)).
:- use_module(library(messages)).
:- use_module(library(distutils(detcheader))).
:- use_module(ciaodesrc(makedir('ConfigMenu'))).
:- use_module(ciaodesrc(makedir('ConfigValues'))).
:- use_module(ciaodesrc(makedir(makedir_component)), [component_name_version/2]).

:- meta_predicate settings_set_value(goal).

settings_set_value(ValMethod) :-
	call(ValMethod).

% ============================================================================
% Predicates for detecting several default configuration values:
% ============================================================================

def_sys_avail(all).

get_prefix(all,  ins, '/usr/local').
get_prefix(all,  src, ~component_src(ciaode)).
get_prefix(user, ins, ~get_home).
get_prefix(user, src, ~component_src(ciaode)).

get_home(H) :-
	( usersrc(UserSrc) ->
	    absolute_dir_name(~atom_concat('~', UserSrc), H)
	; absolute_dir_name('~', H)
	).

% TODO: where does USERSRC come from? (I have not seen it anywhere)
%       This seems to be an environment variable to allow installation
%       in a user directiory different than the current one. It may
%       have serious problems like not using the right file
%       permissions. It does not seem to be used anywhere.
usersrc(UserSrc) :-
	current_env('USERSRC', UserSrc).

% TODO: useful? move to the libraries?
absolute_dir_name(Dir, AbsDir) :-
	absolute_file_name('', '', '', Dir, _, _, AbsDir).

% absolute_dir_name(Dir, DirN) :-
% 	atom_concat(AbsDir,'/',AbsDir),!.
% absolute_dir_name(Dir, Dir).

get_paths(APath) :-
	getenvstr('PATH', Path),
	extract_paths(Path, PathList),
	member(SPath, PathList),
	atom_codes(APath, SPath).

get_bashrc(all, F) :-
	( member(F, ['/etc/bash.bashrc', '/etc/bashrc']),
	    file_exists(F) ->
	    true
	; F = '/etc/bashrc'
	).
get_bashrc(user) := ~atom_concat(~get_home, '/.bashrc').

get_cshrc(all, F) :-
	( member(F, ['/etc/csh.cshrc', '/etc/tcsh.tcshrc']),
	    file_exists(F) ->
	    true
	; F = '/etc/csh.cshrc'
	).
get_cshrc(user) := ~get_cshrc_name.

get_cshrc_name(C) :-
	get_home(H),
	( member(F, ['/.tcshrc', '/.cshrc']),
	    atom_concat(H, F, C),
	    file_exists(C) ->
	    true
	; atom_concat(H, '/.cshrc', C)
	).

get_dotemacs(user) := ~atom_concat(~get_home, '/.emacs').
get_dotxemacs(user) := ~atom_concat(~get_home, '/.xemacs/init.el').

get_htmldir(all) := '/var/www/html/ciao'.
get_htmldir(user) := ~atom_concat(~get_home, '/public_html/CiaoDE/').

get_htmlurl(all) := '/ciao/'.
get_htmlurl(user) := ~atom_concat(['/~', ~get_pwnam, '/CiaoDE/']).

get_docdir(all,  Prefix,  _HtmlDir) := ~atom_concat(Prefix, '/share/doc/ciao').
get_docdir(user, _Prefix, HtmlDir) := HtmlDir.

get_mandir(all,  Prefix,  _HtmlDir) := ~atom_concat(Prefix, '/share/man').
get_mandir(user, _Prefix, HtmlDir) := HtmlDir.

get_infodir(all,  Prefix,   _HtmlDir) := ~atom_concat(Prefix, '/share/info').
get_infodir(user, _Preifix, HtmlDir) := HtmlDir.

%get_web_images_path( all  ) := '/var/www/html/images/'.
%get_web_images_path( user ) := ~atom_concat( ~get_home,
%	    '/public_html/images/' ).
get_web_images_path(HtmlDir) := ~atom_concat(HtmlDir, '/images').

%get_web_images_url( all  ) := ~get_all_url.
%get_web_images_url( user ) := ~get_user_url.
get_web_images_url(HtmlUrl) := ~atom_concat(HtmlUrl, '/images').

get_all_url := '/images/'.

get_user_url(Url) :-
	( atom_concat(['/~', ~get_pwnam, '/images/'], Url) ->
	    true
	; get_all_url(Url)
	).

verify_emacs(Value) :-
	(emacs_installed -> Value = yes ; Value = no),
	display_option("Emacs available", Value).

verify_xemacs(Value) :-
	(xemacs_installed -> Value = yes ; Value = no),
	display_option("XEmacs available", Value).

verify_svnversion(Value) :-
	(have_svnversion -> Value = yes ; Value = no),
	display_option("svnversion available", Value).

warning_verify_app(App) :-
	warning_message(
	    "~w not installed.  Is is hightly recommended that\n"||
	    "you stop Ciao configuration and install it first."||
	    "It is required for the graphical development environment.",
	    [App]).

possible_emacs_site_start :=
	'/usr/share/emacs/site-lisp/site-start.d'|
	'/etc/emacs/site-start.d'.

emacs_site_start(SiteStart) :-
	possible_emacs_site_start(SiteStart),
	file_exists(SiteStart),
	!.

possible_xemacs_site_start :=
	'/usr/share/xemacs/site-packages/lisp/site-start.d'.

xemacs_site_start(SiteStart) :-
	possible_xemacs_site_start(SiteStart),
	file_exists(SiteStart),
	!.

% update_dotemacs(InsType, VerifyEmacs, UpdateEmacs)

update_dotemacs(all,  no) :- !.
update_dotemacs(user, yes).

update_dotxemacs(all,  no) :- !.
update_dotxemacs(user, yes) :-
	file_exists('~/.xemacs'),
	!.
update_dotxemacs(user, no).

get_emacs_init_dir('all', 'ins', _RealLibDir, Value) :-
	emacs_site_start(Value),
	!.
get_emacs_init_dir(_, _, RealLibDir, RealLibDir).

get_emacs_init_file(all, '65ciao-mode-init.el') :-
% Use if it is debian based
	get_os('LINUX'),
	file_exists('/etc/debian_version'),
	!.
get_emacs_init_file(_, 'ciao-mode-init.el').

get_xemacs_init_dir('all', 'ins', _RealLibDir, Value) :-
	xemacs_site_start(Value),
	!.
get_xemacs_init_dir(_, _, RealLibDir, RealLibDir).

get_xemacs_init_file('ciao-mode-init.el').

display_option_with_help(NameMsg, Value, HelpMsg) :-
	display_option(NameMsg, Value),
	( Value = yes ->
	    true
	; warning($$(HelpMsg))
	).

verify_mysql(Value) :-
	(mysql_installed -> Value = yes ; Value = no),
	display_option_with_help("MySQL available", Value,
	    "MySQL has not been detected.  If you would like to use the\n"||
	    "Ciao-MySQL interface it is highly recommended that you stop\n"||
	    "the Ciao configuration now and install MySQL first.").

verify_gsl(Value) :-
	(gsl_installed -> Value = yes ; Value = no),
	display_option_with_help("GSL available", Value,
	    "GSL has not been detected.  If you want to use the math\n"||
	    "library it is highly recommended that you stop the Ciao\n"||
	    "configuration and install the GSL library first.").

verify_ppl(Value) :-
	( ppl_installed, ppl_version(V) ->
	    Value = yes,
	    message([V])
	; Value = no
	),
	display_option_with_help("PPL >= 0.9 available", Value,
	    "PPL has not been detected.").

verify_java(Value) :-
	(javac_installed, javadoc_installed -> Value = yes ; Value = no),
	display_option_with_help("Javac and Javadoc available", Value,
	    "Java has not been detected. If you would like to use the\n"||
	    "utilities for the Java interface it is highly recommended that\n"||
	    "you stop the Ciao configuration now and install Java first.").

verify_ant(no,  no).
verify_ant(yes, VerifyAnt) :-
	verify_ant_yes(VerifyAnt).

verify_ant_yes(Value) :-
	(ant_installed -> Value = yes ; Value = no),
	display_option_with_help("Ant available", Value,
	    "Ant has not been detected. If you would like to use the\n"||
	    "resource analysis for java, which requires ant, it is\n"||
	    "highly recommended that you stop the Ciao configuration\n"||
	    "now and install Ant first.").

% In the future, the detection of ppl must be implemented here.

emacs_base := emacs.
xemacs_base := xemacs.
gsl_config_base := 'gsl-config'.
ppl_config_base := 'ppl-config'.
svnversion_base := svnversion.

exec_names(App, Exec) :-
	get_exec_ext(Ext),
	Ext \== '',
	atom_concat(App, Ext, Exec).
exec_names(App, App).

emacs_exec := ~exec_names(~emacs_base).
xemacs_exec := ~exec_names(~xemacs_base).
gsl_config_exec := ~exec_names(~gsl_config_base).
ppl_config_exec := ~exec_names(~ppl_config_base).
javac_exec := ~exec_names(javac).
javadoc_exec := ~exec_names(javadoc).
ant_exec := ~exec_names(ant).
svnversion_exec := ~exec_names(~svnversion_base).

find_paths_file(Name, File) :- find_file(Name, _, File).

find_emacs(File) :- find_paths_file(~emacs_exec, File).
find_xemacs(File) :- find_paths_file(~xemacs_exec, File).
find_svnversion(File) :- find_paths_file(~svnversion_exec, File).

emacs_installed :- find_emacs(_).

xemacs_installed :- find_xemacs(_).

have_svnversion :- find_svnversion(_).

mysql_installed :-
	detect_c_headers(['mysql/mysql.h']).

gsl_installed :-
	find_paths_file(~gsl_config_exec, _).

ppl_installed :-
	find_paths_file(~ppl_config_exec, _).

ppl_version(Version) :-
	do_str([~ppl_config_exec, ' --version'], nofail, Str),
	string_to_list_(Str, Version).

string_to_list_(Str, L) :-
	( append(StrH, "." || StrT, Str) ->
	    L = [H|T],
	    number_codes(H, StrH),
	    string_to_list_(StrT, T)
	; L = [H],
	    append(StrH, "\n", Str),
	    number_codes(H, StrH)
	).

javac_installed :- find_paths_file(~javac_exec, _), !,
	is_sun_javac.

% This could be fail prune, so tell me if somebody knows a better way
% to do this. --EMM
is_sun_javac :-
	do_str(['(javac -version 2>&1)'], nofail, String),
	append(_, "javac 1."||_, String),
	!.

ant_installed :- find_paths_file(~ant_exec, _).

locate_file(FileName, FileDir) :-
	do_str(['locate ', FileName], nofail, S),
	atom_codes(FileName, SFileName),
	append(SFileName, "\n",              SFileNameN),
	append(SFileDir,  "/" || SFileNameN, S),
	atom_codes(FileDir, SFileDir),
	!.

locate_files([]).
locate_files([F|Fs]) :-
	locate_file(F, _),
	locate_files(Fs).

javadoc_installed :-
	find_file(~javadoc_exec, _, _).

get_mysql_dir(MySQL) :-
	locate_file('libmysqlclient.a', MySQL),
	!.
get_mysql_dir('/usr/lib/mysql') :-
	warning_message(
	    "Unable to determine where the MySQL client library is " ||
	    "installed.\nCurrent value (/usr/lib/mysql) is only a guess.").

% get_installgroup( InstallGroup ) :-
% 	do_str( [ 'groups 2>/dev/null | cut -f 1 -d \' \'' ],
% 	    nofail, S ),
% 	append( InstallGroupS, "\n", S ),
% 	atom_codes( InstallGroup, InstallGroupS ).

config_entry(ciaosrcsettings, Name, HowToGetValue) :-
	ciao_config_entry(Name, HowToGetValue).

makeop := gmake|make.

get_make(MakeDir, MakeName) :-
	( makeop(MakeName),
	    find_file(MakeName, Path, _) ->
	    atom_concat(Path, '/', MakeDir)
	; error_message("Unable to determine the make utility used."),
	    fail
	).

% Find @var{File} in get_paths/1
find_file(File, '', File) :-
	file_exists(File),
	!.
find_file(File, Path, PathFile) :-
	get_paths(Path),
	atom_concat([Path, '/', File], PathFile),
	file_exists(PathFile),
	!.

get_libdir('src', LibRoot) := LibRoot.
get_libdir('ins', LibRoot) := ~path_concat(LibRoot, 'ciao').

get_relreallibdir('src') := ''.
get_relreallibdir('ins') := ~component_name_version(ciao).

get_reallibdir(LibDir, RelRealLibDir) :=
	~path_concat(LibDir, RelRealLibDir).

% todo: could we unify the relative path for 'ins' and 'src'? (in
%       'src', it is now under 'build')
get_enginedir('src', Prefix) := ~path_concat(Prefix, 'objs').
get_enginedir('ins', Prefix) :=
	~path_list_concat([Prefix, 'lib', 'ciao', ~component_name_version(ciao), 'engine']).

get_final_libroot('src', _Prefix) := ~component_src(ciao).
get_final_libroot('ins', Prefix) := ~path_concat(Prefix, 'lib').

get_final_includeroot('src', Prefix) := ~path_concat(Prefix, 'include').
get_final_includeroot('ins', Prefix) := ~path_concat(Prefix, 'include').

% Where C header files live for this installation type, minus CIAOARCH subdir
% todo: this is SRCINCLUDEDIR, use a single definition!
get_ciaohdir_root('src', Prefix) := ~path_concat(Prefix, 'include').
% note: this is INSTALLEDINCLUDEDIR, use a single definition!
get_ciaohdir_root('ins', Prefix) :=
	~path_list_concat([Prefix, 'lib', 'ciao', ~component_name_version(ciao), 'include']).

get_update_sh('all',  'no').
get_update_sh('user', 'yes').

%get_ppl_interface :=
%	'/usr/local/src/ppl-0.7/interfaces/Prolog/Ciao/ppl_ciao.pl'.

% ----------------------------------------------------------------------------
% Facts defining the menu behavior.
%

% WARNING!!!! Order matters!!! If you use depend_of, the dependent
% variable must be defined after.

% The order in the following clauses is the order of the menu.
% ciao_config_entry(Name, HowToGetValue).
% ----------------------------------------------------------------------------

ciao_config_entry('CIAODESRC', [set_value(~component_src(ciaode))]).
ciao_config_entry('MAIN',      [set_value(~basemain)]).
% ciao_config_entry('ABSSRC',      [set_value(~absolute_dir_name('.'))]).
ciao_config_entry('CONFIGLEVEL',
	    [
		noprevious,
		default('1'),
		valid_values(['1', '2', '3']),
		query(
		    "\nWelcome to the CiaoDE compilation and installation " ||
		    "configuration process.\n"||
		    "You will now be asked some related questions.\n"||
		    "Defaults are given in square brackets. \n"||
		    "Just hit [Enter] to accept the default values.\n\n"||

		    "Please select the configuration level you prefer:\n\n"||
		    "        1 --  Choose default values for all options (recommended).\n"||
		    "        2 --  Configure just a minimum set of options.\n"||
		    "        3 --  Configure an extended set of options.",
		    [default, minimum, extended])
	    ]).
ciao_config_entry('SILENT',
	    [
		default(false),
		valid_values([false, true]),
		query("Removes several messages shown during installation.",
		    [extended])
	    ]).
ciao_config_entry('SYSAVAIL',
	    [
		default(def_sys_avail(SysAvail), SysAvail),
		valid_values(['all', 'user']),
		query(
		    "Select system availability:\n\n"||
		    "        all  --  Make the system available to all users. Typically you\n"||
		    "                 you will need to complete the installation as root.\n"||
		    "                 NOTE: If you select this option, you will not be\n"||
		    "                 able to use the experimental optimizing compiler.\n"||
		    "        user --  Make the system available only for the current user\n"||
		    "                 (configure it in the user\'s home directory).",
		    [minimum, extended]),
		show("System availability is", [default])
	    ]).
ciao_config_entry('INSTYPE',
	    [
		default('ins'),
		valid_values(['ins', 'src']),
		query(
		    "Select the type of installation:\n\n"||
		    "        ins  --  Install the system in a separate location from the\n"||
		    "                 sources and set up things to use the installed version.\n"||
		    "                 The system will not require the sources to run, and \n"||
		    "                 they can be erased after installation.\n"||
		    "                 NOTE: If you select this option, you will not be\n"||
		    "                 able to use the experimental optimizing compiler.\n"||
		    "        src  --  The system will be compiled in, and run from the \n"||
		    "                 sources (this is specially useful for developers).",
		    [minimum, extended]),
		show("Installation type is", [default])
	    ]).
ciao_config_entry('PREFIX',
	    [
		depend_of([('INSTYPE', InsType), ('SYSAVAIL', SysAvail)]),
		set_value(( InsType == 'src',
			component_src(ciaode, Value0),
			atom_concat(Value0, '/build', Value) ),
		    Value),
		default(get_prefix(SysAvail, InsType, DefValue), DefValue),
		query("Specify the directory to perform the installation.",
		    [minimum, extended]),
		show("Prefix is", [default])
	    ]).
ciao_config_entry('EXTRA_CFLAGS', 
            [
	        set_value(get_name_value(cflags, Value), Value),
	        query("Specify additional C compiler flags", [extended]),
                default(true, '')
	    ]).
ciao_config_entry('EXTRA_LDFLAGS', 
            [
	        set_value(get_name_value(ldflags, Value), Value),
	        query("Specify additional linker flags", [extended]),
                default(true, '')
	    ]).
ciao_config_entry('OPTIMIZING_COMPILER',
	    [
		depend_of([('INSTYPE', InsType), ('SYSAVAIL', SysAvail)]),
		valid_values(['yes', 'no']),
		set_value(\+((InsType == 'src', SysAvail == 'user')), no),
		default('no'),
		query(
		    "Specify if you want to install the optimizing compiler.", [
			minimum, extended]),
		show("Install optimizing compiler", [default])
	    ]).
ciao_config_entry('STOP_IF_ERROR',
	    [
		default('yes'),
		valid_values(['yes', 'no']),
		query(
		    "Stop installation if an error is found.",
		    [extended]),
		show("Stop if error", [default])
	    ]).
ciao_config_entry('BINROOT',
	    [
		depend_of([('PREFIX', Prefix)]),
		set_value(atom_concat(Prefix, '/bin', Value), Value),
		show("Executables will go here", [default, minimum, extended])
	    ]).
ciao_config_entry('LIBROOT',
	    [
		depend_of([('PREFIX', Prefix), ('INSTYPE', InsType)]),
		set_value(get_final_libroot(InsType, Prefix, Value), Value),
		show("Libraries will go here", [default, minimum, extended])
	    ]).
ciao_config_entry('INCLUDEROOT',
	    [
		depend_of([('PREFIX', Prefix), ('INSTYPE', InsType)]),
		set_value(get_final_includeroot(InsType, Prefix, Value), Value
		),
		show("Headers will temporarily go here",
		    [default, minimum, extended])
	    ]).
ciao_config_entry('CIAOHDIRROOT',
	    [
		depend_of([('PREFIX', Prefix), ('INSTYPE', InsType)]),
		set_value(get_ciaohdir_root(InsType, Prefix, Value), Value),
		show("Final destination for headers is",
		    [default, minimum, extended])
	    ]).
ciao_config_entry('BINDIR',
	    [
		depend_of([('BINROOT', BinRoot)]),
		set_value(true, BinRoot)
	    ]).
ciao_config_entry('LIBDIR',
	    [
		depend_of([('INSTYPE', InsType), ('LIBROOT', LibRoot)]),
		set_value(get_libdir(InsType, LibRoot, Value), Value)
	    ]).
ciao_config_entry('ENGINEDIR',
	    [
		depend_of([('INSTYPE', InsType), ('PREFIX', Prefix)]),
		set_value(get_enginedir(InsType, Prefix, Value), Value)
	    ]).
ciao_config_entry('UPDATE_BASHRC',
	    [
		depend_of([('SYSAVAIL', SysAvail)]),
		default(get_update_sh(SysAvail, DefValue), DefValue),
		valid_values(['yes', 'no']),
		query(
		    "Set to \"no\" if you do not wish to configure bash to "||
		    "work with Ciao or \nif you wish to configure it by hand.",
		    [minimum, extended]),
		show("Update bash init file", [default])
	    ]).
ciao_config_entry('DOTBASHRC',
	    [
		depend_of([('UPDATE_BASHRC', yes),
			('SYSAVAIL', SysAvail)]),
		default(get_bashrc(SysAvail, DefValue), DefValue),
		query(
		    "The bash initialization file where the Ciao variables are set.",
		    [minimum, extended]),
		show("Bash initialization file", [default])
	    ]).
ciao_config_entry('UPDATE_CSHRC',
	    [
		depend_of([('SYSAVAIL', SysAvail)]),
		default(get_update_sh(SysAvail, DefValue), DefValue),
		valid_values(['yes', 'no']),
		query(
"Set to \"no\" if you do not wish to configure csh/tcsh to work with Ciao or
if you wish to configure it by hand.",
		    [minimum, extended]),
		show("Update csh init file", [default])
	    ]).
ciao_config_entry('DOTCSHRC',
	    [
		depend_of([('UPDATE_CSHRC', yes), ('SYSAVAIL', SysAvail)]),
		default(get_cshrc(SysAvail, DefValue), DefValue),
		query(
		    "The csh/tcsh initialization file where the Ciao variables are set.\n"||
		    "Note that on some systems tcsh reads \"~/.tcshrc\".",
		    [minimum, extended]),
		show("Csh/Tcsh initialization file", [default])
	    ]).
%% ciao_config_entry('VERIFY_EMACS',
%% 	    [
%% 		set_value(verify_emacs(VerifyEmacs), VerifyEmacs)
%% 	    ]).
ciao_config_entry('INSTALL_EMACS_SUPPORT',
	    [
%		depend_of([('VERIFY_EMACS', VerifyEmacs)]),
		valid_values(['yes', 'no']),
%		default(VerifyEmacs),
		default(verify_emacs(VerifyEmacs), VerifyEmacs),
		query(
		    "Set to \"yes\" if you wish to install the Ciao emacs libraries which\n"||
		    "implement the Emacs based graphical environment (highly recommended).\n"||
		    "It should be set to no if emacs is not installed in the system.  It \n"||
		    "is safe to leave as \"yes\" otherwise.", [minimum, extended
		    ]),
		show("Install graph. devel. env. support (Emacs)",
		    [default])
	    ]).
ciao_config_entry('EMACS_FOR_CIAO',
	    [
		depend_of([('INSTALL_EMACS_SUPPORT', yes)]),
		default(find_emacs(DefValue), DefValue),
		query(
		    "The version of emacs that you wish to use with Ciao. The development \n" ||
		    "environment will be compiled for use with this version.",
		    [minimum, extended]),
		show("Emacs version to be used", [default])
	    ]).
ciao_config_entry('UPDATE_DOTEMACS',
	    [
		depend_of([('INSTALL_EMACS_SUPPORT', yes),
			('SYSAVAIL', SysAvail)]),
%, ('VERIFY_EMACS', yes)]),
% This set value means that no question is really asked
		set_value(update_dotemacs(SysAvail, Value), Value),
		default('yes'),
		valid_values(['yes', 'no']),
		query("Set to \"yes\" if you wish to configure emacs to\n"||
		    "work with Ciao (modify emacs initialization file).",
		    [minimum, extended]),
		show("Modify emacs init file", [default])
	    ]).
ciao_config_entry('DOTEMACS',
	    [
		depend_of([('SYSAVAIL', SysAvail), ('UPDATE_DOTEMACS', yes)]),
		default(get_dotemacs(SysAvail, DefValue), DefValue),
		query("Define the emacs initialization file where the "||
		    "Ciao settings will be added.", [minimum, extended]),
		show("Emacs initialization file", [default])
	    ]).
ciao_config_entry('RELREALLIBDIR',
	    [
		depend_of([('INSTYPE', InsType)]),
		set_value(get_relreallibdir(InsType, Value), Value),
		show("RELREALLIBDIR", [default])
	    ]).
ciao_config_entry('REALLIBDIR',
	    [
		depend_of([('LIBDIR', LibDir),
			('RELREALLIBDIR', RelRealLibDir)]),
		set_value(get_reallibdir(LibDir, RelRealLibDir, Value), Value),
		show("REALLIBDIR", [default])
	    ]).
ciao_config_entry('CIAOMODEINITDIR',
	    [
		depend_of([('SYSAVAIL', SysAvail), ('INSTYPE', InsType),
			('INSTALL_EMACS_SUPPORT', yes),
			('REALLIBDIR',            RealLibDir)
		    ]),
		default(get_emacs_init_dir(SysAvail, InsType, RealLibDir,
			Value), Value),
		query(
		    "Specify in what directory you want to copy the emacs ciao mode elisp\n" ||
		    "code in order for it to be accessible to Emacs.", [extended
		    ]),
		show("Emacs init dir", [default, minimum])
	    ]).
ciao_config_entry('EMACSINITFILE',
	    [
		depend_of([('SYSAVAIL', SysAvail),
			('INSTALL_EMACS_SUPPORT', yes)]),
		set_value(get_emacs_init_file(SysAvail, Value), Value),
		query(
		    "Specify the name of the emacs lisp file defining the ciao mode.",
		    []),
		show("Emacs init file", [default, minimum, extended])
	    ]).
%% ciao_config_entry('VERIFY_XEMACS',
%% 	    [
%% 		set_value(verify_xemacs(VerifyXEmacs), VerifyXEmacs)
%% 	    ]).
ciao_config_entry('INSTALL_XEMACS_SUPPORT',
	    [
%		depend_of([('VERIFY_XEMACS', VerifyXEmacs)]),
		valid_values(['yes', 'no']),
%		default(VerifyXEmacs),
		default(verify_xemacs(VerifyXEmacs), VerifyXEmacs),
		query(
		    "Set to \"yes\" if you wish to install the Ciao emacs libraries which\n"||
		    "implement the XEmacs based graphical environment (highly recommended).\n"||
		    "It should be set to no if xemacs is not installed in the system.  It \n"||
		    "is safe to leave as \"yes\" otherwise.", [minimum, extended
		    ]),
		show("Install graph. devel. env. support (XEmacs)",
		    [default]
		)
	    ]).
ciao_config_entry('XEMACS_FOR_CIAO',
	    [
		depend_of([('INSTALL_XEMACS_SUPPORT', yes)]),
		default(find_xemacs(DefValue), DefValue),
		query(
		    "The version of xemacs that you wish to use with Ciao. The development \n" ||
		    "environment will be compiled for use with this version.",
		    [minimum, extended]),
		show("XEmacs version to be used", [default])
	    ]).
ciao_config_entry('UPDATE_DOTXEMACS',
	    [
		depend_of([('INSTALL_XEMACS_SUPPORT', yes),
			('SYSAVAIL', SysAvail)]),
% , ('VERIFY_XEMACS', yes)]),
% This set value means that no question is really asked
		set_value(update_dotxemacs(SysAvail, Value), Value),
		default('yes'),
		valid_values(['yes', 'no']),
		query("Set to \"yes\" if you wish to configure XEmacs to\n"||
		    "work with Ciao (modify xemacs initialization file).",
		    [minimum, extended]),
		show("Modify xemacs init file", [default])
	    ]).
ciao_config_entry('DOTXEMACS',
	    [
		depend_of([('SYSAVAIL', SysAvail), ('UPDATE_DOTXEMACS', yes)]),
		default(get_dotxemacs(SysAvail, DefValue), DefValue),
		query("Define the xemacs initialization file where the "||
		    "Ciao settings will be added.", [minimum, extended]),
		show("XEmacs initialization file", [default])
	    ]).
ciao_config_entry('XEMACSINITDIR',
	    [
		depend_of([('SYSAVAIL', SysAvail), ('INSTYPE', InsType),
			('INSTALL_XEMACS_SUPPORT', yes),
			('REALLIBDIR',             RealLibDir)]),
		default(get_xemacs_init_dir(SysAvail, InsType, RealLibDir,
			Value), Value),
		query(
		    "Specify in what directory you want to copy the xemacs ciao mode elisp\n" ||
		    "file in order for it to be accessible to XEmacs.", [
			extended]),
		show("XEmacs init dir", [default, minimum])
	    ]).
ciao_config_entry('XEMACSINITFILE',
	    [
		depend_of([('INSTALL_XEMACS_SUPPORT', yes)]),
		set_value(get_xemacs_init_file(Value), Value),
		query(
		    "Specify the name of the xemacs lisp file defining the ciao mode.",
		    []),
		show("XEmacs init file", [default, minimum, extended])
	    ]).
ciao_config_entry('HTMLDIR',
	    [
		depend_of([('SYSAVAIL', SysAvail), ('INSTYPE', InsType)]),
		set_value(( InsType == 'src', build_doc_dir(Value) ), Value),
		default(get_htmldir(SysAvail, DefValue), DefValue),
		query("Define this to be the directory in which you wish "||
		    "the documentation \nin html format to be installed. "||
		    "It is recommended that this directoriy \n"||
		    "be accessible via WWW.\n",
		    [minimum, extended]),
		show("HTML manuals will go here", [default])
	    ]).
ciao_config_entry('HTMLURL',
	    [
		depend_of([('SYSAVAIL', SysAvail), ('INSTYPE', InsType)]),
		set_value(( InsType == 'src', build_doc_dir(Value) ), Value),
		default(get_htmlurl(SysAvail, DefValue), DefValue),
		query(
		    "Define the URL to access the previous directory via WWW.",
		    [minimum, extended]),
		show("Broswer will read website as", [default])
	    ]).
ciao_config_entry('DOCDIR',
	    [
		depend_of([('SYSAVAIL', SysAvail), ('INSTYPE', InsType), (
			    'HTMLDIR', HtmlDir), ('PREFIX', Prefix)]),
		set_value(( InsType == 'src', build_doc_dir(Value) ), Value),
		default(get_docdir(SysAvail, Prefix, HtmlDir, DefValue),
		    DefValue),
		query(
		    "Define this to be the directory in which you wish the documentation\n"||
		    "to be installed.\n",
		    [minimum, extended]),
		show("Documentation will go here", [default])
	    ]).
ciao_config_entry('MANDIR',
	    [
		depend_of([('SYSAVAIL', SysAvail), ('INSTYPE', InsType), (
			    'HTMLDIR', HtmlDir), ('PREFIX', Prefix)]),
		set_value(( InsType == 'src', build_doc_dir(Value) ), Value),
		default(get_mandir(SysAvail, Prefix, HtmlDir, DefValue),
		    DefValue),
		query("Define this to be the directory in which you wish "||
		    "the man (unix manual entry) file\n"||
		    "to be installed.\n",
		    [minimum, extended]),
		show("Man entry will go here", [default])
	    ]).
ciao_config_entry('INFODIR',
	    [
		depend_of([('SYSAVAIL', SysAvail), ('INSTYPE', InsType), (
			    'HTMLDIR', HtmlDir), ('PREFIX', Prefix)]),
		set_value(( InsType == 'src', build_doc_dir(Value) ), Value),
		default(get_infodir(SysAvail, Prefix, HtmlDir, DefValue),
		    DefValue),
		query(
		    "Define this to be the directory in which you wish the info file\n"||
		    "installed.  Ideally, this directory should be accesible via emacs.\n",
		    [minimum, extended]),
		show("Info file will go here", [default])
	    ]).
ciao_config_entry('WEB_IMAGES_PATH',
	    [
%	    depend_of( [ ( 'SYSAVAIL', SysAvail ) ] ),
%	    default( get_web_images_path( SysAvail, DefValue ), DefValue ),
		depend_of([('HTMLDIR', HtmlDir)]),
		default(get_web_images_path(HtmlDir, DefValue), DefValue),
		query(
		    "For the PiLLoW Web programming library, define the directory \n"||
		    "(accessible via WWW) where the icons which come with PiLLoW \n"||
		    "(in library/pillow/images) will go.", [minimum, extended]
		),
		show("Pillow images will go here", [default])
	    ]).
ciao_config_entry('WEB_IMAGES_URL',
	    [
%	    depend_of( [ ( 'SYSAVAIL', SysAvail ) ] ),
%	    default( get_web_images_url( SysAvail, DefValue ), DefValue ),
		depend_of([('HTMLURL', HtmlUrl)]),
		default(get_web_images_url(HtmlUrl, DefValue), DefValue),
		query(
		    "Define the URL to access the previous directory via WWW.",
		    [minimum, extended]),
		show("The browser will find the Pillow images at", [default])
	    ]).
ciao_config_entry('INSTALL_PROLOG_NAME',
	    [
		default('yes'),
		valid_values(['yes', 'no']),
		query(
		    "Set to \"yes\" if you wish to create a link that brings up Ciao \n"||
		    "when you type \"prolog\". You may want to say no if there are other\n"||
		    "systems that support the Prolog language in your machine and you\n"||
		    "do not want to make Ciao the default.",
		    [minimum, extended])
	    ]).
ciao_config_entry('WITH_MYSQL',
	    [
		default(verify_mysql(WithMySQL), WithMySQL),
		valid_values(['yes', 'no']),
		query(
		    "Set to \"yes\" if you wish to interface with the MySQL database.\n"||
		    "If you choose to have the MySQL interface, you should have the MySQL\n"||
		    "client part installed in the machine where you are compiling and using\n"||
		    "it.  The MySQL daemon should also be up and running when using the\n"||
		    "interface.", [extended]),
		show("Install MySQL support", [default, minimum])
	    ]).
ciao_config_entry('MYSQL_CLIENT_DIRECTORY',
	    [
		depend_of([('WITH_MYSQL', 'yes')]),
		default(get_mysql_dir(MySQLDir), MySQLDir),
		query(
		    "You should also specify where the MySQL client library is installed.",
		    [minimum, extended]),
		show("MySQL client library path", [default])
	    ]).
ciao_config_entry('WITH_GSL',
	    [
		default(verify_gsl(WithGSL), WithGSL),
%	    default('no'),
		valid_values(['yes', 'no']),
		query(
		    "Set to \"yes\" if you wish to interface with the GSL (GNU Scientific\n"||
		    "Library). If you choose to have the GSL interface, you should have the\n"||
		    "GSL development library installed in the machine where you are\n"||
		    "compiling and using it.", [extended]),
		show("Install GSL support", [default, minimum])
	    ]).
ciao_config_entry('WITH_PPL',
	    [
%		default(verify_ppl(WithPPL), WithPPL),
		default('no'),
		valid_values(['yes', 'no']),
		query(
		    "Set to \"yes\" if you wish to interface with the PPL", [
			extended]),
		show("Install PPL support", [default, minimum])
	    ]).
ciao_config_entry('WITH_JAVA_INTERFACE',
	    [
		default(verify_java(VerifyJava), VerifyJava),
		valid_values(['yes', 'no']),
		query(
		    "Whether you have a reasonably recent version of Java.\n"||
		    "If so the utilities for the Java interface under\n"||
		    "$(CIAOSRC)/library/javall will be compiled , along with\n"||
		    "examples and documentation.",
		    [extended]),
		show("Use Java interface", [default, minimum])
	    ]).
ciao_config_entry('WITH_ANT',
	    [
		depend_of([('WITH_JAVA_INTERFACE', WithJavaInterface)]),
		default(verify_ant(WithJavaInterface, VerifyAnt), VerifyAnt),
		valid_values(['yes', 'no']),
		query(
		    "Verify if ant (a Java based make tool) is installed\n"||
		    "in the system.",
		    [extended]),
		show("Use ant, a Java make tool", [default, minimum])
	    ]).

ciao_config_entry('USE_THREADS',
	    [
		default('yes'),
		valid_values(['yes', 'no']),
		query(
		    "If you wish to compile an engine with threads capability\n"||
		    "(concurrency), set the following variable to \"yes\".  Otherwise, set\n"||
		    "it to \"no\".  If the architecture does not support threads (or\n"||
		    "thread support has not yet been added to Ciao for this\n"||
		    "architecture), this will be automatically disabled at compile time.\n"||
		    "Concurrency support does not cause any appreciable runtime overhead\n"||
		    "for non-concurrent programs, so it is safe to leave it as \"yes\".",
		    [extended])
	    ]).
ciao_config_entry('USE_POSIX_LOCKS',
	    [
		default('no'),
		valid_values(['yes', 'no']),
		query(
		    "When using threads, locks are mandatory, and they do not make any\n"||
		    "sense if you are not using threads.  So, threads enable locks.  Ciao\n"||
		    "includes native code locks for some architectures, but allows\n"||
		    "specifying the use of POSIX-compliant locks if posix libraries are\n"||
		    "available.  Posix locks will be automatically selected if no native\n"||
		    "lock implementation is included in Ciao for a given architecture.  We\n"||
		    "recommend letting this option set to \"no\" since a primitive lock\n"||
		    "implementation is usually much faster than the library-based POSIX\n"||
		    "one.", [extended])
	    ]).
ciao_config_entry('AND_PARALLEL_EXECUTION',
	    [
		default('no'),
		valid_values(['yes', 'visandor', 'no']),
		query(
		    "Set the following variable to \"yes\" if you wish to compile an\n"||
		    "engine with support for and-parallel execution of goals in\n"||
		    "(Herbrand-)independent fashion or to \"visandor\" if you wish also\n"||
		    "support for VisAndOr's events. Choose one of:\n\n"||
		    "        yes             -- Support for and-parallel execution.\n"||
		    "        visandor        -- Support for and-parallel execution and\n"||
		    "                           VisAndOr's events.\n"||
		    "        no              -- No support.", [extended])
	    ]).
ciao_config_entry('PAR_BACK',
	    [
		default('no'),
		valid_values(['yes', 'no']),
		query(
		    "Set the following variable to \"yes\" if you wish to compile an\n"||
		    "engine with support for parallel backtracking execution of goals.\n"||
		    "This feature is experimental and may not be available in all releases.",
		    [
			extended])
	    ]).
ciao_config_entry('TABLED_EXECUTION',
	    [
		default('no'),
		valid_values(['yes', 'no']),
		query(
		    "Set the following variable to \"yes\" if you wish to compile an\n"||
		    "engine with support for tabled execution of goals.\n"||
		    "This feature is experimental and may not be available in all releases.",
		    [
			extended])
	    ]).
ciao_config_entry('COMPILE_CHR',
	    [
		default(no),
		valid_values([yes, no]),
		query(
"Please specify if you would like to compile CHR (adds significant time 
to compilation).",
		    [extended]),
		show("Compile CHR", [default, minimum])
	    ]).
ciao_config_entry('OPTIM_LEVEL',
	    [
		default('optimized'),
		valid_values(['optimized', 'normal']),
		query(
		    "Optimization level used when compiling the bytecode emulator. Choose\n"||
		    "one of:\n"||
		    "\n"||
		    "   optimized       -- Turn on optimization flags\n"||
		    "   normal          -- Normal emulator (non-optimized code)\n"||
		    "\n"||
		    "For normal use, we recommend leaving it as \"optimized\".  But if you\n"||
		    "suspect that your C compiler performs buggy optimizations (which should\n"||
		    "not be the case), turn optimization off.  This can be happen more\n"||
		    "easily in concurrent applicacions: if you write any thread-based\n"||
		    "program and unexpected results appear, try recompiling Ciao without\n"||
		    "optimization options first.", [extended])
	    ]).
ciao_config_entry('EXECMODE',
	    [
		default('775'),
		query("Permissions for installed execs/dirs", [extended])
	    ]).
ciao_config_entry('DATAMODE',
	    [
		default('664'),
		query("Permissions for installed data files", [extended])
	    ]).
ciao_config_entry('INSTALLGROUP',
	    [
		default(''),
		query(
		    "Group for the installed files (empty means use default)",
		    [extended])
	    ]).
% ciao_config_entry('USE_PPL',
% 	    [
% 	    default(verify_ppl(VerifyPPL), VerifyPPL),
% 	    valid_values(['yes', 'no']),
% 	    query("Do you have the Parma Polyhedra Library (PPL) installed "||
% 		"and want to use it?", [extended]),
% 	    show("Using PPL",          [default, minimum])
% 	    ]).
% ciao_config_entry('PPL_INTERFACE',
% 	    [
% 	    depend_of([('USE_PPL', 'yes')]),
% 	    default(get_ppl_interface(PPLInterface), PPLInterface),
% 	    query("Specify the full file name of the PPL-Ciao interface file",
% 		[extended]),
% 	    show("PPL interface file", [default, minimum])
% 	    ]).
ciao_config_entry('COMPILE_CIAOPPCL',
	    [
		default('no'),
		valid_values(['yes', 'no']),
		query(
		    "Please specify if you want to compile the CiaoPP Command Line Utility.",
		    [extended]),
		show("Compile CiaoPP Command line", [default, minimum])
	    ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ciao_config_entry('CROSS_COMPILER_HOST',
	    [
		default('none'),
		query(
		    "If you will cross-compile the engine later, please enter the user\n"||
		    "name and address of the target machine to extract run-time\n"||
		    "characteristics from -- e.g., \"root@my.other.host.com\".  If you\n"||
		    "are not going to crosscompile, leave the default value.\n"||
		    "Cross-compiling is at the moment done with \"make crossengine\" once \n"||
		    "the regular compilation is over.", [extended])
	    ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ciao_config_entry('DEBUG_LEVEL',
	    [
		default('nodebug'),
		valid_values(['nodebug', 'debug', 'profile', 'profile-debug',
			'paranoid-debug']),
		query(
		    "You only want to change this if you are a developer.  Additionally,\n"||
		    "setting the environment variable CIAODEBUG to the value \'-debug\'\n"||
		    "at the time of compiling the engine will override the OPTIM_LEVEL\n"||
		    "and DEBUG_LEVEL flags, and produce non optimized emulator code with\n"||
		    "debugging information.\n"||
		    "\n"||
		    "Level of debugging built into the bytecode emulator. Choose one of:\n"||
		    "\n"||
		    "   nodebug         -- Do not include debug information or messages\n"||
		    "   debug           -- Emulator with C level debugging info available\n"||
		    "                      plus extended C compilation warnings\n"||
		    "   profile         -- Include profiling options for the emulator\n"||
		    "   profile-debug   -- Include profiling and debug options for the\n"||
		    "                      emulator\n"||
		    "   paranoid-debug  -- Emulator with C level debugging info available\n"||
		    "                      plus paranoid C compilation warnings.",
		    [extended])
	    ]).
ciao_config_entry('MAKEDIR',
	    [
		set_value(get_make(Value, _), Value)
	    ]).
ciao_config_entry('MAKENAME',
	    [
		set_value(get_make(_, Value), Value)
	    ]).
%% The .bib files are in the repository (i.e. as SVN external),
%% it makes no sense configuring this. (JFMC)
% ciao_config_entry('BIBFILES',
% 	    [
% 		query(
% 		    "Specifies the bibtex files used to create the bibliography of the\n" ||
% 		    "documentation.", []),
% 		default(
% 		    '/home/clip/bibtex/clip/clip,/home/clip/bibtex/clip/general'
% 		)
% 	    ]).
ciao_config_entry('GEN_CIAO_ASR',
	    [
		default(yes),
		valid_values([yes, no])
	    ]).
ciao_config_entry('GEN_CIAOPP_ASR',
	    [
		default(yes),
		valid_values([yes, no])
	    ]).
ciao_config_entry('HAVE_SVNVERSION',
	    [
		default(verify_svnversion(HaveSvnVersion), HaveSvnVersion),
		valid_values([yes, no])
	    ]).
ciao_config_entry(FlagName,
	    [
		default(Default)
%		query("Set the given Prolog Flag.", [exended])
		|Options]) :-
	define_flag(FlagNameL, Values, Default),
	\+ flag_black_list(FlagNameL),
	( list(Values) -> Options = [valid_values(Values)|FlagOpts]
	; Options = FlagOpts ),
	toupper(FlagNameL, FlagName),
	(flag_options(FlagNameL, FlagOpts) -> true ; FlagOpts = []).
ciao_config_entry('SET_FLAG_OPTIONS',
	    [
		default(no),
		valid_values([yes, no]),
		query(
		    "Set the prolog flags configured here in the ciao shell script.",
		    [extended])
	    ]).
ciao_config_entry('CIAOSH_COMMANDS',
	    [
		default(true),
		query(
		    "Pass extra commands to the ciao shell script.",
		    [extended])
	    ]).


% Options that are considered in define_flag:

flag_options(runtime_checks,
	    [
		query(
		    "If you wish to compile the Ciao libraries with runtime checks \n"||
		    "enabled then set the following variable to \"yes\".",
		    [extended])
	    ]).
flag_options(compress_lib,
	    [
		query(
		    "If you wish to compile the Ciao libraries with their bytecode\n"||
		    "compressed then set the following variable to \"yes\". Libraries\n"||
		    "generated this way will be smaller at the cost of a slightly slower\n"||
		    "usage, both in their load as when employed to create an executable.",
		    [extended])
	    ]).
flag_options(unused_pred_warnings,
	    [
		query(
		    "If you wish to show warnings about unused predicates, set this\n"||
		    "variable to \"yes\".", [extended])
	    ]).
