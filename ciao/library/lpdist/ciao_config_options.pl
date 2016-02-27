:- module(ciao_config_options, [], [ciaopaths, dcg, make, fsyntax, assertions, hiord, define_flag, regexp]).

:- set_prolog_flag(unused_pred_warnings, no).

% TODO: obtain the the help messages from this file as documentation (automatically)

:- doc(title, "Ciao System Configuration Options").
:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module contains definitions for many customizable
   Ciao options.").

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system)).
:- use_module(library(dirutils)).
:- use_module(library(system_extra), [do_str/3]).
:- use_module(library(lists)).
:- use_module(library(messages)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(format)).
:- use_module(library(regexp(regexp_flags)), []). % TODO: why?
:- use_module(library(rtchecks(rtchecks_basic))). % TODO: why?

:- use_module(library(bundle_registry(bundle_registry_load)), [bundle_description/4]).

:- use_module(library(lpdist(ciao_configure))).
:- use_module(library(lpdist(ciao_bundle_db))).
:- use_module(library(lpdist(makedir_aux))).
:- use_module(library(lpdist(ciao_config_db))).
:- use_module(library(lpdist(detcheader))).
% ===========================================================================
% Those facts define the configuration options for the
% build/installation of the Ciao bundles. The option values can also be 
% configured through an interactive menu.

% *** WARNING! ***
%   Order matters! If you use 'depend_on', the dependent variable must
%   be defined after.

% The order in the following clauses is the order of the menu.
% ----------------------------------------------------------------------------

% ciao_config_entry(Name, HowToGetValue).
:- discontiguous(ciao_config_entry/2).
:- export(ciao_config_entry/2).

% ===========================================================================

:- doc(section, "General Build/Installation Options").

% The Ciao version used
% TODO: some definitions here are duplicated in:
%       CIAODESRC/ciaosetup
%       CIAODESRC/ciao/build.sh

:- export(build_dir/1).
build_dir := ~fsR(sys_dir(build)).
:- export(buildbin_dir/1).
buildbin_dir := ~fsR(sys_dir(build)/bin).

% TODO: This name comes from the RPM's buildroot. Is there a better name?
%       The documentation is not very precise.
:- export(rootprefix/1).
:- pred rootprefix(BuildRoot) # "Unifies @var{BuildRoot} with the
   value of the environment variable @tt{ROOTPREFIX}, which is used in
   pbundle generator to do a fake installation in a custom specified
   directory.".

rootprefix(R) :-
	( current_env('ROOTPREFIX', S) -> true
	; S = ''
	),
	S = R.

:- export(install_log/1).
% TODO: defined in ciao/build.sh too
install_log := ~fsR(~build_dir/'install.log').

:- export(build_doc_dir/1).
build_doc_dir := ~fsR(~build_dir/doc).

:- export(setlocalciao/1).
setlocalciao := ~atom_concat([
		'CIAOALIASPATH= ',
 		'CIAOLIB=', ~fsR(bundle_src(ciao)), ' ',
		'CIAOHDIR=', ~local_hdir, ' ',
		'CIAOENGINE=', ~local_engine]).

% TODO: Unused?
unused_pred_warnings := ~get_config_val('UNUSED_PRED_WARNINGS').

% ---------------------------------------------------------------------------

% TODO: Not configurable
ciao_config_entry('CIAODESRC', [set_value(~fsR(bundle_src(ciaode)))]).
% TODO: Not configurable
ciao_config_entry('MAIN',      [set_value('ciao')]). % TODO: why?

% ---------------------------------------------------------------------------

ciao_config_entry('INTERACTIVE_CONFIG',
	    [
		noprevious,
		default('1'),
		valid_values(['1', '2', '3']),
		query(
		    "Entering the interactive configuration.\n"||
		    "You will now be asked some questions related to the configuration.\n"||
		    "Hit [Enter] to accept the default values shown in square brackets.\n\n"||
		    %
		    "Please select level of interaction:\n\n"||
		    "    1 --  Fully automatic (recommended).\n"||
		    "    2 --  Manually configure just a minimum set of options.\n"||
		    "    3 --  Manually configure an extended set of options.",
		    [default, minimum, extended])

	    ]).

% ---------------------------------------------------------------------------

ciao_config_entry('SILENT',
	    [
		default(false),
		valid_values([false, true]),
		query("Removes several messages shown during installation.",
		    [extended])
	    ]).

% ---------------------------------------------------------------------------

:- export(stop_if_error/1).
stop_if_error := ~get_config_val('STOP_IF_ERROR').
ciao_config_entry('STOP_IF_ERROR',
	    [
		default('yes'),
		valid_values(['yes', 'no']),
		query(
		    "Stop installation if an error is found.",
		    [extended]),
		show("Stop if error", [default])
	    ]).

% ---------------------------------------------------------------------------

:- doc(section, "Installation Type, Directories, and Permissions").

% TODO: rename 'ins' by 'global'
% TODO: rename 'src' by 'local'

:- export(instype/1).
instype := ~get_config_val('INSTYPE').
ciao_config_entry('INSTYPE',
	    [
		default('global'),
		valid_values(['global', 'local']),
		query(
		    "Select the type of installation:\n\n"||
		    "    global -- Install the system in a separate location from the\n"||
		    "              sources and set up things to use the installed version.\n"||
		    "              The system will not require the sources to run, and \n"||
		    "              they can be erased after installation.\n"||
		    "    local  -- The system will be compiled in, and run from the \n"||
		    "              sources (this is specially useful for developers).",
		    [minimum, extended]),
		show("Installation type is", [default])
	    ]).

% ---------------------------------------------------------------------------

% TODO: make use of default value (simplify scripts)
% TODO: rename 'all' and 'user'? (e.g. global and local too?)
% TODO: include here other access methods like menus, and desktop icons 

ciao_config_entry('REGISTRATION_TYPE',
	    [
		depend_on([('INSTYPE', InsType)]),
		valid_values(['all', 'user']),
		default(def_registration_type(InsType, SysregType), SysregType),
		query(
		    "Registration type:\n\n"||
		    "    all  --  Make the system available to all users. Typically you\n"||
		    "             you will need to complete the installation as root.\n"||
		    "    user --  Make the system available only for the current user\n"||
		    "             (configure it in the user\'s home directory).",
		    [minimum, extended]),
		show("Registration type is", [default])
	    ]).

def_registration_type(global, all).
def_registration_type(local, user).

% ---------------------------------------------------------------------------

:- export(prefix_dir/1).
prefix_dir := ~get_config_val('PREFIX').

ciao_config_entry('PREFIX',
	    [
		depend_on([('INSTYPE', InsType), ('REGISTRATION_TYPE', SysregType)]),
		set_value((InsType == 'local', build_dir(Value)), Value),
		default(get_prefix(SysregType, InsType, DefValue), DefValue),
		query("Specify the directory to perform the installation.",
		    [minimum, extended]),
		show("Prefix is", [default])
	    ]).

get_prefix(all,  global, '/usr/local').
get_prefix(user, global, ~get_home). % TODO: not valid combination!
get_prefix(all,  local, ~fsR(bundle_src(ciaode))).
get_prefix(user, local, ~fsR(bundle_src(ciaode))).

% ---------------------------------------------------------------------------

:- export(ciaobin_dir/1).
ciaobin_dir := ~get_config_val('BINDIR').
ciao_config_entry('BINDIR',
	    [
		depend_on([('PREFIX', Prefix)]),
		set_value(fsR(Prefix/'bin', Value), Value),
		show("Executables will go here", [default, minimum, extended])
	    ]).

% ---------------------------------------------------------------------------

:- export(ciaolib_root/1).
ciaolib_root := ~get_config_val('LIBROOT').
ciao_config_entry('LIBROOT',
	    [
		depend_on([('PREFIX', Prefix), ('INSTYPE', InsType)]),
		set_value(get_final_lib_root(InsType, Prefix, Value), Value),
		show("Libraries will go here", [default, minimum, extended])
	    ]).

get_final_lib_root('global', Prefix) := ~fsR(Prefix/'lib').
get_final_lib_root('local', _Prefix) := ~fsR(bundle_src(ciao)).

% ---------------------------------------------------------------------------

:- export(lib_dir/1).
lib_dir := ~get_config_val('LIBDIR').
ciao_config_entry('LIBDIR',
	    [
		depend_on([('INSTYPE', InsType), ('LIBROOT', LibRoot)]),
		set_value(get_lib_dir(InsType, LibRoot, Value), Value)
	    ]).

get_lib_dir('global', LibRoot) := ~fsR(LibRoot/'ciao').
get_lib_dir('local', LibRoot) := LibRoot.

% ---------------------------------------------------------------------------

% TODO: Do not configure, but export as a SH variable
%       This gives a value to the 'default_lib_dir' variable
%       in the engine (see ./ciao/build.sh)

:- export(reallib_dir/1).
reallib_dir := ~unix_file_value('REALLIBDIR').
ciao_config_entry('REALLIBDIR',
	    [
		depend_on([('INSTYPE', InsType), ('LIBROOT', LibRoot)]),
		set_value(get_reallib_dir(InsType, LibRoot, Value), Value),
		show("REALLIBDIR", [default])
	    ]).

% TODO: see ~fsR(bundle_inslib(Bundle))

% REALLIBDIR -- ~reallib_dir
get_reallib_dir('global', LibRoot) := ~fsR(concat_ver(ciao, LibRoot/'ciao'/'ciao')).
get_reallib_dir('local', LibRoot) := LibRoot.

% ---------------------------------------------------------------------------

% Relative path (for symbolic links) to reallib_dir from lib_dir
% TODO: find a better solution, required by makedir_SHARE.pl
:- export(relreallib_dir/1).
relreallib_dir := ~relreallib_dir_(~instype).

relreallib_dir_('global') := ~fsR(concat_ver(ciao, 'ciao')).
relreallib_dir_('local') := ''.
% ~bundle_name_version(ciao).

% ---------------------------------------------------------------------------

:- export(include_root/1).
include_root := ~get_config_val('INCLUDEROOT').
ciao_config_entry('INCLUDEROOT',
	    [
		depend_on([('PREFIX', Prefix), ('INSTYPE', InsType)]),
		set_value(get_final_include_root(InsType, Prefix, Value), Value),
		show("Headers will temporarily go here",
		    [default, minimum, extended])
	    ]).

% INCLUDEROOT -- ~include_root
get_final_include_root('global', Prefix) := ~fsR(Prefix/'include').
get_final_include_root('local', Prefix) := ~fsR(Prefix/'include').

% ---------------------------------------------------------------------------

% TODO: needed for CIAOHDIR, which is required to fill the value of
%       'default_c_headers_dir' C variable
ciao_config_entry('CIAOHDIRROOT',
	    [
		depend_on([('PREFIX', Prefix), ('INSTYPE', InsType)]),
		set_value(get_ciaohdir_root(InsType, Prefix, Value), Value),
		show("Final destination for headers is",
		    [default, minimum, extended])
	    ]).

% CIAOHDIRROOT -- 
% Where C header files live for this installation type, minus CIAOARCH subdir
% TODO: if ~instype=local, this is SRCINCLUDEDIR, use a single definition!
get_ciaohdir_root('global', Prefix) := ~fsR(concat_ver(ciao, Prefix/'lib'/'ciao'/'ciao')/'include').
get_ciaohdir_root('local', Prefix) := ~fsR(Prefix/'include').
% TODO: It was:
% 	~path_list_concat([Prefix, 'lib', 'ciao', ~bundle_name_version(ciao), 'include']).

% ---------------------------------------------------------------------------

:- export(enginedir/1).
enginedir := ~get_config_val('ENGINEDIR').
ciao_config_entry('ENGINEDIR',
	    [
		depend_on([('INSTYPE', InsType), ('PREFIX', Prefix)]),
		set_value(get_enginedir(InsType, Prefix, Value), Value)
	    ]).

% TODO: could we unify the relative path for 'global' and 'local'? (in 'local', it is now under 'build')
% ENGINEDIR -- ~enginedir
get_enginedir('global', Prefix) := ~fsR(concat_ver(ciao, Prefix/'lib'/'ciao'/'ciao')/'engine').
get_enginedir('local', Prefix) := ~fsR(Prefix/'objs').
%	~path_list_concat([Prefix, 'lib', 'ciao', ~bundle_name_version(ciao), 'engine']).

% ---------------------------------------------------------------------------

:- export(get_installed_ciaoengine/1).
% TODO: pbundle_gen_mac is the only caller of this predicate
% TODO: there are similar definitions in makedir_part_ciao.pl
get_installed_ciaoengine := ~fsR(~enginedir/(~atom_concat(['ciaoengine', '.', ~get_platform]))).

% ---------------------------------------------------------------------------

ciao_config_entry('EXECMODE',
	    [
		default('775'),
		query("Permissions for installed execs/dirs", [extended])
	    ]).

% ---------------------------------------------------------------------------

ciao_config_entry('DATAMODE',
	    [
		default('664'),
		query("Permissions for installed data files", [extended])
	    ]).

:- export(perms/1).
% Define this to be the permissions for installed execs/dirs and data files:
perms(perm(rwX, rwX, rX)).

% ---------------------------------------------------------------------------

% TODO: Ignored by many of the installation code (thus, not working)
ciao_config_entry('INSTALLGROUP',
	    [
		default(''),
		query(
		    "Group for the installed files (empty means use default)",
		    [extended])
	    ]).

% ===========================================================================

:- doc(section, "Command-line Enviroment").

:- export(update_bashrc/1).
update_bashrc := ~get_config_val('UPDATE_BASHRC').
ciao_config_entry('UPDATE_BASHRC',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType)]),
		default(get_update_sh(SysregType, DefValue), DefValue),
		valid_values(['yes', 'no']),
		query(
		    "Set to \"no\" if you do not wish to configure bash to "||
		    "work with Ciao or \nif you wish to configure it by hand.",
		    [minimum, extended]),
		show("Update bash init file", [default])
	    ]).

% ---------------------------------------------------------------------------

:- export(dotbashrc/1).
dotbashrc := ~get_config_val('DOTBASHRC').
ciao_config_entry('DOTBASHRC',
	    [
		depend_on([('UPDATE_BASHRC', yes),
			('REGISTRATION_TYPE', SysregType)]),
		default(get_bashrc(SysregType, DefValue), DefValue),
		query(
		    "The bash initialization file where the Ciao variables are set.",
		    [minimum, extended]),
		show("Bash initialization file", [default])
	    ]).

get_bashrc(all, F) :-
	( member(F, ['/etc/bash.bashrc', '/etc/bashrc']),
	    file_exists(F) ->
	    true
	; F = '/etc/bashrc'
	).
get_bashrc(user) := ~fsR(~get_home/'.bashrc').

% ---------------------------------------------------------------------------

:- export(update_cshrc/1).
update_cshrc := ~get_config_val('UPDATE_CSHRC').
ciao_config_entry('UPDATE_CSHRC',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType)]),
		default(get_update_sh(SysregType, DefValue), DefValue),
		valid_values(['yes', 'no']),
		query(
"Set to \"no\" if you do not wish to configure csh/tcsh to work with Ciao or
if you wish to configure it by hand.",
		    [minimum, extended]),
		show("Update csh init file", [default])
	    ]).

% ---------------------------------------------------------------------------

:- export(dotcshrc/1).
dotcshrc := ~get_config_val('DOTCSHRC').
ciao_config_entry('DOTCSHRC',
	    [
		depend_on([('UPDATE_CSHRC', yes), ('REGISTRATION_TYPE', SysregType)]),
		default(get_cshrc(SysregType, DefValue), DefValue),
		query(
		    "The csh/tcsh initialization file where the Ciao variables are set.\n"||
		    "Note that on some systems tcsh reads \"~/.tcshrc\".",
		    [minimum, extended]),
		show("Csh/Tcsh initialization file", [default])
	    ]).

% by default, assume /etc/csh.cshrc
get_cshrc(all, F) :-
	( member(F, ['/etc/csh.cshrc', '/etc/tcsh.tcshrc']),
	    file_exists(F) ->
	    true
	; F = '/etc/csh.cshrc'
	).
get_cshrc(user) := ~get_cshrc_name.

% by default, assume .cshrc
get_cshrc_name(C) :-
	( ( member(F, ['.tcshrc', '.cshrc']),
	    C = ~fsR((~get_home)/F),
	    file_exists(C)
	  ) ->
	    true
	; F = '.cshrc',
	  C = ~fsR((~get_home)/F)
	).

get_update_sh('all',  'no').
get_update_sh('user', 'yes').

% ===========================================================================

:- doc(section, "Options for Ciao/Toplevel").

ciaosrc := ~bundle_description(ciao, _, _).

:- export(ciaosh/1).
ciaosh := ~fsR(concat_verk(ciao, plexe, ~buildbin_dir/('ciaosh'))).
:- export(binciaosh/1).
binciaosh := ~fsR(concat_verk(ciao, plexe, ~ciaobin_dir/('ciaosh'))).

% TODO: sformat is slow
% TODO: document: extra commands or the 'ciao' script
:- export(ciao_extra_commands/1).
ciao_extra_commands(ExtraCommands) :-
	sformat(ExtraCommands, "-e '~w'",
	    [~list_to_lits([
			use_module(library(unittest)),
			~ciaosh_commands|~prolog_flag_cmds])]).

% 	unittest_option(yes). % very slow if tested here, but less fail prone
% TODO: used?
unittest_option(yes) --> [run_test_module].
unittest_option(no) --> [].

:- export(install_prolog_name/1).
install_prolog_name := ~get_config_val('INSTALL_PROLOG_NAME').
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

% ===========================================================================

:- doc(section, "Options for Emacs Mode").

:- export(emacs_path/1).
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

% ---------------------------------------------------------------------------

:- export(with_emacs_mode/1).
with_emacs_mode := ~get_env_or_config_val('WITH_EMACS_MODE').
ciao_config_entry('WITH_EMACS_MODE',
	    [
		valid_values(['yes', 'no']),
		% default(VerifyEmacs),
		default(verify_emacs(VerifyEmacs), VerifyEmacs),
		show_option("Emacs available"),
		query(
		    "Set to \"yes\" if you wish to install the Ciao emacs libraries which\n"||
		    "implement the Emacs-based IDE (integrated development environment)\n"||
                    "(highly recommended).  It should be set to no if emacs is not\n"||
		    "installed in the system.  It is safe to leave as \"yes\" otherwise.",
		    [minimum, extended]),
		show("Install Emacs-based IDE",
		    [default])
	    ]).

verify_emacs(Value) :-
	( emacs_installed -> Value = yes ; Value = no ).

emacs_installed :- find_emacs(_).

find_emacs(File) :- find_paths_file(~emacs_exec, File).

% TODO: factor this pattern
emacs_base := emacs.
emacs_exec := ~exec_names(~emacs_base).

% ---------------------------------------------------------------------------

:- export(with_xemacs_mode/1).
with_xemacs_mode := ~get_env_or_config_val('WITH_XEMACS_MODE').
ciao_config_entry('WITH_XEMACS_MODE',
	    [
		valid_values(['yes', 'no']),
		% default(VerifyXEmacs),
		default(verify_xemacs(VerifyXEmacs), VerifyXEmacs),
		show_option("XEmacs available"),
		query(
		    "Set to \"yes\" if you wish to install the Ciao emacs libraries which\n"||
		    "implement the XEmacs based IDE (integrated development environment).\n"||
		    "It should be set to no if xemacs is not installed in the system.  It \n"||
		    "is safe to leave as \"yes\" otherwise.", [minimum, extended
		    ]),
		show("Install XEmacs based IDE",
		    [default]
		)
	    ]).

verify_xemacs(Value) :-
	( xemacs_installed -> Value = yes ; Value = no ).

xemacs_installed :- find_xemacs(_).

xemacs_base := xemacs.
xemacs_exec := ~exec_names(~xemacs_base).
find_xemacs(File) :- find_paths_file(~xemacs_exec, File).

% ---------------------------------------------------------------------------

:- export(emacs_for_ciao/1).
emacs_for_ciao := ~get_env_or_config_val('EMACS_FOR_CIAO').
ciao_config_entry('EMACS_FOR_CIAO',
	    [
		depend_on([('WITH_EMACS_MODE', yes)]),
		default(find_emacs(DefValue), DefValue),
		query(
		    "The version of emacs that you wish to use with Ciao. The development \n" ||
		    "environment will be compiled for use with this version.",
		    [minimum, extended]),
		show("Emacs version to be used", [default])
	    ]).

% ---------------------------------------------------------------------------

% TODO: Change name
:- export(update_dotemacs/1).
update_dotemacs := ~get_config_val('UPDATE_DOTEMACS').
ciao_config_entry('UPDATE_DOTEMACS',
	    [
		depend_on([('WITH_EMACS_MODE', yes),
			('REGISTRATION_TYPE', SysregType)]),
		%, ('VERIFY_EMACS', yes)]),
		% This set value means that no question is really asked
		set_value(update_dotemacs_(SysregType, Value), Value),
		default('yes'),
		valid_values(['yes', 'no']),
		query("Set to \"yes\" if you wish to configure emacs to\n"||
		    "work with Ciao (modify emacs initialization file).",
		    [minimum, extended]),
		show("Modify emacs init file", [default])
	    ]).

% update_dotemacs_(InsType, VerifyEmacs, UpdateEmacs)

update_dotemacs_(all,  no) :- !.
update_dotemacs_(user, yes).

% ---------------------------------------------------------------------------

:- export(dotemacs/1).
dotemacs := ~get_config_val('DOTEMACS').
ciao_config_entry('DOTEMACS',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType), ('UPDATE_DOTEMACS', yes)]),
		default(get_dotemacs(SysregType, DefValue), DefValue),
		query("Define the emacs initialization file where the Ciao settings will be\n"||
		      "added.", [minimum, extended]),
		show("Emacs initialization file", [default])
	    ]).

get_dotemacs(user) := ~fsR(~get_home/'.emacs').

% ---------------------------------------------------------------------------

:- export(emacs_site_start/1).
emacs_site_start := ~get_config_val('EMACS_SITE_START').
ciao_config_entry('EMACS_SITE_START',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType), ('INSTYPE', InsType),
			('WITH_EMACS_MODE', yes),
			('REALLIBDIR',            RealLibDir)
		    ]),
		default(get_emacs_site_start(SysregType, InsType, RealLibDir,
			Value), Value),
		query(
		    "Specify in what file/directory you want to insert/copy the Ciao Emacs Mode\n" ||
		    "initialization code.", [extended]),
		show("Emacs site start", [default, minimum])
	    ]).

get_emacs_site_start('all', 'global', _RealLibDir, Value) :-
	emacs_site_start_(Value),
	!.
get_emacs_site_start(_, _, RealLibDir, RealLibDir).

emacs_site_start_(SiteStart) :-
	possible_emacs_site_start(SiteStart),
	file_exists(SiteStart),
	!.

% Note: this returns files or directories, as follows:
%  .../site-lisp/site-start.d  ==> must put a script in that directory
%  .../site-lisp               ==> must use/create a site-start.el
possible_emacs_site_start :=
	'/Applications/Emacs.app/Contents/Resources/site-lisp/site-start.d'|
	'/Applications/Emacs.app/Contents/Resources/site-lisp'|
	'/usr/share/emacs/site-lisp/site-start.d'|
	'/usr/share/emacs/site-lisp'|
	'/etc/emacs/site-start.d'.

% ---------------------------------------------------------------------------

% TODO: Do not customize this
:- export(emacsinitfile/1).
emacsinitfile := ~get_config_val('EMACSINITFILE').
ciao_config_entry('EMACSINITFILE',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType), ('WITH_EMACS_MODE', yes)]),
		set_value(get_emacs_init_file(SysregType, Value), Value),
		query("Specify the name of the emacs lisp file defining the ciao mode.",
		      []),
		show("Emacs init file", [default, minimum, extended])
	    ]).
%% ciao_config_entry('VERIFY_XEMACS',
%% 	    [
%% 		set_value(verify_xemacs(VerifyXEmacs), VerifyXEmacs)
%% 	    ]).

get_emacs_init_file(all, '65ciao-mode-init.el') :-
	% Use if it is debian based
	get_os('LINUX'),
	file_exists('/etc/debian_version'),
	!.
get_emacs_init_file(_, 'ciao-mode-init.el').

% ---------------------------------------------------------------------------

:- export(xemacs_for_ciao/1).
xemacs_for_ciao := ~get_env_or_config_val('XEMACS_FOR_CIAO').
ciao_config_entry('XEMACS_FOR_CIAO',
	    [
		depend_on([('WITH_XEMACS_MODE', yes)]),
		default(find_xemacs(DefValue), DefValue),
		query(
		    "The version of xemacs that you wish to use with Ciao. The development\n" ||
		    "environment will be compiled for use with this version.",
		    [minimum, extended]),
		show("XEmacs version to be used", [default])
	    ]).

% ---------------------------------------------------------------------------

% TODO: Change name
:- export(update_dotxemacs/1).
update_dotxemacs := ~get_config_val('UPDATE_DOTXEMACS').
ciao_config_entry('UPDATE_DOTXEMACS',
	    [
		depend_on([('WITH_XEMACS_MODE', yes),
			('REGISTRATION_TYPE', SysregType)]),
		% , ('VERIFY_XEMACS', yes)]),
		% This set value means that no question is really asked
		set_value(update_dotxemacs_(SysregType, Value), Value),
		default('yes'),
		valid_values(['yes', 'no']),
		query("Set to \"yes\" if you wish to configure XEmacs to\n"||
		    "work with Ciao (modify xemacs initialization file).",
		    [minimum, extended]),
		show("Modify xemacs init file", [default])
	    ]).

update_dotxemacs_(all,  no) :- !.
update_dotxemacs_(user, yes) :-
	file_exists('~/.xemacs'),
	!.
update_dotxemacs_(user, no).

% ---------------------------------------------------------------------------

:- export(dotxemacs/1).
dotxemacs := ~get_config_val('DOTXEMACS').
ciao_config_entry('DOTXEMACS',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType), ('UPDATE_DOTXEMACS', yes)]),
		default(get_dotxemacs(SysregType, DefValue), DefValue),
		query("Define the xemacs initialization file where the Ciao settings will be\n"||
		      "added.", [minimum, extended]),
		show("XEmacs initialization file", [default])
	    ]).

get_dotxemacs(user) := ~fsR(~get_home/'.xemacs'/'init.el').

% ---------------------------------------------------------------------------

:- export(xemacs_site_start/1).
xemacs_site_start := ~get_config_val('XEMACS_SITE_START').
ciao_config_entry('XEMACS_SITE_START',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType), ('INSTYPE', InsType),
			('WITH_XEMACS_MODE', yes),
			('REALLIBDIR',             RealLibDir)]),
		default(get_xemacs_site_start(SysregType, InsType, RealLibDir,
			Value), Value),
		query("Specify in what file/directory you want to insert/copy the Ciao Emacs Mode\n" ||
		      "initialization code (XEmacs).", [extended]),
		show("XEmacs site start", [default, minimum])
	    ]).

get_xemacs_site_start('all', 'global', _RealLibDir, Value) :-
	xemacs_site_start_(Value),
	!.
get_xemacs_site_start(_, _, RealLibDir, RealLibDir).

xemacs_site_start_(SiteStart) :-
	possible_xemacs_site_start(SiteStart),
	file_exists(SiteStart),
	!.

possible_xemacs_site_start :=
	'/usr/share/xemacs/site-packages/lisp/site-start.d'.

% ---------------------------------------------------------------------------

:- doc(section, "LPdoc Options").

% TODO: do the changes required to remove '/'
:- export(htmldir/1).
htmldir := ~atom_concat(~fsR(rootprefix(~get_config_val('HTMLDIR'))), '/').
% ~atom_concat(~rootprefix, ~get_config_val('HTMLURL')).
ciao_config_entry('HTMLDIR',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType), ('INSTYPE', InsType)]),
		set_value((InsType == 'local', build_doc_dir(Value)), Value),
		default(get_htmldir(SysregType, DefValue), DefValue),
		query("Define this to be the directory in which you wish "||
		    "the documentation \nin html format to be installed. "||
		    "It is recommended that this directoriy \n"||
		    "be accessible via WWW.\n",
		    [minimum, extended]),
		show("HTML manuals will go here", [default])
	    ]).
% TODO: trailing /?
get_htmldir(all) := '/var/www/html/ciao'.
get_htmldir(user) := ~atom_concat(~fsR(~get_home/'public_html'/'CiaoDE'), '/').

% ---------------------------------------------------------------------------

ciao_config_entry('HTMLURL',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType), ('INSTYPE', InsType)]),
		set_value((InsType == 'local', build_doc_dir(Value)), Value),
		default(get_htmlurl(SysregType, DefValue), DefValue),
		query(
		    "Define the URL to access the previous directory via WWW.",
		    [minimum, extended]),
		show("Broswer will read website as", [default])
	    ]).
% TODO: trailing /?
get_htmlurl(all) := '/ciao/'.
get_htmlurl(user) := ~atom_concat(['/~', ~get_pwnam, '/CiaoDE/']).

% ---------------------------------------------------------------------------

% TODO: do the changes required to remove '/'
:- export(docdir/1).
docdir := ~atom_concat(~fsR(rootprefix(~get_config_val('DOCDIR'))), '/').
ciao_config_entry('DOCDIR',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType), ('INSTYPE', InsType), (
			    'HTMLDIR', HtmlDir), ('PREFIX', Prefix)]),
		set_value((InsType == 'local', build_doc_dir(Value)), Value),
		default(get_docdir(SysregType, Prefix, HtmlDir, DefValue),
		    DefValue),
		query(
		    "Define this to be the directory in which you wish the documentation\n"||
		    "to be installed.\n",
		    [minimum, extended]),
		show("Documentation will go here", [default])
	    ]).
get_docdir(all,  Prefix,  _HtmlDir) := ~fsR(Prefix/'share'/'doc'/'ciao').
get_docdir(user, _Prefix, HtmlDir) := HtmlDir.

% ---------------------------------------------------------------------------

% TODO: do the changes required to remove '/'
:- export(mandir/1).
mandir := ~atom_concat(~fsR(rootprefix(~get_config_val('MANDIR'))), '/').
ciao_config_entry('MANDIR',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType), ('INSTYPE', InsType), (
			    'HTMLDIR', HtmlDir), ('PREFIX', Prefix)]),
		set_value((InsType == 'local', build_doc_dir(Value)), Value),
		default(get_mandir(SysregType, Prefix, HtmlDir, DefValue),
		    DefValue),
		query("Define this to be the directory in which you wish the man (unix manual\n"||
		      "entry) file to be installed.\n",
		    [minimum, extended]),
		show("Man entry will go here", [default])
	    ]).
get_mandir(all,  Prefix,  _HtmlDir) := ~fsR(Prefix/'share'/'man').
get_mandir(user, _Prefix, HtmlDir) := HtmlDir.

% ---------------------------------------------------------------------------

% TODO: do the changes required to remove '/'
:- export(infodir/1).
infodir := ~atom_concat(~fsR(rootprefix(~get_config_val('INFODIR'))), '/').
ciao_config_entry('INFODIR',
	    [
		depend_on([('REGISTRATION_TYPE', SysregType), ('INSTYPE', InsType), (
			    'HTMLDIR', HtmlDir), ('PREFIX', Prefix)]),
		set_value((InsType == 'local', build_doc_dir(Value)), Value),
		default(get_infodir(SysregType, Prefix, HtmlDir, DefValue),
		    DefValue),
		query(
		    "Define this to be the directory in which you wish the info file\n"||
		    "installed.  Ideally, this directory should be accesible via emacs.\n",
		    [minimum, extended]),
		show("Info file will go here", [default])
	    ]).
get_infodir(all,  Prefix,   _HtmlDir) := ~fsR(Prefix/'share'/'info').
get_infodir(user, _Preifix, HtmlDir) := HtmlDir.

% ---------------------------------------------------------------------------

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

% ---------------------------------------------------------------------------

:- export(buildbin_lpdoc/1).
buildbin_lpdoc := ~fsR(~buildbin_dir/'lpdoc').

% TODO: also computed in lpdoc/src/lpdoc.pl (ensure_lpdoclib_defined/0) 
%       (this cannot be a configurable setting, lpdoc knows where it lives)
:- export(lpdoclib/1).
lpdoclib := ~fsR(bundle_src(lpdoc)/'lib').

%% .bib files now in a predefined location (JFMC)
%%bibfile := ~decompose(~get_config_val('BIBFILES'), ',').
% Bibliography files for documentation (clip.bib and general.bib)
:- export(bibfile/1).
bibfile := ~fsR(bundle_src(ciaode)/'doc'/'bibtex'/'clip')
	 | ~fsR(bundle_src(ciaode)/'doc'/'bibtex'/'general').

% decompose(Text0, Separator, Element) :-
%	( atom_concat([Element0, ',', Text], Text0) ->
%	    ( Element = Element0
%	    ; decompose(Text, Separator, Element)
%	    )
%       ; Element = Text0
%       ).

:- export(docformatdir/2).
docformatdir(html, Dir) :- !, htmldir(Dir).
docformatdir(manl, Dir) :- !, mandir(Dir).
docformatdir(info, Dir) :- !, infodir(Dir).
docformatdir(_,    Dir) :- docdir(Dir).

:- export(docformat/1).
docformat := pdf|manl|info|html. % | ps.

% ---------------------------------------------------------------------------
% TODO: Depends on LPdoc options!

:- doc(section, "Pillow Options").

:- export(webimagespath/1).
webimagespath := ~get_config_val('WEB_IMAGES_PATH').
ciao_config_entry('WEB_IMAGES_PATH',
	    [
		% depend_on( [ ( 'REGISTRATION_TYPE', SysregType ) ] ),
		% default( get_web_images_path( SysregType, DefValue ), DefValue ),
		depend_on([('HTMLDIR', HtmlDir)]),
		default(get_web_images_path(HtmlDir, DefValue), DefValue),
		query(
		    "For the PiLLoW Web programming library, define the directory \n"||
		    "(accessible via WWW) where the icons which come with PiLLoW \n"||
		    "(in library/pillow/images) will go.", [minimum, extended]
		),
		show("Pillow images will go here", [default])
	    ]).

% TODO: trailing /?
% TODO: use 'pillow/images'? 'images' alone is a very common name
get_web_images_path(HtmlDir) := ~fsR(HtmlDir/images).

% ---------------------------------------------------------------------------

:- export(webimagesurl/1).
webimagesurl := ~get_config_val('WEB_IMAGES_URL').
ciao_config_entry('WEB_IMAGES_URL',
	    [
		% depend_on( [ ( 'REGISTRATION_TYPE', SysregType ) ] ),
		% default( get_web_images_url( SysregType, DefValue ), DefValue ),
		depend_on([('HTMLURL', HtmlUrl)]),
		default(get_web_images_url(HtmlUrl, DefValue), DefValue),
		query(
		    "Define the URL to access the previous directory via WWW.",
		    [minimum, extended]),
		show("The browser will find the Pillow images at", [default])
	    ]).

get_web_images_url(HtmlUrl) := ~fsR(HtmlUrl/'images').

% ===========================================================================

:- doc(section, "Options for Foreign Libraries").

% TODO: share definitions

% ---------------------------------------------------------------------------

% (Support for GNU pkg-config based libraries)

:- export(foreign_config_tool/2).
:- discontiguous foreign_config_tool/2.

:- export(foreign_config_var/3).
% The configuration for foreign library @var{Foreign} has value
% @var{Value} in variable @var{Var}.
foreign_config_var(Foreign, Var, Value) :-
	foreign_config_tool(Foreign, CfgTool),
	% TODO: correct? options to get PPL version were set to nofail
	do_str([CfgTool, ' --', Var], ~command_option, Value0),
	( append(Value, "\n", Value0) -> % remove trailing \n
	    true
	; Value = Value0
	).

% ---------------------------------------------------------------------------

:- doc(section, "Options for MySQL bindings").

:- export(with_mysql/1).
with_mysql := ~get_config_val('WITH_MYSQL').
ciao_config_entry('WITH_MYSQL',
	    [
		default(verify_mysql(WithMySQL), WithMySQL),
		show_option_with_help(
		    "MySQL available",
		    "MySQL has not been detected.  If you would like to use the\n"||
	    	    "Ciao-MySQL interface it is highly recommended that you stop\n"||
	    	    "the Ciao configuration now and install MySQL first."),
		valid_values(['yes', 'no']),
		query(
		    "Set to \"yes\" if you wish to interface with the MySQL database.\n"||
		    "If you choose to have the MySQL interface, you should have the MySQL\n"||
		    "client part installed in the machine where you are compiling and using\n"||
		    "it.  The MySQL daemon should also be up and running when using the\n"||
		    "interface.", [extended]),
		show("Install MySQL support", [default, minimum])
	    ]).

verify_mysql(Value) :-
	( mysql_installed -> Value = yes ; Value = no ).

mysql_installed :-
	detect_c_headers(['mysql/mysql.h']).

% ---------------------------------------------------------------------------

:- export(mysql_client_directory/1).
mysql_client_directory := ~get_config_val('MYSQL_CLIENT_DIRECTORY').
ciao_config_entry('MYSQL_CLIENT_DIRECTORY',
	    [
		depend_on([('WITH_MYSQL', 'yes')]),
		default(get_mysql_dir(MySQLDir), MySQLDir),
		query(
		    "You should also specify where the MySQL client library is installed.",
		    [minimum, extended]),
		show("MySQL client library path", [default])
	    ]).

get_mysql_dir(MySQL) :-
	locate_file('libmysqlclient.a', MySQL),
	!.
get_mysql_dir('/usr/lib/mysql') :-
	warning_message(
	    "Unable to determine where the MySQL client library is " ||
	    "installed.\nCurrent value (/usr/lib/mysql) is only a guess.").

% TODO: 'locate' IS NOT available in many platforms (e.g., Mac OS X)
% TODO: Move to system_extra; find a better way of detecting libraries
locate_file(FileName, FileDir) :-
	do_str(['locate ', FileName], nofail, S),
	atom_codes(FileName, SFileName),
	append(SFileName, "\n",              SFileNameN),
	append(SFileDir,  "/" || SFileNameN, S),
	atom_codes(FileDir, SFileDir),
	!.

% TODO: Used?
locate_files([]).
locate_files([F|Fs]) :-
	locate_file(F, _),
	locate_files(Fs).

% ---------------------------------------------------------------------------

:- doc(section, "Options for GSL Library").

:- export(with_gsl/1).
with_gsl := ~get_config_val('WITH_GSL').
ciao_config_entry('WITH_GSL',
	    [
		default(verify_gsl(WithGSL), WithGSL),
		% default('no'),
		show_option_with_help(
	    	    "GSL available",
	    	    "GSL has not been detected.  If you want to use the math\n"||
	    	    "library it is highly recommended that you stop the Ciao\n"||
	    	    "configuration and install the GSL library first."),
		valid_values(['yes', 'no']),
		query(
		    "Set to \"yes\" if you wish to interface with the GSL (GNU Scientific\n"||
		    "Library). If you choose to have the GSL interface, you should have the\n"||
		    "GSL development library installed in the machine where you are\n"||
		    "compiling and using it.", [extended]),
		show("Install GSL support", [default, minimum])
	    ]).

foreign_config_tool(gsl, CfgTool) :-
	CfgTool = ~gsl_config_base.

gsl_config_base := 'gsl-config'.
gsl_installed :-
	% TODO: Next literal required because now GSL 32 bits is not available
	% TODO: in Linux 64 bits -- EMM.
	\+ get_platform('LINUXi86_64'),
	find_paths_file(~exec_names(~gsl_config_base), _).

verify_gsl(Value) :-
	( gsl_installed -> Value = yes ; Value = no ).

% ---------------------------------------------------------------------------

:- doc(section, "Options for PPL Library").

:- export(with_ppl/1).
with_ppl := ~get_config_val('WITH_PPL').
ciao_config_entry('WITH_PPL',
	    [
		% default(verify_ppl(WithPPL), WithPPL),
%	        show_option_with_help(
%		    "PPL >= 0.9 available",
%	            "PPL has not been detected."),
		default('no'),
		valid_values(['yes', 'no']),
		query(
		    "Set to \"yes\" if you wish to interface with the PPL", [
			extended]),
		show("Install PPL support", [default, minimum])
	    ]).

foreign_config_tool(ppl, CfgTool) :-
	CfgTool = ~ppl_config_base.

ppl_config_base := 'ppl-config'.
ppl_installed :-
	find_paths_file(~exec_names(~ppl_config_base), _).

% TODO: not used... reactivate?
verify_ppl(Value) :-
%	( ppl_installed, ppl_version(V) ->
%	    Value = yes
%	    message([V])
	( ppl_installed, ppl_version(_) ->
	    Value = yes
	; Value = no
	).

:- export(ppl_version/1).
ppl_version(Version) :-
	foreign_config_var(ppl, '--version', Str),
	parse_ppl_version(Str, Version).

% from "Major.Minor" string to [Major,Minor]
parse_ppl_version(Str, L) :-
	( append(StrH, "." || StrT, Str) ->
	    L = [H|T],
	    number_codes(H, StrH),
	    parse_ppl_version(StrT, T)
	; L = [H],
	  number_codes(H, Str)
	).

%get_ppl_interface :=
%	'/usr/local/src/ppl-0.7/interfaces/Prolog/Ciao/ppl_ciao.pl'.

% ---------------------------------------------------------------------------

% ciao_config_entry('USE_PPL',
% 	    [
% 	    default(verify_ppl(VerifyPPL), VerifyPPL),
% 	    valid_values(['yes', 'no']),
% 	    query("Do you have the Parma Polyhedra Library (PPL) installed "||
% 		"and want to use it?", [extended]),
% 	    show("Using PPL",          [default, minimum])
% 	    ]).

% ---------------------------------------------------------------------------

% ciao_config_entry('PPL_INTERFACE',
% 	    [
% 	    depend_on([('USE_PPL', 'yes')]),
% 	    default(get_ppl_interface(PPLInterface), PPLInterface),
% 	    query("Specify the full file name of the PPL-Ciao interface file",
% 		[extended]),
% 	    show("PPL interface file", [default, minimum])
% 	    ]).

% ---------------------------------------------------------------------------

:- doc(section, "Options for Java Interface").

:- export(with_java_interface/1).
with_java_interface := ~get_config_val('WITH_JAVA_INTERFACE').
ciao_config_entry('WITH_JAVA_INTERFACE',
	    [
		default(verify_java(VerifyJava), VerifyJava),
		show_option_with_help(
		    "Javac and Javadoc available",
		    "Java has not been detected. If you would like to use the\n"||
	    	    "utilities for the Java interface it is highly recommended that\n"||
	    	    "you stop the Ciao configuration now and install Java first."),
% 	    "If Java is already installed and you use Debian/Ubuntu perhaps\n"||
% 	    "you forgot to run: sudo update-java-alternatives --set java-6-sun."
		valid_values(['yes', 'no']),
		query(
		    "Whether you have a reasonably recent version of Java.\n"||
		    "If so the utilities for the Java interface under\n"||
		    "$(CIAOSRC)/library/javall will be compiled , along with\n"||
		    "examples and documentation.",
		    [extended]),
		show("Use Java interface", [default, minimum])
	    ]).

verify_java(Value) :-
	( javac_installed, javadoc_installed -> Value = yes ; Value = no ).

javac_exec := ~exec_names(javac).
javac_installed :- find_paths_file(~javac_exec, _), !,
	is_sun_javac.

% This could be fail prune, so tell me if somebody knows a better way
% to do this. --EMM
is_sun_javac :-
	do_str(['(javac -version 2>&1)'], nofail, String),
	append(_, "javac 1."||_, String),
	do_str(['(java -version 2>&1)'], nofail, SJava),
	% Kludge: In linux 64, you have to use the 64-bit Server VM --EMM
	( get_platform('LINUXi86_64') -> append(_, "64-Bit"||_, SJava)
	; true
	),
	!.

javadoc_exec := ~exec_names(javadoc).
javadoc_installed :-
	find_file(~javadoc_exec, _, _).

% ---------------------------------------------------------------------------

:- doc(section, "Ant Options").

:- export(with_ant/1).
with_ant := ~get_config_val('WITH_ANT').
ciao_config_entry('WITH_ANT',
	    [
		depend_on([('WITH_JAVA_INTERFACE', WithJavaInterface)]),
		default(verify_ant(WithJavaInterface, VerifyAnt), VerifyAnt),
		show_option_with_help(
                    "Ant available",
		    "Ant has not been detected. If you would like to use the\n"||
	    	    "resource analysis for java, which requires ant, it is\n"||
	    	    "highly recommended that you stop the Ciao configuration\n"||
	    	    "now and install Ant first."),
		valid_values(['yes', 'no']),
		query(
		    "Verify if ant (a Java based make tool) is installed\n"||
		    "in the system.",
		    [extended]),
		show("Use ant, a Java make tool", [default, minimum])
	    ]).

verify_ant(no,  no).
verify_ant(yes, VerifyAnt) :-
	verify_ant_yes(VerifyAnt).

verify_ant_yes(Value) :-
	( ant_installed -> Value = yes ; Value = no ).

ant_exec := ~exec_names(ant).
ant_installed :- find_paths_file(~ant_exec, _).

% ---------------------------------------------------------------------------

:- doc(section, "Engine and C Compilation Options").

:- export(get_platformdeb/1).
get_platformdeb := ~atom_concat(~get_platform, ~get_debug).

local_engine := ~fsR(concat_k(exec, ~build_dir/objs/(~get_platformdeb)/'ciaoengine')).
local_hdir := ~fsR(~build_dir/include/(~get_platformdeb)).

% ---------------------------------------------------------------------------
% TODO: Those flags are set from sh code in ciaosetup (see
%       do__bootstrap_settings), but those definitions are necessary to
%       write the final configuration files from Prolog.

ciao_config_entry('CUSTOM_CC',
	    [
		option_name(cc), % name for command line option
		query("Custom C compiler", [extended]),
		default(true, '')
	    ]).

ciao_config_entry('CUSTOM_LD',
	    [
		option_name(ld), % name for command line option
		query("Custom C linker", [extended]),
		default(true, '')
	    ]).

ciao_config_entry('EXTRA_CFLAGS',
	    [
		option_name(cflags), % name for command line option
		query("Specify additional C compiler flags", [extended]),
		default(true, '')
	    ]).

ciao_config_entry('EXTRA_LDFLAGS',
	    [
		option_name(ldflags), % name for command line option
		query("Specify additional C linker flags", [extended]),
		default(true, '')
	    ]).

% ---------------------------------------------------------------------------

:- export(optimizing_compiler/1).
optimizing_compiler := ~get_config_val('OPTIMIZING_COMPILER').
ciao_config_entry('OPTIMIZING_COMPILER',
	    [
		depend_on([('INSTYPE', InsType), ('REGISTRATION_TYPE', SysregType)]),
		valid_values(['yes', 'no']),
		set_value(\+((InsType == 'local', SysregType == 'user')), no),
		default('no'),
		query(
		    "Specify if you want to install the optimizing compiler.", [
			minimum, extended]),
		show("Install optimizing compiler", [default])
	    ]).

% ---------------------------------------------------------------------------

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

% ---------------------------------------------------------------------------

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

% ---------------------------------------------------------------------------

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

% ---------------------------------------------------------------------------

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

% ---------------------------------------------------------------------------

% TODO: should be: with-...?
:- export(tabled_execution/1).
tabled_execution := ~get_config_val('TABLED_EXECUTION').
ciao_config_entry('TABLED_EXECUTION',
	    [
		default('yes'),
		valid_values(['yes', 'no']),
		query(
		    "Set the following variable to \"yes\" if you wish to compile an engine\n"||
		    "with support for tabled execution of goals.",
		    [
			extended])
	    ]).

% ---------------------------------------------------------------------------

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
		    "suspect that your C compiler performs buggy optimizations (which\n"||
		    "should not be the case), turn optimization off.  This may happen more\n"||
		    "easily in concurrent applicacions: if you write any thread-based\n"||
		    "program and unexpected results appear, try recompiling Ciao without\n"||
		    "optimization options first.", [extended])
	    ]).

% ---------------------------------------------------------------------------

% TODO: I'd say... mostly obsolete
ciao_config_entry('CROSS_COMPILER_HOST',
	    [
		default('none'),
		query(
		  "If you will cross-compile the engine later, please enter the user\n"||
		  "name and address of the target machine to extract run-time\n"||
		  "characteristics from -- e.g., \"root@my.other.host.com\".  If you\n"||
		  "are not going to crosscompile, leave the default value.\n"||
		  "Cross-compiling is at the moment done with \"make build crossengine\"\n"||
		  "once the regular compilation is completed.", [extended])
	    ]).

% ---------------------------------------------------------------------------

:- export(command_option/1).
command_option := ~stop_command_option(~stop_if_error).
:- use_module(library(lpdist(distutils)), [stop_command_option/2]).

% ---------------------------------------------------------------------------

debug_level := ~get_config_val('DEBUG_LEVEL').
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

% ---------------------------------------------------------------------------

:- doc(section, "Options for CHR").

:- export(with_chr/1).
with_chr := ~get_config_val('WITH_CHR').
ciao_config_entry('WITH_CHR',
	    [
		default(no),
		valid_values([yes, no]),
		query(
"Please specify if you would like to compile Constraint Handling Rules\n"||
"(CHR). This adds significant time to compilation.",
		    [extended]),
		show("Compile CHR", [default, minimum])
	    ]).

% ---------------------------------------------------------------------------

:- doc(section, "Options for CiaoPP").

% TODO: Only used from ./ciaopp/tests/settings.pl (and it should not be necessary)
:- export(ciaoppsrc/1).
ciaoppsrc := ~bundle_description(ciaopp, _, _).

:- export(with_ciaoppcl/1).
with_ciaoppcl := ~get_config_val('WITH_CIAOPPCL').
ciao_config_entry('WITH_CIAOPPCL',
	    [
		default('no'),
		valid_values(['yes', 'no']),
		query(
		    "Please specify if you want to compile the CiaoPP Command Line Utility.",
		    [extended]),
		show("With CiaoPP Command line", [default, minimum])
	    ]).

% ===========================================================================

:- doc(section, "General Compilation Options").

:- use_module(library(gen_asr_file), [gaf/1, gpo/1]).

% TODO: It could be simpler (maybe if metapred were avoided)
% TODO: Maybe it is not the place to put this file
:- export(compiling_options/2).
:- meta_predicate compiling_options(?, list(pred(1))).
compiling_options(Bundle, C) :- compiling_options_(Bundle, C, [gpo]).

:- meta_predicate compiling_options_(?, list(pred(1)), list(pred(1))).
compiling_options_(Bundle) -->
	{ bundle_gen_asr(Bundle, GCA) },
	asr_option(GCA).

% Generate 'asr' files in the compilation of this bundle?
% TODO: why?
bundle_gen_asr(ciao, GCA) :- !, gen_ciao_asr(GCA).
bundle_gen_asr(ciaopp, GCA) :- !, gen_ciaopp_asr(GCA).
bundle_gen_asr(_, no).

:- meta_predicate asr_option(?, list(pred(1)), list(pred(1))).
asr_option(yes, [gaf|T], T).
asr_option(no) --> [].

% TODO: Some of those are passed in the command line
%:- export(gen_ciao_asr/1).
gen_ciao_asr := ~get_config_val('GEN_CIAO_ASR').
ciao_config_entry('GEN_CIAO_ASR',
	    [
		default(yes),
		valid_values([yes, no])
	    ]).

%:- export(gen_ciaopp_asr/1).
gen_ciaopp_asr := ~get_config_val('GEN_CIAOPP_ASR').
ciao_config_entry('GEN_CIAOPP_ASR',
	    [
		default(yes),
		valid_values([yes, no])
	    ]).

% ===========================================================================

:- doc(section, "Configuration of Prolog flags").

ciao_config_entry(FlagName,
	    [
		default(Default)
		% query("Set the given Prolog Flag.", [exended])
		|Options]) :-
	define_flag(FlagNameL, Values, Default),
	\+ ignored_flag(FlagNameL),
	( list(Values) ->
	    Options = [valid_values(Values)|FlagOpts]
	; Options = FlagOpts
	),
	toupper(FlagNameL, FlagName),
	( flag_options(FlagNameL, FlagOpts) ->
	    true
	; FlagOpts = []
	).

% Options that are considered in define_flag:

flag_options(runtime_checks,
	    [
		query(
		    "If you wish to compile the Ciao libraries with runtime checks enabled\n"||
		    "then set the following variable to \"yes\". This of course reduces\n"||
		    "performance.",
		    [extended])
	    ]).
flag_options(compress_lib,
	    [
		query(
		    "If you wish to compile the Ciao libraries with their bytecode\n"||
		    "compressed then set the following variable to \"yes\". Libraries\n"||
		    "generated this way will be smaller at the cost of a slightly slower\n"||
		    "usage, both in their load as when used to create an executable.",
		    [extended])
	    ]).
flag_options(unused_pred_warnings,
	    [
		query(
		    "If you wish to show warnings about unused predicates, set this\n"||
		    "variable to \"yes\".", [extended])
	    ]).

runtime_checks := ~get_config_val('RUNTIME_CHECKS').

% ---------------------------------------------------------------------------

ciao_config_entry('SET_FLAG_OPTIONS',
	    [
		default(no),
		valid_values([yes, no]),
		query(
		    "Set the prolog flags configured here in the ciao shell script.",
		    [extended])
	    ]).

% ---------------------------------------------------------------------------

:- doc(section, "Other Options").

ciaosh_commands := ~get_config_val('CIAOSH_COMMANDS').
ciao_config_entry('CIAOSH_COMMANDS',
	    [
		default(true),
		query(
		    "Pass extra commands to the ciao shell script.",
		    [extended])
	    ]).

% ---------------------------------------------------------------------------

:- doc(section, "Custom (non configurable) Settings").

:- export(gmake/1).
gmake := ~atom_concat(~get_config_val('MAKEDIR'), ~get_config_val('MAKENAME')).
% TODO: Improve notation
ciao_config_entry('MAKEDIR',
	    [
		set_value(get_make(Value, _), Value)
	    ]).

ciao_config_entry('MAKENAME',
	    [
		set_value(get_make(_, Value), Value)
	    ]).

makeop := gmake|make.

get_make(MakeDir, MakeName) :-
	( makeop(MakeName),
	    find_file(MakeName, Path, _) ->
	    atom_concat(Path, '/', MakeDir)
	; error_message("Unable to determine the make utility used."),
	    fail
	).

% ---------------------------------------------------------------------------

% TODO: Improve notation
ciao_config_entry('HAVE_SVNVERSION',
	    [
		default(verify_svnversion(HaveSvnVersion), HaveSvnVersion),
		show_option("svnversion available"),
		valid_values([yes, no])
	    ]).

verify_svnversion(Value) :-
	( have_svnversion -> Value = yes ; Value = no ).

svnversion_base := svnversion.
svnversion_exec := ~exec_names(~svnversion_base).
find_svnversion(File) :- find_paths_file(~svnversion_exec, File).

have_svnversion :- find_svnversion(_).

% ============================================================================

:- doc(section, "Auxiliary Definitions").

:- doc(bug, "Relation between rootprefix/1 and docdir/1 must
   be checked").

:- pred get_env_or_config_val(Name, Value) # "Let us to use
	environment variables to define values.".

get_env_or_config_val(Name, Value) :-
	get_name_value(Name, Value),
	!.
get_env_or_config_val(Name, Value) :-
	get_config_val(Name, Value).

unix_file_value(Name, Value) :-
	get_env_or_config_val(Name, Value0),
	winpath(Value, Value0).

% ---------------------------------------------------------------------------

% TODO: better method?
:- export(call_in_config_options_mod/1).
%:- meta_predicate call_in_config_options_mod(goal).

call_in_config_options_mod(ValMethod) :-
	call(ValMethod).

% ---------------------------------------------------------------------------

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

% TODO: Unused?
% warning_verify_app(App) :-
% 	warning_message(
% 	    "~w not installed.  Is is hightly recommended that\n"||
% 	    "you stop Ciao configuration and install it first."||
% 	    "It is required for the Emacs based IDE.",
% 	    [App]).

% ---------------------------------------------------------------------------

% In the future, the detection of PPL must be implemented here.
exec_names(App, Exec) :-
	get_exec_ext(Ext),
	Ext \== '',
	atom_concat(App, Ext, Exec).
exec_names(App, App).

find_paths_file(Name, File) :- find_file(Name, _, File).

% get_installgroup( InstallGroup ) :-
% 	do_str( [ 'groups 2>/dev/null | cut -f 1 -d \' \'' ],
% 	    nofail, S ),
% 	append( InstallGroupS, "\n", S ),
% 	atom_codes( InstallGroup, InstallGroupS ).

% Find @var{File} in get_paths/1
find_file(File, '', File) :-
	file_exists(File),
	!.
find_file(File, Path, PathFile) :-
	Path = ~get_paths, % note: get_paths is nondet
	PathFile = ~fsR(Path/File),
	file_exists(PathFile),
	!.

get_paths(APath) :-
	getenvstr('PATH', Path),
	extract_paths(Path, PathList),
	member(SPath, PathList),
	atom_codes(APath, SPath).

% ---------------------------------------------------------------------------

:- export(set_configured_flags/0).
set_configured_flags :-
	define_flag(FlagNameL, _, _),
	\+ ignored_flag(FlagNameL),
	toupper(FlagNameL, FlagName),
	set_prolog_flag(FlagNameL, ~get_config_val(FlagName)),
	fail.
set_configured_flags.

:- export(ignored_flag/1).
ignored_flag(optimized_profiler).
ignored_flag(prompt_alternatives_no_bindings).
ignored_flag(regexp_format).
ignored_flag(regexp_exact).

prolog_flag_cmds(TFlagCmds) :-
	set_configured_flags,
	findall(
	    set_prolog_flag(FlagName, Value),
	    (
		define_flag(FlagName, _Values, Default),
		current_prolog_flag(FlagName, Value),
		(Value \== Default)
	    ),
	    TFlagCmds).

% ===========================================================================

% TODO: duplicated in config-and-dump.sh
:- export(ciaoc/1).
ciaoc := ~b_filebuild(plexe, ciao, 'ciaoc').

% TODO: duplicated in config-and-dump.sh
:- export(bootstrap_ciaoc/1).
bootstrap_ciaoc := ~fsR(bundle_src(ciao)/'bootstrap'/'ciaoc.sta').

% setinstalledciao := ~atom_concat(['CIAOALIASPATH= CIAOLIB=', ~rootprefix,
% 	~reallib_dir, ' CIAOENGINE=', ~rootprefix, ~enginedir,
% 	'/ciaoengine.', ~get_platform, ~get_exec_ext]).

% ===========================================================================
% The URL and directory for our main distribution site

% (Not configurable)

:- export(home_url_str/1).
home_url_str := "http://www.ciaohome.org/".
:- export(packages_dir_str/1).
packages_dir_str := "Software/Ciao/packages/trunk/".

