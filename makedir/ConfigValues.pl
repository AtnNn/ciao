:- module(_, _, [assertions, make, dcg, fsyntax, hiord, define_flag, regexp]).

:- use_module(library(system)).
:- use_module(library(terms),      [atom_concat/2]).
:- use_module(library(filenames),  [no_path_file_name/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(distutils)).
:- use_module(library(autoconfig)).
:- use_module(library(format)).
:- use_module(library(regexp(regexp_flags)), []).
:- use_module(library(rtchecks(rtchecks_basic))).
:- use_module(library(gen_asr_file), [gaf/1]).

:- use_module(ciaodesrc(makedir('ConfigMenu'))).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).
:- use_module(ciaodesrc(makedir('DOCCOMMON'))).

% ----------------------------------------------------------------------------
% Settings which are common to makefiles below
% but which you should not need to change
%-----------------------------------------------------------------------------

% :- reexport(ciaodesrc(makedir('CIAODESETTINGS')), [docformat/1]).

staticcompname := 'ciaoc'.

% execmode(Perm) :-
% 	number_codes(Number, 8, ~atom_codes(~current_value(ciaosrcsettings,
% 	    'EXECMODE'))),
% 	convert_permissions(Perm, Number).
% datamode(Perm) :-
% 	number_codes(Number, 8, ~atom_codes(~current_value(ciaosrcsettings,
% 	    'DATAMODE'))),
% 	convert_permissions(Perm, Number).

% Define this to be the main file name (with suffix if there is one???).
mainname := 'ciao'.
% Headers for the executable at startup
header := 'Ciao'.
copyrt := 'UPM-CLIP'.
actualdir := ~no_path_file_name(~component_src(ciao)).
codes_atom(A, B) :- atom_codes(B, A).

basemain := ~no_path_file_name(~mainname).
vers := ~vers(~basemain).
patch := ~patch(~basemain).
versionmain := ~versionmain(~basemain).
vpmain := ~vpmain(~basemain).
win32vpmain := ~win32vpmain(~basemain).


% The Ciao version used
% todo: some definitions here are duplicated in
%       ciaode/ciaosetup_modules/common.sh, ciaode/ciao/Makefile,
%       ciaode/ciao/SHARED

prolog := ~versionmain.
home := ~codes_atom(~getenvstr('HOME')).
builddir := ~atom_concat(~component_src(ciaode), '/build').
srcbindir := ~atom_concat(~builddir, '/bin').
ciaoc := ~atom_concat([~srcbindir, '/ciaoc-', ~vers, ~get_ciao_ext]).
baseciaosh := ~atom_concat(['ciaosh-', ~vers, ~get_ciao_ext]).
ciaosh := ~atom_concat([~srcbindir, '/', ~baseciaosh]).
binciaosh := ~atom_concat([~ciaobindir, '/', ~baseciaosh]).

local_engine := ~atom_concat([~builddir, '/objs/', ~get_platform,
		~get_debug, '/ciaoengine', ~get_exec_ext]).
local_hdir := ~atom_concat([~builddir, '/include/', ~get_platform,
		~get_debug]).

get_lpdoclibdir := ~lpdoclibdir_(~instype).
%	component_ins( lpdoc , LPDOC ),
%	atom_concat( [ LPDOC , '/lib' ] , X ).

lpdoclibbasedir(src) := ~atom_concat(~component_src(lpdoc), '/lib').
lpdoclibbasedir(ins) := ~atom_concat(~ciaolibroot,          '/lpdoc').

lpdoclibsubdir(src) := ''.
lpdoclibsubdir(ins) := ~atom_concat('/', ~versionmain('lpdoc')).

lpdoclibdir_(InsType) := ~atom_concat(~lpdoclibbasedir(InsType),
	    ~lpdoclibsubdir(InsType)).

% ============================================================================
% Constants defined using the configuration values
% ============================================================================

:- doc(bug, "gmake must be replaced by lpmake.").

gmake := [~settings_value('MAKEDIR'), ~settings_value('MAKENAME')].

:- doc(bug, "Relation between build_root/1 and docdir/1 must
   be checked").

%lpdoclib := ~atom_concat([~settings_value('LIBROOT'),'/', 'lpdoc-2.0']).

:- pred env_or_settings_value(Name, Value) # "Let us to use
	environment variables to define values.".

env_or_settings_value(Name, Value) :-
	get_name_value(Name, Value),
	!.
env_or_settings_value(Name, Value) :-
	settings_value(Name, Value).

unix_file_value(Name, Value) :-
	env_or_settings_value(Name, Value0),
	winpath(Value, Value0).

install_prolog_name := ~settings_value('INSTALL_PROLOG_NAME').
compress_libs := ~settings_value('COMPRESS_LIBS').
runtime_checks := ~settings_value('RUNTIME_CHECKS').

lpmake([~atom_concat(~srcbindir, '/'), 'lpmake.sta']).
lpdoc2([~atom_concat(~srcbindir, '/'), 'lpdoc']).

libdir := ~settings_value('LIBDIR').
ciaolibroot := ~settings_value('LIBROOT').
ciaobinroot := ~settings_value('BINROOT').
ciaobindir := ~settings_value('BINDIR').

instype := ~settings_value('INSTYPE').
reallibdir := ~unix_file_value('REALLIBDIR').
install_emacs_support := ~env_or_settings_value('INSTALL_EMACS_SUPPORT').
install_xemacs_support := ~env_or_settings_value('INSTALL_XEMACS_SUPPORT').
emacs_for_ciao := ~env_or_settings_value('EMACS_FOR_CIAO').
xemacs_for_ciao := ~env_or_settings_value('XEMACS_FOR_CIAO').

update_bashrc := ~settings_value('UPDATE_BASHRC').
update_cshrc := ~settings_value('UPDATE_CSHRC').
update_dotemacs := ~settings_value('UPDATE_DOTEMACS').
update_dotxemacs := ~settings_value('UPDATE_DOTXEMACS').
dotbashrc := ~settings_value('DOTBASHRC').
dotcshrc := ~settings_value('DOTCSHRC').
dotemacs := ~settings_value('DOTEMACS').
dotxemacs := ~settings_value('DOTXEMACS').
webimagespath := ~settings_value('WEB_IMAGES_PATH').
webimagesurl := ~settings_value('WEB_IMAGES_URL').
with_mysql := ~settings_value('WITH_MYSQL').
mysql_client_directory := ~settings_value('MYSQL_CLIENT_DIRECTORY').
with_gsl := ~settings_value('WITH_GSL').
with_ppl := ~settings_value('WITH_PPL').
with_java_interface := ~settings_value('WITH_JAVA_INTERFACE').
with_ant := ~settings_value('WITH_ANT').
tabled_execution := ~settings_value('TABLED_EXECUTION').
includeroot := ~settings_value('INCLUDEROOT').

buildreallibdir := ~atom_concat(~build_root, ~reallibdir).
buildlibdir := ~atom_concat(~build_root, ~libdir).
installedincludedir := ~atom_concat([~reallibdir, '/include/',
		~get_platform]).
compile_ciaoppcl := ~settings_value('COMPILE_CIAOPPCL').
optimizing_compiler := ~settings_value('OPTIMIZING_COMPILER').
relreallibdir := ~settings_value('RELREALLIBDIR'). % todo: remove?
enginedir := ~settings_value('ENGINEDIR').
stop_if_error := ~settings_value('STOP_IF_ERROR').
debug_level := ~settings_value('DEBUG_LEVEL').
gen_ciao_asr := ~settings_value('GEN_CIAO_ASR').
gen_ciaopp_asr := ~settings_value('GEN_CIAOPP_ASR').
emacsinitfile := ~settings_value('EMACSINITFILE').
xemacsinitfile := ~settings_value('XEMACSINITFILE').
unused_pred_warnings := ~settings_value('UNUSED_PRED_WARNINGS').
ciaosh_commands := ~settings_value('CIAOSH_COMMANDS').

command_option := ~stop_command_option(~stop_if_error).

:- meta_predicate asr_option(?, list(pred(1)), list(pred(1))).
asr_option(yes, [gaf|T], T).
asr_option(no) --> [].

unittest_option(yes) --> [run_test_module].
unittest_option(no) --> [].

% 	unittest_option(yes). % very slow if tested here, but less fail prone

%% Note that lpdocsrc must be defined in ciao, to avoid errors when
%% ciao is distributed without lpdoc

ciaosrc := ~component_description(ciao, _, _).
ciaoppsrc := ~component_description(ciaopp, _, _).
setlocalciao := ~atom_concat([
		'CIAOALIASPATH= ',
		'CIAOLIB=', ~component_src(ciao), ' ',
		'CIAOHDIR=', ~local_hdir, ' ',
		'CIAOENGINE=', ~local_engine]).

% setinstalledciao := ~atom_concat(['CIAOALIASPATH= CIAOLIB=', ~build_root,
% 	~reallibdir, ' CIAOENGINE=', ~build_root, ~enginedir,
% 	'/ciaoengine.', ~get_platform, ~get_exec_ext]).

flag_black_list(optimized_profiler).
flag_black_list(regexp_format).
flag_black_list(regexp_exact).

set_configured_flags :-
	define_flag(FlagNameL, _, _),
	\+ flag_black_list(FlagNameL),
	toupper(FlagNameL, FlagName),
	set_prolog_flag(FlagNameL, ~settings_value(FlagName)),
	fail.
set_configured_flags.

ciao_extra_commands(ExtraCommands) :-
	sformat(ExtraCommands, "-e '~w'",
	    [~list_to_lits([
			use_module(library(unittest)),
			~ciaosh_commands|~prolog_flag_cmds])]).

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
