:- module(autodoc_settings, [], [dcg, assertions, regtypes, fsyntax]). 

:- doc(title,"Access to Default Settings").
:- doc(author,"Jose F. Morales").

:- doc(module, "
	This module defines the setting values with some default values.

@begin{alert}   
@bf{Note: This part needs better documentation. -- JFMC}
@end{alert}
   ").

% ===========================================================================

:- doc(section, "Checking or Setting Options").

:- use_module(library(system_extra)).
:- use_module(library(make(make_rt))).

:- export(lpdoc_option/1).
:- doc(lpdoc_option/1, "Defines the global options of lpdoc.").
:- data lpdoc_option/1.
lpdoc_option('-ncv').

% verify that the loaded settings implement SETTINGS_schema
:- export(verify_settings/0).
verify_settings :-
	( setting_value('$schema', 'SETTINGS_schema') ->
	    true
	; throw(make_error("The settings file does not seem to be including SETTINGS_schema", []))
	).

:- export(check_setting/1).
check_setting(Name) :- check_var_exists(Name).

% (With implicit default value)
:- export(setting_value_or_default/2).
:- pred setting_value_or_default(Var, Value)
# "Returns in @var{Value} the value of the variable @var{Var}. In case
  this variable does not exists, it returns a default value. If there
  is no default value for the variable @var{Var} it fails.".

setting_value_or_default(Name, Value) :-
	( make_rt:get_value(Name, Value0) ->
	    Value = Value0
	; Value = ~default_val(Name)
	).

default_val(startpage) := 1.
default_val(papertype) := afourpaper.
default_val(perms) := perm(rwX, rX, rX).
default_val(owner) := ~get_pwnam.
default_val(group) := G :- ( G = ~get_grnam -> true ; G = 'unknown' ).

% (With explicit default value)
:- export(setting_value_or_default/3).
setting_value_or_default(Name, DefValue, Value) :-
	( make_rt:get_value(Name, Value0) ->
	    Value = Value0
	; Value = DefValue
	).

:- export(setting_value/2).
setting_value(Name, Value) :-
	make_rt:get_value(Name, Value).

:- export(all_setting_values/2).
%all_setting_values(Name) := ~findall(T, ~setting_value(doc_mainopt)).
all_setting_values(X) := ~findall(T, setting_value(X, T)) :-
	( X = doc_mainopts ; X = doc_compopts ), !. % TODO: all_values fail if empty?!
all_setting_values(Name) := ~all_values(Name).

:- use_module(library(aggregates)).

:- export(get_command_option/1).
% TODO: Document?
get_command_option([C]) :-
	make_rt:get_value(stop_if_error, V),
	stop_command_option(V, C),
	!.
get_command_option([nofail, silent, show_error_on_error|A]) :-
	( current_fact(make_option('-v')) ->
	    A = [verbose]
	; A = []
	).
%get_command_option := exception.

:- use_module(library(lpdist(distutils)), [stop_command_option/2]).

:- export(requested_file_formats/1).
:- pred requested_file_formats(F) # "@var{F} is a requested file format".
requested_file_formats := F :-
	F = ~all_values(docformat).

% ===========================================================================

:- doc(section, "Paths to Code").

:- export(load_vpaths/0).
load_vpaths :-
	load_filepath,
	load_systempath.

load_filepath :-
	( setting_value(filepath, P),
	  add_vpath(P),
	  verbose_message("Added file path: ~w", [P]),
	  fail
	; true
	).

load_systempath :-
	( setting_value(systempath, P),
	  add_vpath(P),
	  verbose_message("Added system path: ~w", [P]),
	  fail
	; true
	).

% ===========================================================================

:- doc(section, "External Commands").
% TODO: Ideally, each backend should specify this part.

:- doc(subsection, "Visualization of Documents").
% TODO: These commands were originally customizable by the
%       user. Nowadays, configuration files are not easy to find... It
%       is lpdoc task to determine what application to use
%       automatically based on the operating system.

:- use_module(engine(system_info), [get_os/1]).

:- export(viewer/4).
% The viewer application for a given file format
% viewer(Suffix, App, Mode):
%   Mode = fg (call in foreground) or bg (call in background)
% -- Default viewer for MacOS X
viewer('html', 'open "', '"', fg) :- get_os('DARWIN'), !.
viewer('pdf', 'open "', '"', fg) :- get_os('DARWIN'), !.
% viewer('pdf', 'emacsclient -n "', '"', fg) :- get_os('DARWIN'), !.
viewer('ps', 'open "', '"', fg) :- get_os('DARWIN'), !.
viewer('manl', 'emacsclient -n --eval ''(man "./', '")''', fg) :- get_os('DARWIN'), !.
% -- Default viewer for Windows
viewer('html', 'cygstart "', '"', fg) :- get_os('Win32'), !.
viewer('pdf', 'cygstart "', '"', fg) :- get_os('Win32'), !.
% viewer('pdf', 'emacsclient -n "', '"', fg) :- get_os('Win32'), !.
viewer('ps', 'cygstart "', '"', fg) :- get_os('Win32'), !.
viewer('manl', 'emacsclient -n --eval ''(man "./', '")''', fg) :- get_os('Win32'), !.
%viewer('html', 'start "', '"', fg) :- get_os('Win32'), !.
%viewer('pdf', 'start "', '"', fg) :- get_os('Win32'), !.
%viewer('ps', 'start "', '"', fg) :- get_os('Win32'), !.
% -- Default viewer for LINUX
viewer('html', 'xdg-open "', '"', fg) :- get_os('LINUX'), !.
viewer('pdf', 'xdg-open "', '"', fg) :- get_os('LINUX'), !.
% viewer('pdf', 'emacsclient -n "', '"', fg) :- get_os('LINUX'), !.
viewer('ps', 'xdg-open "', '"', fg) :- get_os('LINUX'), !.
viewer('manl', 'emacsclient -n --eval ''(man "./', '")''', fg) :- get_os('LINUX'), !.
% -- Viewer for info files (assume emacs for all systems)
viewer('info', 'emacsclient -n ', '"', fg) :- !.
% -- Other default viewers (probably, this will not work)
viewer('html', 'see "', '"', bg) :- !.
% viewer('pdf', 'see "', '"', bg) :- !.
viewer('pdf', 'emacsclient -n "', '"', fg) :- get_os('Win32'), !.
viewer('ps', 'see "', '"', bg) :- !.
viewer('manl', 'emacsclient -n --eval ''(man "./', '")''', fg) :- !.

% TODO: This seems to be done by the emacs mode...
% lpsettings <- [] # "Generates default LPSETTINGS.pl in the current directory"
% 	:-
% 	working_directory(CWD0, CWD0),
%       path_name(CWD0, CWD),
% 	generate_default_lpsettings_file(CWD, '').

%% The command that views dvi files in your system
:- export(xdvi/1).
xdvi := 'xdvi'.

%% The default size at which manuals are viewed This
%% is typically an integer (1-10 usually) and unfortunately changes
%% depending on the version of xdvi used.
:- export(xdvisize/1).
xdvisize := '8'.

:- doc(subsection, "Bibliography Generation").

%% The command that builds .bbl files from .bib bibliography
%% files in your system
:- export(bibtex/1).
bibtex := 'bibtex'.

:- doc(subsection, "Texinfo Related Commands").

%% Define this to be the command that runs tex in your system
:- export(tex/1).
tex := 'tex'.

%% Alternative (sometimes smarter about number of times it needs to run):
%% tex := 'texi2dvi '.
%% (but insists on checking the links, which is a pain...)

%% The command that runs texindex in your system
%% (Not needed if texi2dvi is installed)
:- export(texindex/1).
texindex := 'texindex'.

%% The command that converts dvi to postscript in your system. Make
%% sure it generates postscript fonts, not bitmaps (selecting -Ppdf
%% often does the trick). -z preserves hypertext links.
:- export(dvips/1).
dvips := 'dvips -z -Ppdf'.

%% The command that converts postscript to pdf in your system. Make
%% sure it generates postscript fonts, not bitmaps (selecting -Ppdf in
%% dvips often does the trick)
:- export(ps2pdf/1).
ps2pdf := 'ps2pdf'.

%% The command that converts tex to pdf in your system
%% texpdf := 'pdftex'.

%% The command that converts texinfo files into info
%% files in your system. Set also the appropriate flags.
:- export(makeinfo/1).
makeinfo := 'makeinfo'.

%% The command that converts .texi files into .rtf files
% TODO: This may be obsolete, keep anyway
:- export(makertf/1).
makertf := 'makertf'.

%% The command that converts .rtf files into Win32 .HLP files
% TODO: This may be obsolete, keep anyway
:- export(rtftohlp/1).
rtftohlp := 'hc31'.

:- doc(subsection, "Image Conversions").

%% The command that converts graphics files to other formats
:- export(convertc/1).
convertc := 'convert'.

