:- module(lpdoc, _, [dcg, assertions, regtypes, basicmodes, make, fsyntax]).
%:- module(_,[main/0,main/1],[make,fsyntax,assertions]).

:- doc(title, "The lpdoc Documentation Generator").

:- doc(subtitle, 
	"@em{An Automatic Documentation Generator for (C)LP Systems}").
:- doc(subtitle, "REFERENCE MANUAL").
:- doc(subtitle, "@bf{The Ciao Documentation Series}").
:- doc(subtitle, "@uref{http://www.ciaohome.org/}").
:- doc(subtitle,"@em{Generated/Printed on:} @today{}").
:- doc(subtitle, "Technical Report CLIP 5/97.1-@em{<version below>}").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Jos@'{e} Francisco Morales").

%% Need to add path 
% :- include(library('ClipAddress')).

:- doc(copyright, " Copyright @copyright{} 1996-2011 Manuel
Hermenegildo and Jos@'{e} Francisco Morales.

@include{Copyright.Manuals}
").

:- doc(summary, "@include{README_LPDOC.lpdoc}").

:- doc(module, "@include{Intro.lpdoc}

@section{lpdoc usage}
The following provides the different command line options available
when invoking @apl{lpdoc}. This description is intended only for
advanced users which might like to use @apl{lpdoc} in custom
applications. Note that the normal way to use @apl{lpdoc} is by
setting parameters in an @file{SETTINGS} file (see @ref{Generating a
manual}).

@begin{alert}
TODO: command line options not available here; need
cooperation with lpmake
@end{alert}
").

% @begin{verbatim}
% @includefact{usage_message/1}
% @end{verbatim}

%% ISO Prolog-like modules
:- use_module(library(format)).
:- use_module(library(aggregates)).
:- use_module(library(compiler), [use_module/1]).

%% Ciao packages and libraries
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(messages)).
:- use_module(library(lists), [list_concat/2, append/3]).
:- use_module(library(errhandle), [handle_error/2]).
:- use_module(library(file_utils)).

% :- use_module(library(filenames)).
:- use_module(library(autoconfig)).

%% LPdoc libraries
:- use_module(.(autodoc)).
:- use_module(.(autodoc_structure)).
:- use_module(.(autodoc_settings)).
:- use_module(.(autodoc_filesystem)).
:- use_module(.(autodoc_aux)).

:- use_module(library(distutils(dirutils))).

%% Version information
:- include(lpdocsrc(src(version_auto))).

%% This is to let lpdoc be able to do installation without makefiles:
:- use_module(library(make(make_rt))).

% ===========================================================================

:- doc(section, "(Definitions for this Application)").
:- include(library(lpsettings_based_app)). % TODO: use real interfaces...

% TODO: Many of those definitions should come from the documentation
% of this file.

% The application name
app_name(lpdoc).
% Version atom
%% version(_)
% Copyright
app_copyright("CLIP Group, T.U. of Madrid (UPM)").

app_options_message("
LPdoc options:

-cv,--comment-version The source files contain version information.
                If not specified lpdoc will asume the opposite.

-c FILE         Process FILE as a separate (stand-alone) component.
").

is_option0('-cv').
handle_option0('-cv') :- do_comment_version.
%
is_option0('--comment-version').
handle_option0('--comment-version') :- do_comment_version.
do_comment_version :-
	retractall_fact(lpdoc_option('-ncv')),
	assertz_fact(lpdoc_option('-cv')).

% ===========================================================================

:- doc(section, "Processing Documentation Targets").

start(Targets) :-
	clean_fs_db,
	reset_output_dir_db,
	ensure_doc_build_root,
	load_vpaths,
	parse_structure,
	process_targets(Targets).

% ---------------------------------------------------------------------------

ensure_doc_build_root :-
	% If no build_root is specified, the output is the same
	% dir in which lpdoc is executing
	% TODO: Unused?
	( setting_value(doc_build_root, _) ->
	    true
	; get_cwd(CWD),
	  add_name_value(doc_build_root, CWD)
	),
	% Define lpdoclib
	% TODO: This is defined somewhere else. Make sure that it works 
	% in all installation types.
	LpDocLibDir = ~atom_concat([~component_src(lpdoc), '/lib/']),
	add_name_value(lpdoclib, LpDocLibDir).
	% TODO: I am not sure that this is the best way to do it;
	%   maybe resources have to be specified individually

% ---------------------------------------------------------------------------
% Process the targets

process_targets([]) :- !.
process_targets(['-c', Target|Targets]) :- !,
	process_standalone(Target),
	process_targets(Targets).
process_targets([Target|Targets]) :- !,
	process_target(Target),
	process_targets(Targets).

process_target(Target) :-
	report_make_target('Starting', Target),
	make(Target),
	report_make_target('Finished', Target).

%% Treat Target as a separated component
process_standalone(Target) :-
	base_from_target(Target, Base),
	report_make_target('Starting', Base),
	standalone_docstr(Base),
	( make(Target) -> Ok = yes ; Ok = no ),
	clean_docstr,
	Ok = yes,
	report_make_target('Finished', Base).

% Obtain the name of a target (by removing the suffix, which must be a
% supported one)
base_from_target(Target) := Base :-
	( supported_file_format(Suffix),
	  atom_concat([Base, '.', Suffix], Target) ->
	    true
	; Base = Target
	).

report_make_target(BegEnd, Ext) :-
	file_format_name(Ext, FormatName),
	!,
	bold_message("~w manual generation in ~w (~w) format.",
	    [BegEnd, Ext, FormatName]).
report_make_target(BegEnd, Base) :-
	bold_message("~w processing of '~w'.", [BegEnd, Base]).

% ===========================================================================

:- doc(section, "Command Entries (lpmake)").

%% ---------------------------------------------------------------------------
% target_comment(all,       "Generate manual in all default formats", []).
%% ---------------------------------------------------------------------------

% Note: do not confuse a file format (e.g. PDF) with a backend (texinfo)

% Generate all the requested file formats (in SETTINGS.pl)
all <- ~requested_file_formats
# "Generates all documentation files specified by @pred{docformat/1}." :- true.

% Generate one file format (not necessarily requested in SETTINGS.pl)
~supported_file_format <- ~main_absfile_in_format(~top_suffix(Suffix)) :: Suffix :- true.

% ---------------------------------------------------------------------------

:- doc(subsection, "Rules for Documentation Generation").
% Schematically, there are the rules that defines how documentation is
% generated for a specific backend. Let A be a file, Main the mainmod
% and Ci the components:
%
%   1) dr(A) <- source(A)
%   2) gr(Main) <- [dr(Main), dr(C1),...,dr(Cn)]
%   3) cr(A) <- [gr(Main),dr(A)]
%   4) fr(Main) <- [cr(Main),cr(C1),...,cr(Cn)]

% 1) Doctree and references from each source
~absfile_for_subtarget(~get_name(~dupspec(Spec)), ~dupft(F), dr) <-
	    [~query_source(Spec, S)] :-
	gen_doctree(F, Spec, S).

% 2) Globally resolved references
~main_absfile_for_subtarget(~dupft(F), gr) <-
	    [~main_absfile_for_subtarget(F, dr)|~components_target(F, dr)] :-
	compute_grefs(F).

% 3) Backend-specific temporal result
~absfile_for_subtarget(~get_name(~dupspec(Spec)), ~dupft(F), cr) <-
	    [~absfile_for_subtarget(~get_name(Spec), F, dr), ~main_absfile_for_subtarget(F, gr)] :-
	% note: Base here is a modname (not a modspec)
	translate_doctree(F, ~get_name(Spec)).

% 4) Backend-specific final result
~main_absfile_for_subtarget(~dupft(F), fr) <-
	    [~main_absfile_for_subtarget(F, cr)|~components_target(F, cr)] :-
	bold_message("Post-processing the document.", []),
	autodoc_finish(F).

% (extra) Alternative final results (e.g. PDF, PS, etc.)
% [Rules for generating DVI, PS, and PDF from texi]
~main_absfile_in_format(~dup_alt_format(Alt, F)) <-
	    [~main_absfile_for_subtarget(F, fr)] :-
	bold_message("Generating document in ~w format.", [Alt]),
	autodoc_gen_alternative(F, Alt).

% @var{Alt} is an alternative format for backend @var{F}
dup_alt_format(Alt, F) := ~dup(~backend_alt_format(~dupft(F)), Alt).

% ---------------------------------------------------------------------------

:- doc(subsection, "Some Auxiliary Definitions").
% (for the rules above)

components_target(Backend, _Subtarget, []) :- backend_ignores_components(Backend), !.
components_target(Backend, Subtarget, CompTargets) :-
	CompTargets = ~target_files(~all_component_specs, Backend, Subtarget).

target_files([],           _, _) := [].
target_files([FileBase|FileBases], Backend, Subtarget) := [NewFile|NewFiles] :-
	get_name(FileBase, Name),
	NewFile = ~absfile_for_subtarget(Name, Backend, Subtarget),
	NewFiles = ~target_files(FileBases, Backend, Subtarget).

% Unify arguments and return them. This is useful to define generic
% rules that work for several formats and source suffixes.
% TODO: This trick is necessary because of how the fsyntax package
%       currently interacts with the lpmake package. This should
%       be reviewed carefully. --JF
dup(X,X) := X.

dupft(F) := ~dup(~backend_id,F).

query_source(Spec, S) := Main :-
	% Find the first source that exists (e.g., .pl or .lpdoc)
	S = ~srcsuff,
	Main = ~atom_concat([Spec, '.', S]),
%	catch(absolute_file_name(library(Main), _), _, fail),
	% TODO: Cannot use absolute_file_name, library_directory is
	%       not updated, only vpath.
	find_file(Main, _),
	!.

dupspec(Spec) := ~dup(~all_specs, Spec).

all_specs := B :-
	( B = ~get_mainmod_spec
	; Bs = ~all_component_specs,
	  member(B, Bs)
	).

% TODO: I am not sure if here is the place to define this.
srcsuff := pl | lpdoc.

% ---------------------------------------------------------------------------

:- doc(subsection, "Steps of Documentation Generation").

gen_doctree(Backend, FileBase, SourceSuffix) :-
	ensure_cache_dir(Backend),
	get_name(FileBase, Name),
%	display(user_error, generating_doctree(Backend, Name)), nl(user_error),
	get_autodoc_opts(Backend, Name, Opts),
	autodoc_gen_doctree(Backend, FileBase, SourceSuffix, Opts, Name).

compute_grefs(Backend) :-
	Mod = ~get_mainmod,
	get_autodoc_opts(Backend, Mod, Opts),
%	display(user_error, computing_grefs(Backend, Mod)), nl(user_error),
	autodoc_compute_grefs(Backend, Mod, Opts).

translate_doctree(Backend, FileBase) :-
	get_name(FileBase, Base),
%	display(user_error, translating_doctree(Backend, Base)), nl(user_error),
	get_autodoc_opts(Backend, Base, Opts),
	ensure_output_dir_prepared(Backend, Opts),
	autodoc_translate_doctree(Backend, Opts, Base).

%% ===========================================================================

:- doc(section, "Commands for Output Visualization").

%% Names and paths of external commands used by lpdoc and other paths
%% which get stored in the executable on installation:
:- use_module(lpdocsrc(makedir('LPDOCSETTINGS')), [
		xdvi/1, xdvisize/1, htmlview/1, pdfview/1, psview/1]).

% Default 
view <- [htmlview] # "Visualize default format (.html)" :- true.

% TODO: May not work; deprecate?
dviview <- [] # "Visualize .dvi (with xdvi)" :-
	main_absfile_in_format('dvi', File),
	sh_exec([~xdvi, ' -s ', ~xdvisize, ' -expert -geometry 623x879-0-0 ', File], [default, bg]).

% TODO: May not work; deprecate?
svgaview <- [] # "Visualize .dvi (with xdvi) (svga screen)" :-
	main_absfile_in_format('dvi', File),
	sh_exec([~xdvi, ' -s 8 -expert -geometry 643x590+1280+0 ', File], [default, bg]).

% TODO: May not work; deprecate?
xgaview <- [] # "Visualize .dvi (with xdvi) (xga screen)" :-
	main_absfile_in_format('dvi', File),
	sh_exec([~xdvi, ' -s 8 -expert -geometry 643x759-0-0 ', File], [default, bg]).

% % TODO: May not work
% psview <- [] # "Visualize .ps (with ghostscript)" :-
%       ...
% 	sh_exec([~ghostview, ' -magstep -1 -portrait -geometry +349+10 ',
% 		OutputBase, '.ps'], [default, bg]).

pdfview <- [] # "Visualize .PDF (with a default viewer)" :-
	view('pdf').

psview <- [] # "Visualize .PS (with a default viewer)" :-
	view('ps').

htmlview <- [] # "Visualize .html (with a default viewer)" :-
	view('html').

view(Suffix) :-
	main_absfile_in_format(Suffix, File),
	view_document(Suffix, File).

view_document(Suffix, File) :-
	viewer(Suffix, App, Mode),
	( Mode = fg -> Opts = [] ; Opts = [bg] ),
	sh_exec([App, ' ', File], [default|Opts]).

% The viewer application for a given file format
% viewer(Suffix, App, Mode):
%   Mode = fg (call in foreground) or bg (call in background)
viewer('html', 'open', fg) :- get_os('DARWIN'), !.
viewer('pdf', 'open', fg) :- get_os('DARWIN'), !.
viewer('ps', 'open', fg) :- get_os('DARWIN'), !.
viewer('html', App, bg) :- App = ~htmlview, !.
viewer('pdf', App, bg) :- App = ~pdfview, !.
viewer('ps', App, bg) :- App = ~psview, !.

:- use_module(engine(system_info), [get_os/1]).

% TODO: This seems to be done by the emacs mode...
% lpsettings <- [] # "Generates default LPSETTINGS.pl in the current directory"
% 	:-
% 	get_cwd(CWD),
% 	generate_default_lpsettings_file(CWD, '').

%% ===========================================================================

:- doc(section, "Cleaning up Commands").
% (after generating manuals)

clean <- [] # "Delete intermediate files. Leaves .texi & docs" :-
	clean_intermediate.

docsclean <- [] # "Delete all generated document files (temporal and final). Leaves only .texi" :-
	clean_docs_no_texi.

%% distclean <- [clean] # "Leaves only generated docs." :- 
distclean <- [] # "Delete all temporal files, including .texi" :-
	clean_all_temporal.

%% realclean <- [docsclean] # "Deletes everything." :-
%% 	-delete_files(~ls('*.texic')).
realclean <- [] # "Deletes everything." :-
	clean_all.





