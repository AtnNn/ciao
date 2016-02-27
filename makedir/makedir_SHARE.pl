:- use_module(library(aggregates), [findall/3]).
:- use_module(library(distutils(readme_generator))).

:- use_module(library(component_registry), [component_src/2]).

% Temporal solution to share code from component's makedir

% INTERFACE OF THIS TEMPLATE

% (Rule)
%   component_all <- ... # "component 'all' command"
% 
% component_id(Id) # "@var{Id} is the identifier of the component".
% component_dname(X) # "@var{X} is the textual name of the component".
% component_readme_dir(D) # "@var{D} is the subdirectory where the
%   READMEs are found (in lpdoc format)".
% component_readme(X) # "@var{X} is a file description (see code for
%   more details) of each README".
% component_manual_dir(D) # "@var{D} is the subdirectory where each
%   manual is found (containing a SETTINGS.pl file)".

% ---------------------------------------------------------------------------
% Compilation 
% TODO: and more...

all <- [compiling_message,
	component_all,
	compilation_done_message] :- true.

compiling_message <- :-
	component_dname(CompName),
	bold_message("Compiling ~w", [CompName]).

compilation_done_message <- :-
	component_dname(CompName),
	bold_message("~w compilation completed", [CompName]).

% ---------------------------------------------------------------------------
% Installation

install <- [component_install, component_register] :- true.
uninstall <- [component_uninstall, component_unregister] :- true.

% ---------------------------------------------------------------------------

docs_readmes <- [] # "Creation of README files" :- docs_readmes.
docs_readmes :-
	component_readme_dir(SrcDir0), !,
	component_id(Id),
	ComponentSrc = ~component_src(Id),
	compose_dir(ComponentSrc, SrcDir0, SrcDir),
	BuildDocDir = ~build_doc_dir,
	findall(File, component_readme(File), Files),
	generate_readme_files(Files, SrcDir, BuildDocDir).
docs_readmes. % no readmes

docs_manuals <- [] # "Creates the manuals." :-
	docs_manuals.
docs_manuals :-
	( % (failure-driven loop)
	  component_manual_dir(SrcDir0),
	    component_id(Id),
	    ComponentSrc = ~component_src(Id),
	    compose_dir(ComponentSrc, SrcDir0, SrcDir),
	    invoke_lpdoc(~atom_concat(SrcDir, '/SETTINGS'), all),
	    fail
	; true
	).

% TODO: defined elsewhere?
compose_dir(Base, Subdir, Dir) :-
	( Subdir = '' ->
	    Dir = Base
	; Dir = ~atom_concat([Base, '/', Subdir])
	).

% ---------------------------------------------------------------------------
