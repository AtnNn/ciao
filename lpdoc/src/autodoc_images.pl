:- module(autodoc_images, [], [dcg, assertions, regtypes, fsyntax]). 

:- doc(title,"Image Handling").
:- doc(author,"Jose F. Morales").

:- doc(module, "
	This module defines the handling of image commands.
        It defines predicates to locate and convert images in the
	different formats required for documentation.

@begin{alert}   
@bf{Note: This part needs better documentation. -- JFMC}
@end{alert}
   ").

:- use_module(library(terms), [atom_concat/2]).

:- use_module(lpdocsrc(src(autodoc))).
:- use_module(lpdocsrc(src(autodoc_filesystem))).
:- use_module(lpdocsrc(src(autodoc_settings))).
:- use_module(lpdocsrc(src(autodoc_aux))).

% TODO: temporary?
:- use_module(library(make(make_rt)), [verbose_message/1, verbose_message/2]).
:- use_module(library(make(system_extra)), [(-) /1]).
:- use_module(library(system), [copy_file/3]).
:- use_module(library(make(make_rt)), [get_name/2]).
:- use_module(library(errhandle), [error_protect/1]).
:- use_module(library(messages)).

% ---------------------------------------------------------------------------
:- use_module(library(format)).

% Format an image command
:- export(locate_and_convert_image/4).
% TODO: Allow file specs in ImageSpecS (see spec_add_suffix/3)
% TODO: directory output for target is missing
:- pred locate_and_convert_image(SrcSpecS, AcceptedFormats, DocSt, TargetFileS) ::
	string * list(atom) * docstate * string # 
        "The image at @var{SrcSpecS} is located (as one of the known
         formats @pred{known_format/1}) and converted to one of the
         @var{AcceptedFormats}. The target file is called
         @var{TargetFileS}".

locate_and_convert_image(SrcSpecS, AcceptedFormats, DocSt, TargetFileS) :-
	atom_codes(SrcSpec, SrcSpecS),
	% TODO: Use the same rules than for modules to locate the images
	( known_format(SrcExt), % (may backtrack)
	  %
	  spec_add_suffix(SrcSpec, SrcExt, SrcSpecExt),
	  error_protect(absolute_file_name(library(SrcSpecExt), SrcFile)) ->
	    % Image found!
	    atom_concat([SrcBase, '.', SrcExt], SrcFile),
	    % Determine the target format
	    ( member(SrcExt, AcceptedFormats) ->
	        % The source format is accepted, keep it
	        TargetFormat = SrcExt
	    ; % Otherwise, use the first accepted format
	      % TODO: This should be done in the image_convert predicate
              %       to find the more optimal conversion
	      AcceptedFormats = [TargetFormat|_]
	    ),
	    % Determine the target file name
	    get_name(SrcBase, SrcName),
	    atom_concat('autofig', SrcName, TargetBase),
	    atom_concat([TargetBase, '.', TargetFormat], TargetFile),
	    % Convert the image
	    doc_message("-> Including image ~w in documentation as ~w", [SrcFile, TargetFile], DocSt),
	    %format(user_error, "-> Including image ~w in documentation as ~w~n", [SrcFile, TargetFile]),
	    % ( verbose_message("Converting/Copying file from ~w to ~w", [SrcFile, TargetFile]),
	    image_convert(SrcBase, SrcExt, TargetBase, TargetFormat, DocSt),
	    %
	    atom_codes(TargetFile, TargetFileS)
	; error_message("-> Image ~w not found in any known format", [SrcSpec]),
	  fail
	).

% Known formats
% TODO: extend?
known_format('eps').
known_format('png').
known_format('jpg').

% TODO: extend so that precondition atom(SrcSpec) can be generalized
spec_add_suffix(SrcSpec, SrcExt, SrcSpecExt) :-
	atom_concat([SrcSpec, '.', SrcExt], SrcSpecExt).

%% Names and paths of external commands used by lpdoc and other paths
%% which get stored in the executable on installation:
:- use_module(lpdocsrc(makedir('LPDOCSETTINGS')), [convertc/1]).
:- use_module(library(make(system_extra)),
		[del_file_nofail/1,
		do/2,
		set_perms/2]).

image_convert(SrcBase, SrcExt, TargetBase, TargetExt, DocSt) :-
	atom_concat([SrcBase, '.', SrcExt], Source),
	atom_concat([TargetBase, '.', TargetExt], Target),
	%% Deprecate use of 'pstogif' ('convert' is better)
        %%( TargetExt = 'gif' ->
	%%  do([~pstogif, ' ', Source], []),
	%%  -del_file_nofail(~atom_concat([SrcBase, '.ppm'])),
	%%  -del_file_nofail(~atom_concat([SrcBase, '.ppm.tmp']))
	%%; TargetExt = 'ppm' ->
	%%    do([~pstogif, ' ', Source], []),
	%%    -del_file_nofail(~atom_concat(SrcBase, '.gif')),
	%%    -del_file_nofail(~atom_concat(SrcBase, '.ppm.tmp'))
	%%;
	docstate_backend(DocSt, Backend),
	absfile_for_aux(Target, Backend, AbsFile),
	( SrcExt = TargetExt ->
	    % same format, just copy
	    -(copy_file(Source, AbsFile, [overwrite]))
	; TargetExt = 'txt' ->
	    % TODO: This is a dummy output (necessary?)
	    open(Target, write, O),
	    format(O, "~n[Image file: ~w.eps]~n", [SrcBase]),
	    close(O)
        ; % TODO: use other commands?
          sh_exec([~convertc, ' ', Source, ' ', AbsFile], [])
%	; throw(unknown_target_ext(TargetExt))
	),
	DataMode = ~setting_value_or_default(perms),
	-set_perms(AbsFile, DataMode).

%% This is a command that converts .eps files into .gif and .ppm files
%% (the -debug option of pstogif does this!)
%% 
% pstogif := 'pstogif -debug'.

% image_convert(ppm, jpg, SrcBase) :- !,
% 	atom_concat([SrcBase,'.jpg'],Target),
% 	do([~ppmtojpeg,' ',SrcBase,'.ppm > ',Target],[]),
% 	-(set_perms(Target,~get_datamode)).
%
%% This is a command that converts .ppm files into .jpg files on stdout
%% 
%% ppmtojpeg := 'cjpeg -progressive'.

% ---------------------------------------------------------------------------

:- doc(bug, "Image conversion can be improved to skip .eps and
accept more sources. E.g., tikz input, etc.").
