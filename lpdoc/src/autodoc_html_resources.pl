:- module(_, [], [assertions, fsyntax]).

:- doc(title, "Resource Handling for the HTML Backend").
:- doc(author, "Jose F. Morales").

:- use_module(library(messages)).
:- use_module(library(file_utils)).
:- use_module(library(system_extra)).

%:- include(library(pillow(ops))).

%:- use_module(library(pillow(html)), [html_template/3]).

:- use_module(lpdocsrc(src(autodoc))).
:- use_module(lpdocsrc(src(autodoc_settings))).
:- use_module(lpdocsrc(src(autodoc_filesystem))).

% ---------------------------------------------------------------------------
% Copy the website skel (images, css, etc.)

:- use_module(library(dirutils), 
   [path_name/2, copy_dir_rec/9, set_owner_rec/2, get_abs_path/2]).

:- export(prepare_web_skel/1).
% TODO: Avoid copy if not necessary
prepare_web_skel(SrcDir) :-
	check_setting(htmldir),
	%
	HtmlDir = ~setting_value(htmldir),
	Owner = ~setting_value_or_default(owner),
	Group = ~setting_value_or_default(group),
	Perms = ~setting_value_or_default(perms),
	%
	( file_exists(SrcDir) ->
	    true
	; error_message("No website skeleton found at '~w'", [SrcDir]),
	  fail
	),
	copy_website_skel(SrcDir, HtmlDir, Perms, Owner, Group).

:- pred copy_website_skel(SrcDir, DestDir, Perm, Owner, Group)
# "Copies recursively the web skel directory into @var{DestDir}.".
% TODO: See makedir_SHARED
copy_website_skel(SrcDir, DestDir, Perm, Owner, Group) :-
	copy_dir_rec(SrcDir, DestDir, Perm, '*', '*~', '.svn', '', [],
	    [overwrite, timestamp]),
	-set_owner_rec(DestDir, grp(Owner, Group)).

% ---------------------------------------------------------------------------

:- export(prepare_mathjax/0).
prepare_mathjax :-
	detect_mathjax,
	( found_mathjax(JS) ->
	    % Create a symlink to MathJax (see @pred{using_mathtax})
	    atom_concat(JSDir, '/MathJax.js', JS),
	    absfile_for_aux('MathJax', html, JSLink),
	    copy_file(JSDir, JSLink, [overwrite, symlink])
	; true
	).

% ---------------------------------------------------------------------------

:- export(using_mathjax/1).
% Path to the MathJax.js file (it may be relative to the document path).
%
% Note: the path to MathJax in the HTML file can be relative; making
% it work from the web and filesystem.
% TODO: This may not work in all cases, but avoids cumbersome
%       configurations.
using_mathjax(JS) :-
	( found_mathjax(_) ->
	    % Uses the symbolic link created in @pred{prepare_mathjax}
	    JS = 'MathJax/MathJax.js'
	; fail
	).

:- data found_mathjax/1.

detect_mathjax :-
	retractall_fact(found_mathjax(_)),
	( find_mathjax(JS) ->
	    % MathJax.js was found
	    assertz_fact(found_mathjax(JS))
	; no_mathjax_message
        ).

no_mathjax_message :-
	note_message(
             "No MathJax detected. In order to view formulas in the HTML output, "||
             "please install MathJax 1.1 under your public_html/ directory."||
             "(http://www.mathjax.org/download/)").

% (fails if no mathjax.js is found)
% TODO: This is ad-hoc
find_mathjax(JS) :-
	Owner = ~setting_value_or_default(owner),
	( JS0 = ~atom_concat(['/Users/', Owner, '/public_html/MathJax/MathJax.js'])
	; JS0 = ~atom_concat(['/home/', Owner, '/public_html/MathJax/MathJax.js'])
	),
	file_exists(JS0), !,
        JS = JS0.

:- use_module(library(terms), [atom_concat/2]).

% ---------------------------------------------------------------------------
