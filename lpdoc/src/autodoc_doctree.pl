:- module(autodoc_doctree, [], [dcg, assertions, regtypes, fsyntax]). 

:- doc(author,"Manuel Hermenegildo (original version)").
:- doc(author,"Jose F. Morales").

:- doc(title,"Documentation Abstract Syntax Tree").

:- doc(module, "This module defines the intermediate tree
   representation @regtype{doctree/1} for documentation and its
   related operations.

@begin{alert}   
@bf{Note: This part needs better documentation. -- JFMC}
@end{alert}
   ").

% ISO-Prolog compatibility libraries
:- use_module(library(write), [write/2, writeq/2, write_term/3]).

% Other libraries
:- use_module(library(operators)).
:- use_module(library(format)).

% Local libraries
:- use_module(lpdocsrc(src(autodoc))).
:- use_module(lpdocsrc(src(comments)), [version_descriptor/1, docstring/1,
	stringcommand/1]).

:- use_module(lpdocsrc(src(autodoc_refsdb))).
:- use_module(lpdocsrc(src(autodoc_index))).
:- use_module(lpdocsrc(src(autodoc_structure))).
:- use_module(lpdocsrc(src(autodoc_filesystem))).
:- use_module(lpdocsrc(src(autodoc_settings))).

% The backends
:- use_module(lpdocsrc(src(autodoc_texinfo)), []).
:- use_module(lpdocsrc(src(autodoc_man)),     []).
:- use_module(lpdocsrc(src(autodoc_html)),     []).

% ===========================================================================

:- doc(section, "Definition of LPdoc Commands").

% We provide types for each lpdoc command argument, as follows:
%
%    +------+--------------------+-------------------------------------+
%    | Name | Parsed (ciao) type | Notes                               |
%    +------+--------------------+-------------------------------------+ 
%    | d    | doctree            | Parsable, recursively rewritable    |
%    | s    | string             | Parsable                            |
%    | p    | term/term          | Parsable                            |
%    | t    | term               | Not parsable, for internal commands |
%    | td   | term               | A doctree for internal commands     |
%    +------+--------------------+-------------------------------------+
%
% This type information is used to parse a docstring into a doctree
% and to do recursive rewriting of commands.

% Valid lpdoc user commands 
:- export(cmd_type/1).
cmd_type(comment(s)) :- !.
cmd_type(include(s)) :- !.
cmd_type(includeverbatim(s)) :- !.
cmd_type(includefact(p)) :- !.
cmd_type(includedef(p)) :- !.
cmd_type(cite(s)) :- !.
% the following are translated during parsing to idx_env
cmd_type(index(d)) :- !.
cmd_type(cindex(d)) :- !.
cmd_type(concept(d)) :- !.
cmd_type(file(d)) :- !.
cmd_type(Command) :-
	Command =.. [Type, X], codetype(Type), !,
	X = d.
% translated during parsing to section_env
cmd_type(section(d)) :- !.
cmd_type(subsection(d)) :- !.
%
cmd_type(sp(s)) :- !.
cmd_type(p(s)) :- !.
cmd_type(noindent(s)) :- !.
cmd_type(math(s)) :- !. % TODO: Allow $...$, \(...\) notation too?
% cmd_type(displaymath(t)) :- !. (done in a 'env') % TODO: Allow $$...$$, \[...\] notation too?
cmd_type(defmathcmd(s,s)) :- !.
cmd_type(defmathcmd(s,s,s)) :- !.
cmd_type(begin(s)) :- !.
cmd_type(end(s)) :- !.
% TODO: Distinguish between item_env/2 (for descriptions) and item_env/1 (for ordered or unordered lists)
cmd_type(item(d)) :- !.
cmd_type('}') :- !.
cmd_type('{') :- !.
cmd_type('@') :- !.
cmd_type(today(s)) :- !.
cmd_type(hfill(s)) :- !.
cmd_type(iso(s)) :- !.
cmd_type('`'(s)) :- !.
cmd_type(''''(s)) :- !.
cmd_type(^'^'(s)) :- !. % NOTE: Escaped ^ due to fsyntax!
cmd_type('..'(s)) :- !.
cmd_type('"'(s)) :- !.
cmd_type(^'~'(s)) :- !. % NOTE: Escaped ~ due to fsyntax!
cmd_type('='(s)) :- !.
cmd_type(uref(s)) :- !.
cmd_type(uref(d, s)) :- !.
cmd_type(email(d)) :- !.
cmd_type(email(d, d)) :- !.
cmd_type(author(d)) :- !.
cmd_type(image(s)) :- !.
cmd_type(image(s, s, s)) :- !.
%
cmd_type(footnote(d)) :- !.
cmd_type(bf(d)) :- !.
cmd_type(em(d)) :- !.
cmd_type(tt(d)) :- !.
cmd_type(key(d)) :- !.
cmd_type(var(d)) :- !.
%% Refs to nodes / sections / chapter --some chars not allowed.
cmd_type(ref(d)) :- !.
%% Accents, etc.
cmd_type('.'(s)) :- !.
cmd_type('u'(s)) :- !.
cmd_type('v'(s)) :- !.
cmd_type('H'(s)) :- !.
cmd_type('t'(s)) :- !.
cmd_type('c'(s)) :- !.
cmd_type('d'(s)) :- !.
cmd_type('b'(s)) :- !.
cmd_type('oe'(s)) :- !.
cmd_type('OE'(s)) :- !.
cmd_type('ae'(s)) :- !.
cmd_type('AE'(s)) :- !.
cmd_type('aa'(s)) :- !.
cmd_type('AA'(s)) :- !.
cmd_type('o'(s)) :- !.
cmd_type('O'(s)) :- !.
cmd_type('l'(s)) :- !.
cmd_type('L'(s)) :- !.
cmd_type('ss'(s)) :- !.
cmd_type('?'(s)) :- !.
cmd_type('!'(s)) :- !.
cmd_type('i'(s)) :- !.
cmd_type('j'(s)) :- !.
cmd_type(copyright(s)) :- !.
cmd_type(bullet(s)) :- !.
cmd_type(result(s)) :- !.
%
% ** Semi-private commamnds **
%  Those are required just parsing bibrefs resolved with
%  docstring.bst, but should not be written by the user.
cmd_type(bibitem(s,s)) :- !.
cmd_type(newblock(s)) :- !.
%
% ** Extensible commands **
%  Those commands should be removed from here and be provided by
%  extension modules similar to compilation modules
%  (i.e. documentation modules)
cmd_type(html_template(s)) :- !.
cmd_type(distpkg_download(s)) :- !.

% TODO: Refine those commands 
% Internal commands (cannot be parsed from any docstring, may depend
% on the backend)

% ** Some internal commands **
icmd_type(if(t, d)). % Conditional environments
icmd_type(env_(t, d)). % Environments (from parsing)
% ** Some universal raw commands **
icmd_type(nop). % nothing
icmd_type(string_esc(t)). % a normal string (to be escaped)
icmd_type(string_verb(t)). % a verbatim string (to be escaped)
icmd_type(raw(t)). % raw text (no processing at all)
icmd_type(raw_string(t)). % raw string (already escaped)
icmd_type(raw_nl). % a raw new line
icmd_type(raw_fc). % ensure that text is written at the column 0
% a new line that eats the blanks following in the next line
%   e.g. "foo\n  bar" ==> "foo  \nbar"
icmd_type(raw_nleb).
% ** Commands for 'texinfo' **
icmd_type(infocmd(t)). % one-liner info command (no arguments)
icmd_type(infocmd(t,d)). % one-liner info command
icmd_type(infoenv(t,d)). % info enviroment (no arguments)
icmd_type(infoenv(t,d,d)). % info environment
% like infoenv/2, but only shows ups in the tex output of texinfo
% (equivalent to [d] in the info output)
icmd_type(infoenv_onlytex(t,d)).
% ** Commands for maths **
icmd_type(mathenv(t)).
icmd_type(mathenv(t,t)).
icmd_type(defmathcmd_(s,s,s)).
% ** Commands for 'html' **
icmd_type(htmlenv(t,d)). % html environment
icmd_type(htmlenv(t,t,d)). % html environment
icmd_type(htmlenv1(t)). % html environment
icmd_type(htmlenv1(t,t)). % html environment
icmd_type(htmldecl(t)). % html decl
icmd_type(htmlcomment(t)). % html comment
% ** Commands for 'manl' **
icmd_type(man_page(td,t,t,td,td,td)). % html environment
% ** Shared internal commands **
icmd_type(section_env(t,t,td,td)).
icmd_type(section_link(t)). % a separated section 
icmd_type(idx_env(t,t,t,d,d)). % index command
% dependency with a separated section (depending on the backend, it may include
% or not the contents of the separated section)
icmd_type(backend_linkdep(t)).
icmd_type(backend_idx(t,t,t)).
icmd_type(hfill).
icmd_type(linebreak). % break the current line (should it be like "@p @noindent"?)
icmd_type(subtitle_line(d)). % a line formatted with bigger font (not a good idea, use subsections instead?)
icmd_type(twocolumns(d)).
icmd_type(itemize_none(d)).
icmd_type(itemize_plain(d)).
icmd_type(itemize_minus(d)).
icmd_type(itemize_bullet(d)).
icmd_type(description_env(d)).
icmd_type(cartouche(d)).
icmd_type(optional_cartouche(d)).
icmd_type(alert(d)).
icmd_type(idx_anchor(t,t,t,t,t)). % an anchor for entries in the index
icmd_type(end_document).
icmd_type(copyright_page(d)).
icmd_type(title(d)).
icmd_type(subtitle(d)).
icmd_type(authors(d)).
icmd_type(backend_comment(t)).
icmd_type(quotation(d)).
icmd_type(setpagenumber(t)).
icmd_type(printindex(t,t)).
icmd_type(backend_printindex(t)).
icmd_type(printtoc(t)). % print table of contents (or part of it)
icmd_type(printbiblio).
icmd_type(simple_link(t,t,t,s)).
icmd_type(cite_link(t,s)).
icmd_type(ref_link(t,s)).
icmd_type(missing_link(s)).
icmd_type(pred_in_toc(t,t)).
% Write text aligned to the left and aligned to the right, in the same
% line. E.g. 
%  | foo/1          PREDICATE |
icmd_type(left_and_right(d,d)).
icmd_type(defpred(t,t,t,t,d)).
icmd_type(defauthor(t,t,t)).
icmd_type(navigation_env(d,d)).
icmd_type(image_auto(s,t)).
%
% this one is internal -- params cannot still be read
icmd_type(html_template_internal(t,t)) :- !.

% Some icmd that cannot be further reduced by definition
% TODO: is this equal to doctokens?
primitive_icmd(nop).
primitive_icmd(raw(_)).
primitive_icmd(raw_nl).
primitive_icmd(raw_fc).
primitive_icmd(raw_nleb).
primitive_icmd(raw_string(_)).

% ===========================================================================

:- doc(section, "Doctree Definition and Basic Operations").

:- export(doctree/1).
:- regtype doctree/1 # "Intermediate tree representation for documentation".
% TODO: define based on cmd_type and icmd_type; it could be a kind of
%       'enriched' regular type.
doctree(_).

:- export(doctree_is_empty/1).
:- pred doctree_is_empty(+R) : doctree(R) # "Emptiness test".
doctree_is_empty(A) :- var(A), !, fail.
doctree_is_empty(nop).
doctree_is_empty([]).
doctree_is_empty([A|B]) :- doctree_is_empty(A), doctree_is_empty(B).
doctree_is_empty(raw([])).
doctree_is_empty(raw_string([])).
doctree_is_empty(string_esc([])).
doctree_is_empty(string_verb([])).

:- export(is_nonempty_doctree/1).
:- pred is_nonempty_doctree(+R) : doctree(R) # "Not empty test".
is_nonempty_doctree(X) :- \+ doctree_is_empty(X).

:- export(empty_doctree/1).
:- pred empty_doctree(-R) : doctree(R) # "Empty".
empty_doctree([]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Documentation Links").

% TODO: current definition of the doctree regular type is incomplete

:- export(doclink/1).
:- regtype doclink/1 # "A link to a document label".
doclink(link_to(Base, Label)) :- term(Base), doclabel(Label).
doclink(no_link). % link to nowhere

:- export(doclabel/1).
:- regtype doclabel/1 # "An internal label".
% Note: global_label is useful for naming nodes in the 'info' backend
doclabel(no_label). % no label
doclabel(global_label(_)). % unique label for the document Base (mimic content's title)
doclabel(local_label(_)). % local labels to the document Base (mimic content's title)
doclabel(localnum_label(_)). % local labels to the document Base (unique numbering)

:- export(doclink_at/2).
doclink_at(link_to(Curr, _), Curr).

:- export(doclink_is_local/1).
% Links to local sections
doclink_is_local(link_to(_, local_label(_))).

% ---------------------------------------------------------------------------

:- export(doctokens/1).  
:- regtype doctokens/1 # "Primitive doctree subset (ready for output,
not further reducible)".
doctokens(_). % TODO: define

% ---------------------------------------------------------------------------

:- doc(subsection, "Section Properties").

% Properties of section_env:
%   level(L): section at level L
%   with_parent: section label appended with parent base
%   unnumbered: special unnumbered section
%   linktop: just contain navigation references to top
%   first_file_section: first section in the file (may require special translation)
% TODO: finish

:- use_module(library(lists), [select/3]).

:- export(section_prop/2).
section_prop(P, SecProps) :-
	member(P, SecProps), !.

:- export(section_select_prop/3).
section_select_prop(P, SecProps0, SecProps) :-
	select(P, SecProps0, SecProps), !.

% ---------------------------------------------------------------------------

:- doc(subsection, "Saving/Restoring a doctree to Disk").

% TODO: I got a segmentation fault with fastrw!

:- export(doctree_save/2).
:- pred doctree_save(+atm, +doctree).
doctree_save(RFile, R) :-
	push_prolog_flag(write_strings, on),
	try_finally(open(RFile, write, ROS),
	            (writeq(ROS, R), write(ROS, '.')),
		    close(ROS)),
	pop_prolog_flag(write_strings).
% TODO: faster, but breaks at some point
%	try_finally(open(RFile, write, ROS),
%	            fast_write(ROS, R),
%		    close(ROS)).

:- export(doctree_restore/2).
:- pred doctree_restore(+atm, ?doctree).
doctree_restore(RFile, R) :-
	try_finally(open(RFile, read, ROS),
	            read(ROS, R),
		    close(ROS)).
% TODO: faster, but breaks at some point
%	try_finally(open(RFile, read, ROS),
%	            fast_read(ROS, R),
%		    close(ROS)).

:- use_module(library(make(system_extra)), [try_finally/3]).
:- use_module(library(write), [writeq/2,write/2]).
:- use_module(library(read), [read/2]).
%:- use_module(library(fastrw), [fast_write/2, fast_read/2]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Other Operations").

:- export(doctree_simplify/2).
% TODO: when should I call this? early? or lazily? Indeed, I just want 
%   a simple way to work with sequences.
doctree_simplify([X|Xs], Ys) :- doctree_is_empty(X), !,
	doctree_simplify(Xs, Ys).
doctree_simplify([X|Xs], [X|Ys]) :- !,
	doctree_simplify(Xs, Ys).
doctree_simplify(X, X).

% TODO: improve documentation, generalize to replace other environments?
:- export(doctree_putvars/5).
:- pred doctree_putvars(R0, DocSt, PDict, VarDict, R)
	: ( doctree(R0), docstate(DocSt) ) => doctree(R) #
   "Traverse @var{R0} and replace each @tt{var(Name)} doctree item
    with a fresh variable @var{B}. For each replacement, the term
    @tt{B=Var} is introduced in @var{VarDict}, where @var{Var} is the
    associated value to @var{Name} in the dictionary @var{PDict}.".

doctree_putvars(R0, DocSt, PDict, VarDict, R) :-
	catch(doctree_putvars_(R0, DocSt, PDict, VarDict, [], R),
	  E,
	  throw(couldnt_putvars(E, R0, PDict, VarDict))).

doctree_putvars_(A, _DocSt, _PDict, Vs, Vs, A) :- ( var(A) ; primitive_icmd(A) ), !.
doctree_putvars_(A, DocSt, PDict, Vs, Vs0, B) :- ( A = [] ; A = [_|_] ), !,
	doctree_putvars_list(A, DocSt, PDict, Vs, Vs0, B).
doctree_putvars_(var([string_esc(VarName)]), _DocSt, PDict, Vs, Vs0, B) :- !,
	atom_codes(VarNameA, VarName),
	( member(VarNameA=Var, PDict) -> true
	; throw(var_not_found(VarNameA, PDict))
	),
	% B is left uninstantiated
	Vs = [B=Var|Vs0].
doctree_putvars_(Command, DocSt, PDict, Vs, Vs0, NewCommand) :-
	functor(Command, Cmd, A),
	functor(BT, Cmd, A),
	( cmd_type(BT) -> true
	; icmd_type(BT) -> true
	; throw(wrong_input(doctree_putvars_(Command)))
	),
	Command =.. [_|Xs],
	BT =.. [_|Ts],
	doctree_putvars_args(Ts, Xs, DocSt, PDict, Vs, Vs0, Ys),
	NewCommand =.. [Cmd|Ys].

doctree_putvars_args([], [], _DocSt, _PDict, Vs, Vs, []).
doctree_putvars_args([T|Ts], [X|Xs], DocSt, PDict, Vs, Vs0, [Y|Ys]) :-
	( T = d -> doctree_putvars_(X, DocSt, PDict, Vs, Vs1, Y)
	; Y = X, Vs = Vs1
	),
	doctree_putvars_args(Ts, Xs, DocSt, PDict, Vs1, Vs0, Ys).

doctree_putvars_list([], _DocSt, _PDict, Vs, Vs, []).
doctree_putvars_list([X|Xs], DocSt, PDict, Vs, Vs0, [Y|Ys]) :-
	doctree_putvars_(X, DocSt, PDict, Vs, Vs1, Y),
	doctree_putvars_list(Xs, DocSt, PDict, Vs1, Vs0, Ys).

% ===========================================================================

:- doc(section, "Treatment and Translation of doctree").

% ---------------------------------------------------------------------------

:- doc(subsection, "Scanning (1st pass)").

:- export(doctree_scan_and_save_refs/2).
:- pred doctree_scan_and_save_refs(R, DocSt) : doctree * docstate
   # "Scan and save the references of the doctree".
doctree_scan_and_save_refs(R, DocSt) :-
	labgen_init(DocSt),
	doctree_scan_refs(R, DocSt),
	labgen_clean(DocSt),
	save_refs(DocSt),
	refs_clean(DocSt).

:- pred doctree_scan_refs(R, DocSt) : ( doctree(R), docstate(DocSt) ).
doctree_scan_refs(A, _DocSt) :-
	( var(A) ; primitive_icmd(A) ), !.
doctree_scan_refs(A, DocSt) :- ( A = [] ; A = [_|_] ), !,
	doctree_scan_refs_list(A, DocSt).
doctree_scan_refs(L0, DocSt) :-
	% TODO: duplicated in rewrite_cmd
	L0 = section_env(SecProps, SectLabel, TitleR, Body),
	section_select_prop(subfile(SubName), SecProps, PrevSecProps),
	!,
	DocR = section_env([first_file_section|PrevSecProps], SectLabel, TitleR, Body),
	docstate_currmod(DocSt, OutName),
	scan_subfile(OutName, SubName, DocR, DocSt),
	doctree_scan_refs(section_link(external_subfile(OutName, SubName)), DocSt).
doctree_scan_refs(A, DocSt) :-
	A = section_env(SecProps, SectLabel, Title, Body),
	!,
	ensure_fill_label(Title, DocSt, SectLabel),
	section_prop(level(L), SecProps),
	add_refs_entry(sect(L, SectLabel, Title), DocSt),
	doctree_scan_refs(Body, DocSt).
doctree_scan_refs(section_link(E), DocSt) :-
	!,
	resolve_external(E, DocSt, Base),
	add_refs_entry(sectlink(Base), DocSt).
doctree_scan_refs(idx_env(Mode, Type, IdxLabel, Ref, _Body), DocSt) :-
	!,
	add_idx_entry(Mode, Type, IdxLabel, Ref, DocSt).
doctree_scan_refs(defpred(IdxLabel, Type, _, PN, Body), DocSt) :-
	!,
	% TODO: This logic is repeated in each backend
	PN = F/A, format_to_string("~w/~w", [F, A], S0),
	S = string_esc(S0),
	add_idx_entry(def, Type, IdxLabel, S, DocSt),
	%
	doctree_scan_refs(Body, DocSt).
doctree_scan_refs(defauthor(IdxLabel, Name, _Text), DocSt) :-
	!,
	add_idx_entry(def, author, IdxLabel, Name, DocSt).
doctree_scan_refs(cite(Ref), DocSt) :- !,
	%% bibtex generates an error like this
	%% The top-level auxiliary file: ciaorefs.aux
	%% White space in argument---line 33 of file bla bla bla
	%% : \citation{att-var-iclp,ciao-manual-tr,
	remove_spaces(Ref, RefClean),
	add_refs_entry(citation(RefClean), DocSt).
doctree_scan_refs(Command, DocSt) :-
	functor(Command, Cmd, A),
	functor(BT, Cmd, A),
	( cmd_type(BT) -> true
	; icmd_type(BT) -> true
	; throw(wrong_input(doctree_scan_refs_(Command)))
	),
	Command =.. [_|Xs],
	BT =.. [_|Ts],
	doctree_scan_refs_args(Ts, Xs, DocSt).

doctree_scan_refs_args([], [], _DocSt).
doctree_scan_refs_args([T|Ts], [X|Xs], DocSt) :-
	( T = d -> doctree_scan_refs(X, DocSt)
	; true
	),
	doctree_scan_refs_args(Ts, Xs, DocSt).

doctree_scan_refs_list([], _DocSt).
doctree_scan_refs_list([X|Xs], DocSt) :-
	doctree_scan_refs(X, DocSt),
	doctree_scan_refs_list(Xs, DocSt).

% Ensure that a label is filled
ensure_fill_label(Title, DocSt, SectLabel) :-
	( SectLabel = global_label(Label0),
	  var(Label0) ->
	    doctree_to_rawtext(Title, DocSt, Label0)
	; SectLabel = local_label(Label0),
	  var(Label0) ->
	    % TODO: useful to obtain a permalink? make it dependant to the backend?
	    doctree_to_rawtext(Title, DocSt, Label0)
	; SectLabel = localnum_label(Label0),
	  var(Label0) ->
	    labgen_get(DocSt, Label0)
	; true
	).

add_idx_entry(Mode, Type, IdxLabel, Key, DocSt) :-
	ensure_fill_label(Key, DocSt, IdxLabel),
	% TODO: Accents are lost here; use something richer than 'plaintext'? (e.g. unicode)
	doctree_to_rawtext(Key, DocSt, Key2),
	add_refs_entry(idx(Mode, Type, IdxLabel, Key2), DocSt).

remove_spaces([],     []).
remove_spaces([32|R], RC) :- !,
	remove_spaces(R, RC).
remove_spaces([A|R], [A|RC]) :-
	remove_spaces(R, RC).

% (first pass)
% TODO: See write_as_subfile
% Scan a part that will be rendered as a subfile
scan_subfile(OutName, SubSuffix, X, DocSt) :-
	get_subbase(OutName, SubSuffix, SubName),
	get_sub_docstate(DocSt, SubName, DocSt1),	
	doctree_scan_and_save_refs(X, DocSt1).

get_sub_docstate(DocSt0, SubName, DocSt) :-
	docstate_set_currmod(DocSt0, SubName, DocSt).

% ---------------------------------------------------------------------------

:- doc(subsection, "Translation of doctree to Text (2nd pass)").
% (using each backend)
% (requires doctree whose references has been scaned and saved)

% ---------------------------------------------------------------------------

:- export(doctree_prepare_refs_translate_and_write/3).
doctree_prepare_refs_translate_and_write(ModR, DocSt, OS) :-
	prepare_current_refs(DocSt),
	doctree_translate_and_write(ModR, DocSt, OS),
	clean_current_refs(DocSt),
	!.
doctree_prepare_refs_translate_and_write(_ModR, _DocSt, _OS) :-
	throw(failed_doctree_prepare_refs_translate_and_write).

% ---------------------------------------------------------------------------

:- export(doctree_to_rawtext/3).
:- pred doctree_to_rawtext(X, DocSt, Y) :: doctree * docstate * string
   # "@var{Y} is a simplified raw text representation of the @var{X}".

%   # "A reduced version of @pred{doctree_to_string_slow/3}".
% TODO: It can be implemented more easily with string output of streams.
% TODO: This is required for @section{}, unless the command argument
%       is limited to something simpler than a doc_string
% TODO: I would like to get rid of this and rely only on fmt_out
doctree_to_rawtext(X, DocSt, Ys) :-
	catch(doctree_to_rawtext_(X, DocSt, Ys, []),
	      E,
	      throw(bug_in_doctree_to_rawtext(E, X))).

doctree_to_rawtext_(X, _DocSt, "<var>"||Ys, Ys) :- var(X), !.
doctree_to_rawtext_([], _DocSt, Xs, Xs) :- !.
doctree_to_rawtext_([X|Xs], DocSt, Ys, Zs) :- !,
	doctree_to_rawtext_(X, DocSt, Ys, Ys1),
	doctree_to_rawtext_(Xs, DocSt, Ys1, Zs).
doctree_to_rawtext_(raw(Xs), _DocSt, Ys, Zs) :- !,
	append(Xs, Zs, Ys).
doctree_to_rawtext_(raw_string(Xs), _DocSt, Ys, Zs) :- !,
	append(Xs, Zs, Ys).
doctree_to_rawtext_(X, _DocSt, Ys, Zs) :- plaintext_ignore(X), !,
	arg(1, X, Arg),
	doctree_to_rawtext_(Arg, _DocSt, Ys, Zs).
% TODO: incomplete
doctree_to_rawtext_(X, DocSt, Ys, Zs) :- simple_escaped_command(X), !,
	atom_codes(X, C),
	doctree_to_rawtext_(string_esc(C), DocSt, Ys, Zs).
doctree_to_rawtext_(uref(Xs), DocSt, Ys, Zs) :- !,
	doctree_to_rawtext_(raw(Xs), DocSt, Ys, Zs).
doctree_to_rawtext_(string_esc(Xs), DocSt, Ys, Zs) :- !,
	% TODO: Do not escape here
	escape_string(normal, Xs, DocSt, Xs2),
	append(Xs2, Zs, Ys).
doctree_to_rawtext_(string_verb(Xs), DocSt, Ys, Zs) :- !,
	% TODO: Do not escape here
	escape_string(verb, Xs, DocSt, Xs2),
	append(Xs2, Zs, Ys).
doctree_to_rawtext_(raw_nl, _DocSt, Ys, Zs) :- !,
	Ys = "\n"||Zs.
doctree_to_rawtext_(raw_fc, _DocSt, Ys, Zs) :- !,
	Ys = "\n"||Zs.
doctree_to_rawtext_(raw_nleb, _DocSt, Ys, Zs) :- !,
	Ys = "\n"||Zs.
doctree_to_rawtext_(X, _DocSt, Ys, Zs) :- accent_cmd(X), !,
	% Accented characters are ignored in 'rawtext'
	% TODO: This should be optional...
	arg(1, X, Y),
	append(Y, Zs, Ys).
doctree_to_rawtext_(X, _, _, _) :-
	throw(unknown_in_doctree_to_rawtext(X)).

accent_cmd('`'(_)).
accent_cmd(''''(_)).
accent_cmd(^'^'(_)). % NOTE: Escaped ^ due to fsyntax!
accent_cmd('..'(_)).
accent_cmd('"'(_)).
accent_cmd(^'~'(_)). % NOTE: Escaped ~ due to fsyntax!
accent_cmd('='(_)).

% TODO: generalize for all backends?
simple_escaped_command('@').
simple_escaped_command('{').
simple_escaped_command('}').

plaintext_ignore(var(_)).
plaintext_ignore(bf(_)).
plaintext_ignore(tt(_)).
plaintext_ignore(em(_)).
plaintext_ignore(email(_)).

:- use_module(library(lists), [list_concat/2, append/3]).

% TODO: in 'doctree_to_rawtext' we omit some characters! (it is mandatory,
% e.g., to obtain names for indices, etc.).
%
%% % (complete but slower and badly implemented version)
%% :- export(doctree_to_string_slow/3).
%% doctree_to_string_slow(X, DocSt, String) :-
%% 	telling(Old),
%% 	mktemp(autodocXXXXXX, Tmp),
%% 	tell(Tmp),
%% 	( doctree_translate_and_write(X, DocSt, user_output) ->
%% 	    Ok = yes
%% 	; Ok = no
%% 	),
%% 	told,
%% 	tell(Old),
%% 	read_file(Tmp, String),
%% 	delete_file(Tmp),
%% 	Ok = yes.
%% 
%% :- use_module(library(system), [delete_file/1, mktemp/2]).
%% :- use_module(lpdocsrc(src(autodoc_aux)), [read_file/2]).
%% :- use_module(library(dec10_io)).

% ---------------------------------------------------------------------------

:- use_module(library(terms), [atom_concat/2]).

% TODO: @pred{doctree_translate_and_write/3} is exported for trivial
%       infoindex generation

:- export(doctree_translate_and_write/3).
doctree_translate_and_write(X0, DocSt, OS) :-
	% rewrite all commands until we get a tree of tokens
	% output the tree of tokens
	rewrite_command(X0, DocSt, X2),
	doctokens_write(X2, DocSt, OS),
	!.
doctree_translate_and_write(X0, _DocSt, _OS) :-
	throw(bug_in_doctree_translate_and_write(X0)).

% ---------------------------------------------------------------------------

rewrite_command(L, _DocSt, L) :- ( var(L) ; primitive_icmd(L) ), !.
rewrite_command(A, DocSt, B) :- ( A = [] ; A = [_|_] ), !,
	rewrite_commands(A, DocSt, B).
% TODO: Hack to fix unwanted blanks after "@begin{verbatim}", solve it in a better way
rewrite_command(env_('verbatim', Body0), DocSt, R) :-
	remove_first_nl(Body0, Body),
	!,
	rewrite_command(env_('verbatim', Body), DocSt, R).
rewrite_command(Command, DocSt, NewCommand2) :-
	functor(Command, Cmd, A),
	functor(BT, Cmd, A),
	Command =.. [_|Xs],
	( cmd_type(BT) -> Kind = cmd
	; icmd_type(BT) -> Kind = icmd
	; throw(cannot_rewrite(rewrite_command(Command)))
	),
	BT =.. [_|Ts],
	rewrite_cmd_args(Ts, Xs, DocSt, Ys),
	B1 =.. [Cmd|Ys],
	rewrite_cmd(B1, DocSt, NewCommand),
	rewrite_command(NewCommand, DocSt, NewCommand2).

rewrite_cmd(L0, DocSt, R) :-
	L0 = section_env(SecProps, SectLabel, TitleR, Body),
	section_select_prop(subfile(SubName), SecProps, PrevSecProps),
	!,
	DocR = section_env([first_file_section|PrevSecProps], SectLabel, TitleR, Body),
	docstate_currmod(DocSt, OutName),
	write_as_subfile(OutName, SubName, DocR, DocSt),
	R = section_link(external_subfile(OutName, SubName)).
rewrite_cmd(section_link(External), DocSt, L) :- !,
	% Note: this translation is just for the texinfo backend
	% (for the other backends, doctree_scan_refs has already made its work)
	resolve_external(External, DocSt, Base),
	docstate_backend(DocSt, Backend),
	absfile_for_subtarget(Base, Backend, cr, F),
	% TODO: hack, remove the path since we live in the same directory
	get_name(F, FName),
	L = backend_linkdep(FName).
% ... (shared commands) ...
rewrite_cmd(math(X), _DocSt,  R) :- !,
	fmt_to_latex(X, X1),
	R = mathenv(X1).
rewrite_cmd(env_('displaymath', X), DocSt,  R) :- !,
	doctree_to_rawtext(X, DocSt, X1),
	fmt_to_latex(X1, X2),
	R = mathenv(display, X2).
rewrite_cmd(defmathcmd(Cmd,N,Def), _DocSt,  R) :- !,
	fmt_to_latex(Cmd, Cmd1),
	fmt_to_latex(N, N1),
	fmt_to_latex(Def, Def1),
	R = defmathcmd_(Cmd1, N1, Def1).
rewrite_cmd(defmathcmd(Cmd,Def), _DocSt,  R) :- !,
	R = defmathcmd(Cmd, "0", Def).
rewrite_cmd(image(F), _DocSt, R) :- !, R = image_auto(F, []).
rewrite_cmd(image(F,X,Y), _DocSt, R) :- !, R = image_auto(F, [X,Y]).
rewrite_cmd(idx_env(Mode, Type, IdxLabel, Ref, Body), DocSt, R) :-
	!,
	fmt_idx_env(Mode, Type, IdxLabel, Ref, Body, DocSt, R).
rewrite_cmd(defauthor(IdxLabel, Name, Text), DocSt, R) :- !,
	% TODO: merge with idx_env or defpred?
	Mode = def, Type = author, Ref = Name, Body = Text,
	fmt_idx_env(Mode, Type, IdxLabel, Ref, Body, DocSt, R).
rewrite_cmd(printtoc(TOCKind), DocSt, R) :- !,
	fmt_toc(TOCKind, DocSt, R).
rewrite_cmd(printbiblio, DocSt, R) :- !,
	( doc_customdic_get(DocSt, biblio_doctree, RefsR0) ->
	    ( RefsR0 = [] ->
	        % TODO: Generalize for other sections?
	        R = string_esc("(this section is empty)")
	    ; R = env_('description', RefsR0)
	    )
	; % No bibliography yet. This happens when a doctree is
	  % written before the bibliography is resolved.
	  R = string_esc("[ERROR - NO BIBLIOGRAPHY]")
	).
rewrite_cmd(printindex(Name,IndexId), DocSt, R) :- !,
	docstate_backend(DocSt, Backend),
	( Backend = html ->
	    % format the index ourselves
	    fmt_index(Name, IndexId, DocSt, R)
	; % let the backend print the index
	  R = backend_printindex(IndexId)
	).
rewrite_cmd(cite(Ref), DocSt, R) :- !,
	fmt_cite(Ref, DocSt, R).
rewrite_cmd(ref(Ref), DocSt, R) :- !,
	fmt_ref(Ref, DocSt, R).
% TODO: include here general shared internal commands
rewrite_cmd(string_esc(X), DocSt, R) :- !,
	R = raw_string(X2),
	escape_string(normal, X, DocSt, X2).
rewrite_cmd(string_verb(X), DocSt, R) :- !,
	R = raw_string(X2),
	escape_string(verb, X, DocSt, X2).
rewrite_cmd(X, DocSt, L) :-
	docstate_backend(DocSt, Backend),
	autodoc_rw_command_hook(Backend, DocSt, X, L).

rewrite_cmd_args([], [], _, []).
rewrite_cmd_args([T|Ts], [X|Xs], DocSt, [Y|Ys]) :-
	( T = d -> rewrite_command(X, DocSt, Y)
	; Y = X
	),
	rewrite_cmd_args(Ts, Xs, DocSt, Ys).

rewrite_commands([], _DocSt, []).
rewrite_commands([X|Xs], DocSt, [Y|Ys]) :-
	rewrite_command(X, DocSt, Y),
	rewrite_commands(Xs, DocSt, Ys).

:- multifile autodoc_rw_command_hook/4. 

write_as_subfile(OutName, SubSuffix, DocR0, DocSt) :-
	% Include X as a subfile
	get_subbase(OutName, SubSuffix, SubName),
	get_sub_docstate(DocSt, SubName, DocSt1),
	insert_printtoc(DocR0, DocSt1, DocR),
	% clean some customdic variables
	doc_customdic_replace(DocSt1, fulltree, _, DocSt2),
	doc_customdic_replace(DocSt2, currtree, _, DocSt3),
	doc_customdic_replace(DocSt3, nav, _, DocSt4),
	%
	docstate_backend(DocSt, Backend),
	%
	absfile_for_subtarget(SubName, Backend, cr, SubFile),
	open(SubFile, write, SubOS),
	doctree_prepare_refs_translate_and_write(DocR, DocSt4, SubOS),
	close(SubOS).

:- use_module(library(make(make_rt)), [get_name/2]).

% ---------------------------------------------------------------------------

:- export(escape_string/4).
:- pred escape_string/4 => atom * string * docstate * string

   # "Escapes needed characters in input string as needed for the
      target format.".

% Modes for escaping: 
%   normal: a normal string
%   verb: a verbatim string 

escape_string(InputType, NS, DocSt, VS) :-
	docstate_backend(DocSt, Backend),
	( autodoc_escape_string_hook(Backend, InputType, NS, DocSt, VS) ->
	    true
	; VS = NS % TODO: wrong! define escape_string for all backends
	).

:- multifile autodoc_escape_string_hook/5.

% ---------------------------------------------------------------------------

:- doc(subsection, "Writting doctokens").

:- pred doctokens_write(X, DocSt, OS) : doctokens * docstate * stream
        # "Write doctokens to the @var{OS} stream".

% Internal state of the algorithm:
%
%   NL=nl:   The previous character was a new line
%   NL=nonl: The previous character was not a new line
%   NL=nl_eat_blank: 
%          The previous character was a new line but was not
%          written. Space after it are moved before the new line.
%          (useful for writting 'info' files)

% TODO: Refine usage of special raw_nleb, raw_fc commands
%       (e.g. not necessary for all backend)

doctokens_write(X2, DocSt, OS) :-
	doctokens_write_(X2, nonl, NL, DocSt, OS),
	( NL = nl_eat_blank ->
	    format(OS, "\n", [])
	; true
	).

doctokens_write_(A, NL, NL, _DocSt, _OS) :- var(A), !.
doctokens_write_(A, NL, NNL, DocSt, OS) :- ( A = [] ; A = [_|_] ), !,
	doctokens_write_list(A, NL, NNL, DocSt, OS). 
doctokens_write_(nop, NL, NL, _DocSt, _OS) :- !.
% (In nl_eat_blank state)
doctokens_write_(raw_string(" "), nl_eat_blank, nl_eat_blank, _DocSt, OS) :- !,
	format(OS, " ", []).
% (eating more than one blank was worse than expected --JF)
%doctokens_write_(raw_string(""), NL, NL, _DocSt, _OS) :- !.
%doctokens_write_(raw_string(" "||Rest), nl_eat_blank, NNL, DocSt, OS) :- !,
%	format(OS, " ", []),
%	doctokens_write_(raw_string(Rest), nonl, NNL, DocSt, OS).
doctokens_write_(A, nl_eat_blank, NL, DocSt, OS) :- !,
	format(OS, "~n", []),
	doctokens_write_(A, nl, NL, DocSt, OS).
% (Not in nl_eat_blank state)
%doctokens_write_(raw(String), _NL, nonl, _DocSt, _OS) :- var(String), !,
%	% TODO: Why? only in XML backend
%	true.
doctokens_write_(raw(String), _NL, nonl, _DocSt, OS) :- nonvar(String), !,
	format(OS, "~s", [String]).
doctokens_write_(raw_fc, nl, nl, _DocSt, _OS) :- !.
doctokens_write_(raw_fc, nonl, nl, _DocSt, OS) :- !,
	format(OS, "~n", []).
doctokens_write_(raw_nl, _, nl, _DocSt, OS) :- !,
	format(OS, "~n", []).
doctokens_write_(raw_nleb, _, nl_eat_blank, _DocSt, _OS) :- !.
doctokens_write_(raw_string(String), _NL, NL, _DocSt, OS) :- !,
	% detect if there was a newline
	( append(_, "\n", String) ->
	    NL = nl
	; NL = nonl
	),
	format(OS, "~s", [String]).
doctokens_write_(A, NL, _, _, _) :-
	throw(cannot_rewrite(doctokens_write_(A, NL))).

doctokens_write_list([], NL, NL, _DocSt, _OS) :- !.
doctokens_write_list([R|Rs], NL0, NL, DocSt, OS) :-
	doctokens_write_(R, NL0, NL1, DocSt, OS),
	doctokens_write_list(Rs, NL1, NL, DocSt, OS).

%% ---------------------------------------------------------------------------

resolve_external(external_subfile(OutName, SubName), _DocSt, Base) :- !,
	get_subbase(OutName, SubName, Base).
resolve_external(external(Base0), _DocSt, Base) :- !, Base = Base0.

%% ---------------------------------------------------------------------------

% TODO: this contains some hardwired cases, can it be improved? --JF
remove_first_nl([string_esc(" ")|Xs0], Xs) :- !,
	remove_first_nl(Xs0, Xs).
remove_first_nl([string_verb(S0)|Xs], [string_verb(S)|Xs]) :- !,
	remove_first_nl_str(S0, S).

remove_first_nl_str(" "||S0, S) :- !, remove_first_nl_str(S0, S).
remove_first_nl_str("\n"||S0, S) :- !, S = S0.

% ===========================================================================

:- doc(section, "Common Formatting Operations").

% ---------------------------------------------------------------------------

:- doc(subsection, "Formatting Cites and References").

:- pred fmt_cite(Ref, DocSt, R) # "Process (resolve) a bibliographical
   citation".
fmt_cite(Ref0, DocSt, R) :-
	( doc_customdic_get(DocSt, biblio_pairs, RefPairs) ->
	    remove_spaces(Ref0, Ref),
	    comma_split(Ref, Refs),
	    resolve_cites(Refs, RefPairs, DocSt, R1),
	    R = [string_esc("["), R1, string_esc("]")]
	; throw(bug_no_biblio_pairs)
	).

% Split comma-separated string list of strings
comma_split([], []) :- !.
comma_split(S0, [R|Rs]) :-
	comma_split_(S0, S1, R),
	comma_split(S1, Rs).

comma_split_([], [], []).
comma_split_([X|Xs], Zs, R) :-
	( X = 0', -> Zs = Xs, R = []
	; R = [X|R0], comma_split_(Xs, Zs, R0)
	).

resolve_cites([], _, _, []).
resolve_cites([C|Cs], RefPairs, DocSt, Rs) :-
	resolve_cite(C, RefPairs, DocSt, R),
	Rs = [R|Rs0],
	( Cs = [] ->
	    Rs0 = []
	; Rs0 = [string_esc(",")|Rs1],
	  resolve_cites(Cs, RefPairs, DocSt, Rs1)
	).

% TODO: Move part of this predicate to autodoc_refsdb
resolve_cite(C, RefPairs, _DocSt, R) :-
	% TODO: make RefPairs a dictionary so that this can be faster
	% note: Label is the textual representation for the cite (e.g. [JS99])
	( member((Label0,Ref), RefPairs),
	  Ref = C ->
	    Text = Label0,
	    get_mainmod(MainBase),
	    get_subbase(MainBase, 'refs', RefsBase),
	    CiteLink = link_to(RefsBase, local_label(Ref)),
	    R = cite_link(CiteLink, Text)
	; R = string_esc("{UNKNOWNCITE?}"),
	  % TODO: Improve warning message
	  message(error, ['Unresolved bibliographical reference `', Ref, '\'.'])
	).

% ---------------------------------------------------------------------------

:- pred fmt_ref(Ref, DocSt, R) # "Process a section reference".
% (note: currently sections can only be referenced by its name)
fmt_ref(Ref0, DocSt, R) :-
	( doc_customdic_get(DocSt, fulltree, FullTree) ->
	    doctree_to_rawtext(Ref0, DocSt, Ref1),
	    resolve_ref(Ref1, FullTree, R)
	; throw(bug_no_fulltree)
	).

resolve_ref(Ref, FullTree, R) :-
	% TODO: make RefPairs a dictionary so that this can be faster
	( secttree_resolve(Ref, FullTree, Link) ->
	    R = ref_link(Link, Ref)
	; % TODO: Emit warning here?
	  R = missing_link(Ref),
	  format(user_error, "WARNING: Could not resolve @ref{~s}~n", [Ref])
	).

% ---------------------------------------------------------------------------

:- doc(subsection, "Formatting the Table of Contents").
% (with references to sections/subsections)
% 
% Note: the full or partial TOC may be wanted

fmt_toc(_, DocSt, R) :-
	% TODO: generalize?
	docstate_backend(DocSt, Backend),
	\+ Backend = html, % ignore if not HTML
	!,
	R = [].
fmt_toc(all, DocSt, R) :- !,
	fmt_toc(local, DocSt, R1),
	fmt_toc(global, DocSt, R2),
	doctree_simplify([R1, R2], R).
fmt_toc(TOCKind, DocSt, R) :- TOCKind = vertical_menu, !,
	( doc_customdic_lookup(DocSt, fulltree, Tree0) ->
	    true
	; throw(menu_not_computed)
	),
	% Remove the root node
	Tree0 = [node(_,_,Tree)],
	%
	docstate_currmod(DocSt, Name),
	fmt_vmenu(Tree, Name, 0, R0),
	( \+ doctree_is_empty(R0) ->
	    R = [itemize_bullet(R0)]
	; R = []
	).
fmt_toc(TOCKind, DocSt, R) :-
	( doc_customdic_lookup(DocSt, currtree, Tree) ->
	    true
	; throw(menu_not_computed)
	),
	fmt_toc_(Tree, TOCKind, R0),
	( \+ doctree_is_empty(R0) ->
	    toc_title(TOCKind, Title),
	    R = [subtitle_line(Title), itemize_bullet(R0)]
	; R = []
	).

% ---------------------------------------------------------------------------

% Format the node tree as a table of contents
fmt_toc_([], _, []).
fmt_toc_([node(Link,T,Subs)|Ss], TOCKind, Rs) :-
	( toc_include_link(TOCKind, Link) ->
	    Rs = [R|Rs0],
	    fmt_toc_(Subs, TOCKind, SubRs),
	    fmt_toc_link(default, Link, T, SubRs, R)
	; Rs = Rs0
        ),
	% Continue with the rest of nodes
	fmt_toc_(Ss, TOCKind, Rs0).

% ---------------------------------------------------------------------------

% Format the node tree as a vertical menu
fmt_vmenu([], _, _Depth, []).
fmt_vmenu([node(Link,T,Subs)|Ss], Name, Depth, Rs) :-
	( \+ doclink_is_local(Link) ->
	    Rs = [R|Rs0],
	    Depth1 is Depth + 1,
	    fmt_vmenu(Subs, Name, Depth1, SubRs),
	    ( Link = link_to(Name, _) -> Style = 'selmenu'
	    ; Link = no_link -> Style = 'phonymenu'
	    ; Style = 'unselmenu'
	    ),
	    ( Depth = 0 -> T2 = bf(T) ; T = T2 ), % use bold for first level
	    fmt_toc_link(Style, Link, T2, SubRs, R)
	; Rs = Rs0
        ),
	% Continue with the rest of nodes
	fmt_vmenu(Ss, Name, Depth, Rs0).

% ---------------------------------------------------------------------------

fmt_toc_link(Style, Link, Title, SubRs, R) :- !,
	R = [item(""), simple_link(Style, no_label, Link, Title)|R0],
	( SubRs = [] ->
	    R0 = []
	; R0 = [itemize_bullet(SubRs)]
	).

% toc_title(full, string_esc("Table of Contents")).
toc_title(local, string_esc("Table of Contents")).
toc_title(global, string_esc("Parts")).

% toc_include_link(full, _).
toc_include_link(local, Link) :- doclink_is_local(Link). 
toc_include_link(global, Link) :- \+ doclink_is_local(Link). 

% ---------------------------------------------------------------------------

:- doc(subsection, "Auxiliary for Table of Contents").

:- export(insert_printtoc/3).
:- pred insert_printtoc(R0, DocSt, R) # "Insert the command to print
   the table of contents in a given doctree. The right place may be
   different depending on the chosen backend.".
insert_printtoc(R0, DocSt, R) :-
	( ( docstate_backend(DocSt, Backend), Backend = texinfo
	  ; doc_doing_mainmod(DocSt),
	    \+ setting_value(html_layout, 'website_layout')
	  ) ->
	    % Only for mainmod and for texinfo; html components puts the menu elsewhere
	    % TODO: @bug{menutexi} Not yet working, still needs external '.el'
	    insert_printtoc_(R0, R)
	; R = R0
	).

% Insert 'printtoc' in the right place (just before first section)
insert_printtoc_(R0, R) :-
	R0 = section_env(PrevSecProps, SectLabel, TitleR, Body0),
	insert_printtoc__(Body0, no, Body),
	R = section_env(PrevSecProps, SectLabel, TitleR, Body).

% (@var(More) indicates if we should insert @tt{printtoc} or fail in
% case that the current list ends without a section command)
insert_printtoc__([], no, [printtoc(all)]) :- !.
insert_printtoc__([A|As], _, [B|As]) :- insert_printtoc__(A, yes, B), !.
insert_printtoc__([A|As], _, [printtoc(all),A|As]) :- is_section_cmd(A), !.
insert_printtoc__([A|As], More, [A|Bs]) :- insert_printtoc__(As, More, Bs).

is_section_cmd(section_env(_,_,_,_)).

% ---------------------------------------------------------------------------

:- doc(subsection, "Auxiliary for Mathematical Notation").
% TODO: Include in the parser?

%:- export(fmt_to_latex/2).
% Using '\' in lpdoc may be tedious when appearing in strings ("...").
% We allow '@' as control character for formulas. This predicate translates '@'
% to '\' (except '@@' and '\@').
fmt_to_latex([], []).
fmt_to_latex("\\@"||Xs, "\\@"||Ys) :- !, % avoid translation of @
	fmt_to_latex(Xs, Ys).
fmt_to_latex("@@"||Xs, "@"||Ys) :- !, % avoid translation of second @
	fmt_to_latex(Xs, Ys).
fmt_to_latex("@"||Xs, "\\"||Ys) :- !, % translate @ to \
	fmt_to_latex(Xs, Ys).
fmt_to_latex([X|Xs], [X|Ys]) :- !,
	fmt_to_latex(Xs, Ys).

% ===========================================================================

% TODO: This code is not complete (it has problems with atoms, does not
%       write lists correctly, etc.)

% :- pred sp_write/2 : stream * term 
% 
%    # "Same as @pred{write/2}, but puts space around operators and
%       between functor arguments. This makes them easier to read in
%       documents.".
% 
% sp_write(OS,T) :-
%         nonvar(T),
%         functor(T, F, A),
%         sp_write_(OS,A, F, T), !.
% sp_write(OS,T) :- write(OS,T).
% 
% sp_write_(OS,1, F, T) :-
%         current_postfixop(F, P, _), !,
%         arg(1, T, A),
%         write_term(OS, A, [priority(P), numbervars(true)]),
%         put_code(OS,0' ),
%         display(OS,F).
% sp_write_(OS,1, F, T) :-
%         current_prefixop(F, _, P), !,
%         display(OS,F),
%         put_code(OS,0' ),
%         arg(1, T, A),
%         write_term(OS, A, [priority(P), numbervars(true)]).
% sp_write_(OS,2, F, T) :-
%         current_infixop(F, P, _, Q), !,
%         arg(1, T, A),
%         write_term(OS, A, [priority(P), numbervars(true)]),
%         put_code(OS,0' ),
%         display(OS,F),
%         put_code(OS,0' ),
%         arg(2, T, B),
%         write_term(OS, B, [priority(Q), numbervars(true)]).
% sp_write_(OS,A, F, T) :-
%         display(OS, F),
%         put_code(OS,0'(),
%         sp_write_args(1,A,OS,T),
%         put_code(OS,0')).
% 
% sp_write_args(A,A,OS,T) :- !,
%         arg(A,T,Ta),
%         write(OS,Ta).
% sp_write_args(E,A,OS,T) :-
%         arg(E,T,Te),
%         write(OS,Te),
%         display(OS,', '),
%         E1 is E+1,
%         sp_write_args(E1,A,OS,T).

% ===========================================================================

:- doc(bug, "@tt{biblio_pairs} should be a dictionary, not a plain list").
