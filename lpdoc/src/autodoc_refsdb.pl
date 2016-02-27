:- module(autodoc_refsdb, [], [dcg, assertions, regtypes]). 

:- doc(title,"Database of Documentation References").
:- doc(author,"Jose F. Morales").

:- doc(module, "This module stores and manages all the
documentation references (indices, sections, bibliography, etc.)").

:- use_module(lpdocsrc(src(autodoc))).
:- use_module(lpdocsrc(src(autodoc_doctree))).
:- use_module(lpdocsrc(src(autodoc_structure))).
:- use_module(lpdocsrc(src(autodoc_filesystem))).

% ======================================================================

:- doc(section, "A generator of labels (for anonymous reference labels)").

:- data labcounter/2.

:- export(labgen_init/1).
labgen_init(DocSt) :-
	docstate_currmod(DocSt, Name),
	assertz_fact(labcounter(0, Name)).

:- export(labgen_clean/1).
labgen_clean(DocSt) :-
	docstate_currmod(DocSt, Name),
	retractall_fact(labcounter(_, Name)).

:- export(labgen_get/2).
labgen_get(DocSt, Label) :-
	docstate_currmod(DocSt, Name),
	retract_fact(labcounter(N, Name)),
	number_codes(N, Label),
	N1 is N + 1,
	assertz_fact(labcounter(N1, Name)).

% ======================================================================

:- doc(section, "Simple references (single file)").

% TODO: Merge with refstream stuff

%:- export(refs_entry/2).
:- data refs_entry/2.

:- export(refs_clean/1).
refs_clean(DocSt) :-
	docstate_currmod(DocSt, Name),
	retractall_fact(refs_entry(_, Name)).

:- export(add_refs_entry/2).
add_refs_entry(Entry, DocSt) :-
	docstate_currmod(DocSt, Name),
	assertz_fact(refs_entry(Entry, Name)).

:- export(save_refs/1).
save_refs(DocSt) :-
	docstate_currmod(DocSt, Name),
	docstate_backend(DocSt, Backend),
	absfile_for_subtarget(Name, Backend, rr, RefsFile),
	open(RefsFile, write, RefsOS),
	push_prolog_flag(write_strings, on),
	( refs_entry(Entry, Name),
%	  display(user_error, refsentry(RefsFile, Entry)), nl(user_error),
	  % quote atoms so that it can be read back properly
	  writeq(RefsOS, Entry),
	  write(RefsOS, '.\n'),
	  fail
	; true
	),
	pop_prolog_flag(write_strings),
	close(RefsOS).

:- use_module(library(write), [writeq/2,write/2]).
:- use_module(library(read), [read/2]).

% ======================================================================

:- doc(section, "Reference Closure (comprising several files)").
% (under expansion of links to external sections (sectlink/1))

:- export(refs_closure_entry/3).
:- data refs_closure_entry/3.

%:- export(compute_refs_closure/1).
compute_refs_closure(DocSt) :-
%	docstate_currmod(DocSt, Name),
	get_mainmod(MainName),
	refs_closure_add_from_file(MainName, DocSt).

% (public)
% (fails if the refs file does not exist)
refs_closure_add_from_file(Base, DocSt) :-
	docstate_backend(DocSt, Backend),
	absfile_for_subtarget(Base, Backend, rr, RefsFile),
	file_exists(RefsFile),
	!,
	docstate_currmod(DocSt, Name),
	open(RefsFile, read, RefsOS),
	( repeat,
	  ( read(RefsOS, Entry) -> true ; fail ),
	  ( Entry = end_of_file ->
	     !
	  ; ( Entry = sectlink(Base2) ->
	        refs_closure_add_from_file(Base2, DocSt)
	    ; assertz_fact(refs_closure_entry(Entry, Base, Name))
	    ),
	    fail
	  )
	; true
	),
	close(RefsOS).
refs_closure_add_from_file(_Base, DocSt) :-
	% TODO: We should emit an error here; all references should be resolved at this point
	Entry = sect(999, "", string_esc("[ERROR-UNRESOLVED]")),
	docstate_currmod(DocSt, Name),
	assertz_fact(refs_closure_entry(Entry, Name, Name)).

:- export(refs_closure_clean/1).
refs_closure_clean(DocSt) :-
	docstate_currmod(DocSt, Name),
	retractall_fact(refs_closure_entry(_, _, Name)).

:- use_module(library(system), [file_exists/1]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Resolving bibliography").

:- use_module(lpdocsrc(src(autodoc_bibrefs)), [resolve_bibliography/1]).

% (execute between scanning and final translation)
:- export(compute_refs_and_biblio/1).
compute_refs_and_biblio(DocSt) :-
	% TODO: [] make sure that all components are processed, then call this, then process again all components
	%       that is not what happens now
	compute_refs_closure(DocSt),
	( \+ doc_opt('-nobiblio', DocSt) ->
	    % Keep the doctree for the bibliography in the state
	    % so that it can be expanded where required
	    resolve_bibliography(DocSt)
	; true
	),
	save_global_refs(DocSt),
	refs_closure_clean(DocSt).

% ---------------------------------------------------------------------------

:- doc(subsection, "Save/restore global references").
% TODO: at this moment, only bibliography

% Save global references
save_global_refs(DocSt) :-
	docstate_currmod(DocSt, Name),
	docstate_backend(DocSt, Backend),
 	absfile_for_subtarget(Name, Backend, gr, File),
	open(File, write, OS),
	%
	( \+ doc_opt('-nobiblio', DocSt) ->
	    ( doc_customdic_get(DocSt, biblio_pairs, RefPairs),
	      doc_customdic_get(DocSt, biblio_doctree, RefsR) ->
	        true
	    ; throw(bug(no_biblio_pairs))
	    ),
	    push_prolog_flag(write_strings, on),
	    writeq(OS, biblio_pairs(RefPairs)),
	    write(OS, '.\n'),
	    writeq(OS, biblio_doctree(RefsR)),
	    write(OS, '.\n'),
	    pop_prolog_flag(write_strings)
	; true
	),
	%
	close(OS).

% Restore global references
restore_global_refs(DocSt) :-
	get_mainmod(MainName),
	docstate_backend(DocSt, Backend),
 	absfile_for_subtarget(MainName, Backend, gr, File),
	open(File, read, OS),
	%
	( \+ doc_opt('-nobiblio', DocSt) ->
	    % Load the bibliography
	    doc_customdic_lookup(DocSt, biblio_pairs, RefPairs),
	    doc_customdic_lookup(DocSt, biblio_doctree, RefsR),
	    read(OS, biblio_pairs(RefPairs)),
	    read(OS, biblio_doctree(RefsR))
	; doc_customdic_lookup(DocSt, biblio_doctree, []),
	  doc_customdic_lookup(DocSt, biblio_pairs, [])
	),
	%
	close(OS).

% ---------------------------------------------------------------------------

:- export(prepare_current_refs/1).
:- pred prepare_current_refs/1 :: docstate
   # "Prepare references for the translation of the current file @var{foo}".
% Restore biblio if necessary and compute navigation and contents
prepare_current_refs(DocSt) :-
	compute_refs_closure(DocSt),
	( % doc_doing_mainmod(DocSt),
	  % TODO: optimize, keep in memory for several passes...
	  % TODO: do not load all biblio (just biblio_pairs?)
          docstate_backend(DocSt, Backend),
	  ( Backend = html ; Backend = texinfo ) -> % TODO: formats hardwired
	    restore_global_refs(DocSt),
	    % Compute navigation links and table of contents for the current file
	    docstate_currmod(DocSt, Name),
	    get_secttree(DocSt, SectTree),
	    get_nav(SectTree, Name, Nav, CurrTree),
	    doc_customdic_lookup(DocSt, fulltree, SectTree),
	    doc_customdic_lookup(DocSt, currtree, CurrTree),
	    doc_customdic_lookup(DocSt, nav, Nav)
	; true
	).

:- export(clean_current_refs/1).
:- pred clean_current_refs/1 :: docstate
   # "Clean the data stored by @pred{prepare_current_refs/1}.".
clean_current_refs(DocSt) :-
	refs_closure_clean(DocSt).

% ---------------------------------------------------------------------------

:- doc(section, "Section Trees").

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [append/3]).

:- export(secttree/1).
:- regtype secttree/1 # "A tree of sections".
secttree(_). % TODO: Define

:- pred get_secttree/2 :: docstate * secttree # "Obtain the complete tree of sections".
get_secttree(DocSt, SectTree) :-
	docstate_currmod(DocSt, Name),
	findall(sect(L, link_to(Base,SectLabel), T),
	        refs_closure_entry(sect(L, SectLabel, T), Base, Name),
		Sects),
	get_secttree_(Sects, _, -1, '__root__', SectTree).

% get_secttree_(+Xs, ?Ys, +BaseLevel, +BaseName, -Nodes):
%
%   Rebuild a tree from a flat representation @var{Xs} of sections.
%   Sections are either level or parent-annotated. @var{Ys} contain
%   the list of pending sections that are not descendants.
get_secttree_([], [], _BaseLevel, _BaseName, []).
get_secttree_([sect(L,Link,T)|Ss], Ss0, BaseLevel, BaseName, Rs) :-
	Link = link_to(N, _),
	% Determine if it is a subsection based on level or parent relationship
	( \+ doclink_is_local(Link), docstr_node(N, _, Parent, Mode) -> Parent = BaseName, NextN = N
	; L > BaseLevel, NextN = '__local__', Mode = normal
	),
	!,
	% Get the subsections
%	display(user_error, foundsub(L, N, Parent, BaseName)), nl(user_error),
	get_secttree_(Ss, Ss1, L, NextN, SubRs),
	( Mode = normal -> Link2 = Link ; Link2 = no_link ),
	R = node(Link2,T,SubRs),
	Rs = [R|Rs1],
	% Continue with the sibling sections
	get_secttree_(Ss1, Ss0, BaseLevel, BaseName, Rs1).
get_secttree_(Ss, Ss, _BaseLevel, _BaseName, []).

:- use_module(.(autodoc_structure), [docstr_node/4]).

:- pred get_nav/4 :: secttree * atm * term * secttree
   # "Obtain some navigation links and the tree for the current section from a global tree".
get_nav(Tree, Curr, Nav, CurrTree) :-
	( tree_df(Tree, [], Nodes, []),
	  % Find @var{Curr} (alongside previous and next nodes) in the
	  % list of nodes @var{Nodes}
	  doclink_at(CurrLink, Curr),
	  Curr0 = df_item(CurrLink, CurrTree0, RootPath),
	  member123(Prev0, Curr0, Next0, Nodes) ->
	    reverse(RootPath, CurrPath),
	    CurrTree = CurrTree0,
	    ( Prev0 = [df_item(Prev1, _, _)] -> Prev = Prev1 ; Prev = no_link ),
	    ( Next0 = [df_item(Next1, _, _)] -> Next = Next1 ; Next = no_link )
	; % TODO: README*.lpdoc files fail here... find a better way to catch this
          fail %throw(bug_get_nav(Curr)) % not found!
	),
	!,
	Nav = nav(CurrPath, Prev, Next).
get_nav(_Tree, _Curr, Nav, CurrTree) :-
	Nav = nav([], no_link, no_link),
	CurrTree = [].

:- use_module(library(lists), [reverse/2]).

% Obtain the list of df_item(Link,Sub,Path), using a depth-first traversal, where
% Path is the path from the tree root to Node, Link is the link in the node, and
% Sub is the subtree of the node.
% 
% Note: links to local sections are not included.
tree_df([], _, Xs, Xs).
tree_df([node(Link,T,Sub)|Ts], RootPath0, Xs, Xs0) :-
	( doclink_is_local(Link) ->
	    Xs = Xs2
	; Xs = [df_item(Link, Sub, RootPath0)|Xs2]
	),
	RootPath = [step(Link,T)|RootPath0],
	tree_df(Sub, RootPath, Xs2, Xs1),
	tree_df(Ts, RootPath0, Xs1, Xs0).

% Prev+[Curr]+Next is a sublist of List.
%   where both Prev and Next are either singleton elements or the
%   empty list
% (nondet)
member123(Prev, Curr, Next, List) :-
	prefix01(Prev, Ns, Ns2), Ns2 = [Curr|Ns1], prefix01(Next, Ns1, _),
	append(_, Ns, List).

% (nondet)
prefix01(Sub, Ns, Ns0) :-
	( Ns = [N|Ns0], Sub = [N] % take one element
	; Ns = Ns0, Sub = [] % do not take anything
	).

% ---------------------------------------------------------------------------

:- export(secttree_resolve/3).
:- pred secttree_resolve(LabelName, Tree, Link) :: string * doctree * doclink
	# "Locate in the section tree @var{Tree} the section with label name
           @var{LabelName} and return the @var{Link} to the section.".

secttree_resolve(LabelName, Tree, Link) :-
	member(node(Link0, _, Subs), Tree),
	( Link0 = link_to(_, Label),
	  ( Label = global_label(LabelName)
	  ; Label = local_label(LabelName)
	  ) ->
	    Link = Link0
	; secttree_resolve(LabelName, Subs, Link)
	).

% ===========================================================================

:- export(get_main_title/2).
:- pred get_main_title(MainTitleR, DocSt) => doctree * docstate 
   # "@var{MainTitleR} is the title of the main section for the whole
      document".

get_main_title(MainTitleR, DocSt) :-
	docstate_currmod(DocSt, Name),
	( Entry = main_title(MainTitleR),
	  refs_closure_entry(Entry, _Base, Name) ->
	    true
	; throw(bug_no_main_title)
	).

