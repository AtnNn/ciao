:- module(xmlread, [an_xml_element/1, an_xml_attrib/1, an_xml_node/1,
		a_prefix_def/1, xml_element/3, xml_element/5, xml_fragment/3,
		xml_fragment/5, translate_entities/2, normalize_spaces/2,
		xml_refine/2]).

:- use_module(lexutils).
:- use_module(library(lists), [reverse/2, append/3]).

:- use_package([assertions, regtypes, isomodes, fsyntax]).

% ======================================================================

:- comment(title, "An XML parsing library").

:- comment(subtitle, "XML reader with namespace semantics").

:- comment(author, "Dragan Ivanovi@'{c} (@email{idragan@@clip.dia.fi.upm.es})").

:- comment(copyright, "@copyright  2007 CLIP Team").

:- comment(summary, "This library contains predicates that read @bf{XML}
	documents and fragments from string source, while correctly
	treating XML Namespace semantics.").

:- comment(module, "(A module level comment) This library contains
	predicates that read @bf{XML} documents and fragments from
	string source, while correctly treating XML Namespace
	semantics.").

% ======================================================================

:- regtype an_xml_element(Node) # "@var{Node} is a parsed XML element".

:- comment(an_xml_element/1, "Defined as: @includedef{an_xml_element/1}").

an_xml_element(env(Name, Attribs, Contents)) :-
	atm(Name), list(Attribs, an_xml_attrib),
	list(Contents, an_xml_node).
an_xml_element(env(NamespaceURI:Name, Attribs, Contents)) :-
	atm(NamespaceURI), atm(Name), list(Attribs, an_xml_attrib),
	list(Contents, an_xml_node).

% ----------------------------------------------------------------------

:- regtype an_xml_attrib(Attrib) # "@var{Attrib} is a parsed attribute
	specification".

:- comment(an_xml_attrib/1, "Defined as: @includedef{an_xml_attrib/1}").

an_xml_attrib(Name=Value) :-
	atm(Name), atm(Value).
an_xml_attrib(NamespaceURI:Name=Value) :-
	atm(NamespaceURI), atm(Name), atm(Value).

% ----------------------------------------------------------------------

:- regtype an_xml_node(Node) # "@var{Node} is a parsed XML node".

:- comment(an_xml_node/1, "Defined as: @includedef{an_xml_node/1}").

an_xml_node(~an_xml_element).
an_xml_node(proci(Lang, Contents)) :-
	atm(Lang), string(Contents).
an_xml_node(entity(~atm)).
an_xml_node(comment).
an_xml_node(~string).

% ----------------------------------------------------------------------

:- regtype a_prefix_def(PrefixDef) # "@var{PrefixDef} is a parsed
	prefix definition".

:- comment(a_prefix_def/1, "Defined as: @includedef{a_prefix_def/1}").

a_prefix_def(Prefix=NamespaceURI) :-
	atm(Prefix),
	atm(NamespaceURI).

% ======================================================================

:- pred xml_element(-E, +InString, -RestString) : an_xml_element *
	string * string # "Same as @tt{xml_element( '', [], E,
	InString, RestString)}".

xml_element(E) -->
	xml_element('', [], E).

% ----------------------------------------------------------------------

:- pred xml_element(+NS, +PL, -E, +InString, -RestString) : atm *
	list(a_prefix_def) * an_xml_element * string * string #
"Parses @var{E} from @var{InString} or fails, assuming current
	nameaspace @var{NS}, and using namespace prefixes from
	@var{PL}".

xml_element(NS, PL, E) -->
	spaces,
	element_tag_start(Prefix, NCName), % Detect opening tag
	element_tag_rest(Prefix, NCName, NS, PL, E). % Read the rest

% Recognize element's opening tag
element_tag_start(Prefix, NCName) --> % Detect start of the tag
	"<", spaces, qname(Prefix, NCName).

% Parse the rest of the opening tag, as well as the content
element_tag_rest(Prefix, NCName, NS0, PL, env(QName, Attribs, Content)) -->
	attribs(PL, [], NPL, Attribs0), % Read attributes in rev. order
	{reverse(Attribs0, Attribs0a)}, % Reverse attrib. list
	{resolve_ns(NS0, Prefix, NPL, Attribs0a, NS, SubNS),
	    replace_attribs(NPL, Attribs0a, [], Attribs0b),
	    reverse(Attribs0b, Attribs0c),
	    combine_element_qname(NS, NCName, QName),
	    add_ns_data(NS, NPL, Attribs0c, Attribs)
	}, elem_content(SubNS, NPL, Prefix, NCName, Content).

% Parse attributes
attribs(P0, A0, P1, A1) -->
	spaces, "xmlns:", ncname(Prefix), spaces, "=",
	string_literal_trimmed(NS),
	{addpfx(Prefix, NS, P0, P0a)},
	attribs(P0a, A0, P1, A1).
attribs(P0, A0, P1, A1) -->
	spaces, qname(Prefix, NCName), spaces, "=", string_literal(Value),
	attribs(P0, [Prefix:NCName=Value|A0], P1, A1).
attribs(P, A, P, A) -->
	[].

% Combine expanded lamespace and local name
combine_element_qname('', N, N) :- !.
combine_element_qname(NS, N, NS:N).

% Add prefix definitions to element's attributes
add_ns_data(_, NPL, A0, A1) :-
	add_ns_data_1(NPL, NPL1),
	append(A0, NPL1, A1).

add_ns_data_1([],       []).
add_ns_data_1([P=NS|A], [xmlns:P=NS|B]) :-
	add_ns_data_1(A, B).

% Read string literal for attribute values

string_literal(A) -->
	string_1(S), {atom_codes(A, S)}.

% Read string literal for attribute values and then trim leading and
% trailing whitespace

string_literal_trimmed(A) -->
	string_1(S0), {trim_spaces(S0, S), atom_codes(A, S)}.

string_1(S) --> spaces, [Delim], {member(Delim, "\"'")},
	string_collect(Delim, S1),
	{translate_entities(S1, S)}.

string_collect(Delim, []) --> [Delim], !.

string_collect(Delim, [X|S]) --> [X], string_collect(Delim, S).

% Resolve namespace URI of the currently parsed element and the
% default namespace URI for its children elements based on parent's
% namespace URI and xmlns-type attributes

resolve_ns(_, '', _, Attribs, NS, NS) :-
	member('':xmlns=NS0, Attribs), !,
	trim_spaces(NS0, NS).
resolve_ns(NS,  '',     _,  _, NS, NS) :- !.
resolve_ns(NS0, Prefix, PL, _, NS, NS0) :-
	member(Prefix=NS, PL), !.
resolve_ns(NS0, _, _, _, '', NS0).

% Replace attributes, by taking into account new and changed namespace
% prefix definitions

replace_attribs(_,  [],        A1, A1).
replace_attribs(PL, [P:N=V|A], A0, A1) :-
	replace_attrib(PL, P, N, V, A0, Aa),
	replace_attribs(PL, A, Aa, A1).

replace_attrib(_,  '', xmlns, _, A,  A) :- !.
replace_attrib(_,  '', N,     V, A0, [N=V|A0]) :- !.
replace_attrib(PL, P,  N,     V, A0, [NS:N=V|A0]) :-
	P\='', member(P=NS, PL), !.
replace_attrib(_, P, N, V, A0, [P:N=V|A0]).

% After having had attributes parsed, finish parsing of the element's
% opening tag, its content and the closing tag

elem_content(NS, PL, Prefix, NCName, Content) -->
	spaces, ">",
	complex_content(NS, PL, Prefix, NCName, Content), !,
	spaces, "<", spaces, "/",
	spaces, qname(Prefix, NCName),
	spaces, ">".

elem_content(_, _, _, _, []) --> spaces, "/>", !.

% ----------------------------------------------------------------------

% TODO: are arguments swapped? (JF) 
:- pred translate_entities(+S0, -S1) : string * string # "Decode
	standard XML entities (&gt; &lt; &quot; &apos; and &amp;.)
	from @var{S0} into @var{S1}".

:- pred translate_entities(-S0, +S1) : string * string # "Encode XML
	special chars (>, <, \", ' and &) from @var{S1} to standard XML
	entities (&gt; &lt; etc.) into @var{S0}".

translate_entities(S0, S1) :-
	( nonvar(S1) -> translate_1(S0, S1, _)
	; nonvar(S0) -> translate_2(S1, S0, _) ).

translate_1([0'"|S]) --> "&quot;", !, translate_1(S).
translate_1([0''|S]) --> "&apos;", !, translate_1(S).
translate_1([0'&, 0'#|S]) --> "&#", !, translate_1(S).
translate_1([0'&|S]) --> "&amp;", !, translate_1(S).
translate_1([0'<|S]) --> "&lt;", !, translate_1(S).
translate_1([0'>|S]) --> "&gt;", !, translate_1(S).
translate_1([X1|S]) --> [X], !, {X<0 -> X1 is 256+X ; X1=X},
	translate_1(S).
translate_1([]) --> [].

translate_2([0'&, 0'q, 0'u, 0'o, 0't, 0';|S]) --> "\"", !,
	translate_2(S).
translate_2([0'&, 0'a, 0'p, 0'o, 0's, 0';|S]) --> "'", !,
	translate_2(S).
translate_2([0'&, 0'a, 0'm, 0'p, 0';|S]) --> "&", !,
	translate_2(S).
translate_2([0'&, 0'l, 0't, 0';|S]) --> "<", !,
	translate_2(S).
translate_2([0'&, 0'g, 0't, 0';|S]) --> ">", !,
	translate_2(S).
translate_2([X1|S]) --> [X], !,
	{X<0 -> X1 is X+256 ; X1=X}, translate_2(S).
translate_2([]) --> [].

% ----------------------------------------------------------------------

:- pred normalize_spaces(+S, -NS) : string * string # "Remove leading
	and trailing spaces from @var{S}, and replace any other
	whitespace in with a single blank.  Store the result in @var{NS}.".

normalize_spaces(S, NS) :-
	norm_1([], S, RNS), reverse(RNS, NS).

norm_1(C, [],    C).
norm_1(C, [X|S], RNS) :-
	space(X), !, spaces(S, S1),
	norm_1([32|C], S1, RNS).
norm_1(C, [X|S], RNS) :-
	norm_1([X|C], S, RNS).

% ----------------------------------------------------------------------

:- pred xml_fragment(-Frag, +InStr, -RestStr) : list(an_xml_node) *
	string * string # "Same as @tt{xml_fragment( '', [],
	@var{Frag}, @var{InStr}, @var{RestStr})}".

xml_fragment(Frag) -->
	xml_fragment('', [], Frag).

% ----------------------------------------------------------------------

:- pred xml_fragment(+NS, +PL, -Frag, +InStr, -RestStr) : atm * list(
	    a_prefix_def) * list(an_xml_node) * string * string #
"Assuming current namespace @var{NS} and list of known
	namespace prefixes @var{PL}, parse @var{InStr} for a valid XML
	nodes, store them into @var{Frag}, and return the rest of
	input string into @var{RestStr}".

xml_fragment(NS, PL, Frag) -->
	complex_content(NS, PL, '', '', Frag).

complex_content(NS, PL, Prefix, NCName, C) -->
	( (non_substantial_whitespace ; []),
	    content_markup(NS, PL, E)
	-> {( E==comment
		-> C= C1
		; C= [E|C1]
		)
	    },
	    complex_content(NS, PL, Prefix, NCName, C1)
	; (end_lookahead ; lt_lookahead)
	-> {C=[]}
	; get_text(Text),
	    {translate_entities(TextT, Text),
		C= [TextT|C1]
	    },
	    complex_content(NS, PL, Prefix, NCName, C1)
	).

content_markup(_, _, comment) -->
	"<!--", !, skip_comment.
content_markup(_, _, proci(LANG, PI)) -->
	"<?", !, get_proci(LANG, PI0),
	{translate_entities(PI, PI0)}.
content_markup(_, _, CData) -->
	"<![CDATA[", !, get_cdata(CData).
content_markup(NS, PL, E) -->
	element_tag_start(Prefix, NCName), !,
	element_tag_rest(Prefix, NCName, NS, PL, E).

get_proci(LANG, PI) -->
	ncname(LANG), !, spaces, get_proci_1(PI).
get_proci('', PI) -->
	spaces, get_proci_1(PI).

get_proci_1([]) --> spaces, "?>", !.
get_proci_1([X|R]) --> [X], get_proci_1(R).

get_cdata([]) --> "]]>", !.
get_cdata([X|R]) --> [X], get_cdata(R).

get_text([]) --> (end_lookahead ; lt_lookahead).
get_text([X|R]) --> [X], get_text(R).

end_lookahead([], []).

lt_lookahead([0'<|R], [0'<|R]).

skip_comment --> "-->", !.
skip_comment --> [_], skip_comment.


addpfx(P, NS, PL, PL1) :-
	append(A, [P=_|B], PL) -> append(A, [P=NS|B], PL1) ;
	PL1 = [P=NS|PL].

% combine_elem(comment, C, C) :- !.
% combine_elem([],      C, C) :- !.
% combine_elem(E,       C, [E|C]).

% ======================================================================

:- pred xml_refine(+Content, -RefinedContent) : list(an_xml_node) *
	list(an_xml_node) # "Refine XML content by removing comments,
	processing instructions, and text nodes consisting only of
	whitespace, and joining adjacent strings.".

xml_refine(Content, RefinedContent) :-
	nonvar(Content),
	xml_refine_1(RC, Content, []),
	join_adjacent_strings(RC, RefinedContent).

% ......................................................................

xml_refine_1([]) --> end_lookahead, !.
xml_refine_1(C) --> [comment], !, xml_refine_1(C).
xml_refine_1(C) --> [proci(_, _)], !, xml_refine_1(C).
xml_refine_1(C) --> [S], {string(S), spaces(S, [])}, !, xml_refine_1(C).
xml_refine_1([X|C]) --> [X], xml_refine_1(C).

% ......................................................................

join_adjacent_strings([],       []).
join_adjacent_strings([X],      [X]).
join_adjacent_strings([X, Y|A], B) :-
	string(X), string(Y), !,
	append(X, Y, Z),
	join_adjacent_strings([Z|A], B).
join_adjacent_strings([X, Y|A], [X|B]) :-
	join_adjacent_strings([Y|A], B).

% ......................................................................

% transl_table( S, C):- 
% 	transl_table_int( T), 
% 	member( S=C, T).
