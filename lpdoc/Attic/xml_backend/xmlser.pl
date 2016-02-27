:- module(xmlser, [xml_serialize/2, xml_serialize/3,
		xml_serialize/5, ciao_ns/1, xml_serialize_to_file/2,
		xml_unserialize_from_file/2, xml_serialize_to_file/3,
		xml_unserialize_from_file/3], [assertions, regtypes,
		isomodes]).

%:- use_module(library(format), [format_to_string/3]).
:- use_module(fastformat, [format_to_string/3]).
:- use_module(library(lists), [length/2]).
:- use_module(library(file_utils), [file_to_string/2]).
:- use_module(xmlread, [xml_fragment/3, xml_refine/2, 
		an_xml_node/1]).
:- use_module(xmlwrite, [xml_format_stream/2]).

% ======================================================================

:- comment(title, "A Term/XML serialization library").

:- comment(summary, "This is a simple and extensible library for
	conversion of an arbitrary Prolog term into an equivalent XML
	form and vice versa.").

% ======================================================================

:- regtype ciao_ns(CNS) : atom # "@var{CNS} is namespace URI for
	Ciao term representation in XML".

ciao_ns('http://clip.dia.fi.upm.es/Ciao/2008/terms').

:- comment(ciao_ns/1, "Defined as: @includedef{ciao_ns/1}").

% ======================================================================

:- regtype an_xml_context_binding(Binding) : var * atom #
	"@var{Binding} is an XML serialization context binding".

an_xml_context_binding(Var=Name) :-
	var(Var), atom(Name).

:- comment(an_xml_context_binding/1, "A serialization context binding
	is a term of form @tt{Var=Name}, where @tt{Var} is a free
	variable, and @var{Name} is an atom containing its symbolic
	name, so that multiple occurences of the same variable during
	serialization can be properly substituted.").

% ======================================================================

:- regtype an_xml_ser_context(Context) : list(
	    an_xml_context_binding) # "@var{Context} is an XML
	serialization context".

an_xml_ser_context([]).
an_xml_ser_context([E|R]) :-
	an_xml_context_binding(E),
	an_xml_ser_context(R).

:- comment(an_xml_ser_context/1, "A serialization context is a list
	of variable-to-name bindings that characterize a serialization
	state.  The idea behind serialization context is to ensure
	that multiple occurences of the same variable during
	serialization are properly substituted.  Serialization usually
	starts with an empty context (signifying that no free
	variables have been encountered), and grows with encountering
	new unbound variables.  Usually, serialization context is of
	little interest after the end of serialization.").

:- comment(an_xml_ser_context/1, "User predicates @bf{should} avoid
	direct manipulation of an XML serialization context, and
	instead rely on calling @decl{find_or_create_var_name}").

% ======================================================================

:- multifile xml_serialize_hook/5.

:- pred xml_serialize_hook(?NS, +Cin, ?Term, -Item, -Cout) : atom *
	list * term * term * list # "Within namespace URI @var{NS},
	convert term @var{Term} into XML item @var{Item} or fail.
	@var{Cin} is an input context, and @var{Cout} is to be set to
	the output context.".

:- comment(xml_serialize_hook/5, "Serialization hook is invoked by
	the serializer whenever a Prolog term has to be converted into
	an XML Item.").

:- comment(xml_serialize_hook/5, "Serializer passes either an
	instantiated or an uninstantiated @var{NS} to hooks.  In the
	former case, serializer is looking for any hook that knows how
	to do the conversion, and the successful hook @bf{must}
	instantiate @var{NS} to its proper namespace URI (possibly
	including @tt{''}, the empty namespace).  In the latter case,
	serializer is looking for a hook that is attached to a
	particular namespace URI.").

:- comment(xml_serialize_hook/5, "@var{Term} can be a free variable
	upon call to a serialization hook, and therefore, hooks
	@bf{should} always first check whether @var{Term} is a free
	variable.").

:- comment(xml_serialize_hook/5, "@var{Cin} is the serialization
	context upon call to the hooks, and a successful hook
	@bf{must} set @var{Cout} to the resuting context, i.e. either
	@var{Cin} or an extended context, normally obtained by
	chaining (un)serializations of sub-terms (see comments to
	@decl{an_xml_ser_context/1}).").

% ----------------------------------------------------------------------

:- multifile xml_unserialize_hook/5.

:- pred xml_unserialize_hook(?NS, +Cin, +Item, -Term, -Cout) : atom *
	an_xml_ser_context * term * term * an_xml_ser_context #
"Within namespace URI @var{NS}, convert XML item @var{Item}
	term @var{Tern} or fail.  @var{Cin} is an input context, and
	@var{Cout} is to be set to the output context.".

:- comment(xml_userialize_hook/5, "Unserialization hook is called by
	serializer whenever an XML item (either an element, a
	processing instruction, comment, or a string), has to be
	converted into a prolog term.").

:- comment(xml_unserialize_hook/5, "For more details, see comments to
	@decl{xml_serialize_hook/5}").

% ======================================================================

:- pred xml_serialize(Term, Element) : term * an_xml_node #
"Equivalent to @tt{ xml_serialize( _, [], @var{Term},
	@var{Element}, _)}.".

xml_serialize(Term, Element) :-
	xml_serialize(_, Term, Element).

% ======================================================================

:- pred xml_serialize(NS, Term, Element) : atom * term *
	an_xml_node # "Equivalent to @tt{ xml_serialize( @var{NS},
	[], @var{Term}, @var{Element}, _)}.".

xml_serialize(NS, Term, Element) :-
	xml_serialize(NS, [], Term, Element, _).

% ======================================================================

:- pred xml_serialize(?NS, +Cin, ?Term, -Item, Cout) : atom *
	an_xml_ser_context * term * an_xml_node * an_xml_ser_context #
"Serialize @var{Term} (possibly a free variable) into an XML
	item @var{Item}, matching or determining namespace URI
	@var{NS}.  Take initial context from @var{Cin}, and leave
	resulting context in @var{Cout}.".


:- pred xml_serialize(?NS, +Cin, ?Term, +Item, Cout) : atom *
	an_xml_ser_context * term * an_xml_node * an_xml_ser_context #
"Unserialize XML item @var{Item} into a Prolog term @var{Term}
	(possibly a free variable), matching or determining namespace
	URI @var{NS}.  Take initial context from @var{Cin}, and leave
	resulting context in @var{Cout}.".

xml_serialize(NS, Cin, Term, Item, Cout) :-
	( var(Item)
	-> ( xml_serialize_hook(NS, Cin, Term, Item, Cout)
	    -> true
	    ; ciao_ns(NS),
		xml_ciao_serialize(NS, Cin, Term, Item, Cout), !
	    )
	; ( xml_unserialize_hook(NS, Cin, Item, Term, Cout)
	    -> true
	    ; ciao_ns(NS),
		xml_ciao_unserialize(NS, Cin, Item, Term, Cout), !
	    )
	).

:- comment(xml_serialize/5, "The two use cases for this predicates
	are mutually differentiated on the basis of @var{Item}: if it
	is a free variable, serialization from @var{Term} to
	@var{Item} is done, and otherwise unserialization from
	@var{Item} to @var{Term}.").

:- comment(xml_serialize/5, "In both cases, successful serialization
	@bf{must} either match or instantiate @var{NS}, which may be
	on entry either free or instantiated to target namespace
	URI.").

:- comment(xml_serialize/5, "Serialization is done first by calling
	the appropriate hooks, and, in case they fail, by performing
	cannonical XML serialization attached to @decl{ciao_ns/1}
	namespace URI.").


:- comment(xml_serialize_hook/5, "@var{Cin} is the serialization
	context before call to the hooks, and a successful hook
	@bf{must} set @var{Cout} to the resuting context, i.e. either
	@var{Cin} or an extended context, normally obtained by
	chaining (un)serializations of sub-terms (see comments to
	@decl{an_xml_ser_context/1}).").

% ......................................................................

xml_serialize_list(_,  C1, [],    [],          C1) :- !.
xml_serialize_list(NS, C1, [A|R], [SerA|SerR], C2) :-
	xml_serialize(NS, C1, A, SerA, C1a),
	xml_serialize_list(NS, C1a, R, SerR, C2).

% ----------------------------------------------------------------------

xml_ciao_serialize(CNS, C1, V, env(CNS:var, [name=Name], []), C2) :-
	var(V), !,
	find_or_create_var_name(C1, V, C2, Name).
xml_ciao_serialize(CNS, C1, [], env(CNS:'empty-list', [], []), C1) :-
	!.
xml_ciao_serialize(CNS, C1, S, env(CNS:string, [], [S]), C1) :-
	proper_string(S), !.
xml_ciao_serialize(CNS, C1, L, env(CNS:list, [], SerL), C2) :-
	proper_list(L), !,
	xml_serialize_list(_, C1, L, SerL, C2).
xml_ciao_serialize(CNS, C1, N, env(CNS:number, [value=N], []), C1) :-
	number(N), !.
xml_ciao_serialize(CNS, C1, A, env(CNS:'atom', [name=A], []), C1) :-
	atom(A), !.
xml_ciao_serialize(CNS, C1, [Head|Tail],
	    env(CNS:'cons', [], [SerHead, SerTail]), C2) :-
	!,
	xml_serialize(_, C1,  Head, SerHead, C1a),
	xml_serialize(_, C1a, Tail, SerTail, C2).
xml_ciao_serialize(CNS, C1, Struct, env(CNS:struct,
		[functor=Functor, arity=Arity], SerArgs), C2) :-
	Struct=.. [Functor|Args],
	length(Args, Arity),
	xml_serialize_list(_, C1, Args, SerArgs, C2).

% ......................................................................

find_or_create_var_name(C1, V, C2, Name) :-
	( find_var_name(C1, V, Name)
	-> C2= C1
	; length(C1, L), K is L+1,
	    format_to_string("_~d", [K], S),
	    atom_codes(Name, S),
	    C2= [V=Name|C1]
	).

% ......................................................................

find_var_name([W=Name|_], V, Name) :-
	W==V, !.
find_var_name([_|R], V, Name) :-
	find_var_name(R, V, Name).

% ......................................................................

proper_list(V) :- var(V), !, fail.
proper_list([]) :- !.
proper_list([_|B]) :- proper_list(B).

% ......................................................................

proper_string(V) :- var(V), !, fail.
proper_string([]) :- !.
proper_string([X|B]) :-
	number(X), X>= -128, X=<255, proper_string(B).

% ======================================================================

xml_ciao_unserialize(CNS, C1, env(CNS:var, Attribs, C), V, C2) :-
	!, member(name=Name, Attribs),
	xml_refine(C, []),
	( member(V=Name, C1)
	-> C2= C1
	; C2= [V=Name|C1]
	).
xml_ciao_unserialize(CNS, C1, env(CNS:'empty-list', _, C), [], C1) :-
	!, xml_refine(C, []).
xml_ciao_unserialize(CNS, C1, env(CNS:'string', _, C), S, C1) :-
	!, xml_refine(C, [S]),
	proper_string(S).
xml_ciao_unserialize(CNS, C1, env(CNS:'list', _, C), L, C2) :-
	!, xml_refine(C, CR),
	xml_unserialize_list(_, C1, CR, L, C2).
xml_ciao_unserialize(CNS, C1, env(CNS:'number', A, C), N, C1) :-
	!, member(value=NA, A),
	xml_refine(C, []),
	( number(NA)
	-> N=NA
	; atom_codes(NA, NAC),
	    number_codes(N, NAC)
	).
xml_ciao_unserialize(CNS, C1, env(CNS:'atom', A, C), N, C1) :-
	!, member(name=NA, A),
	xml_refine(C, []),
	( atom(NA)
	-> N= NA
	; number_codes(NA, NAC),
	    atom_codes(N, NAC)
	).
xml_ciao_unserialize(CNS, C1, env(CNS:'cons', _, C), [H|T], C2) :-
	!, xml_refine(C, [HS, TS]),
	xml_ciao_unserialize(CNS, C1,  HS, H, C1a),
	xml_ciao_unserialize(CNS, C1a, TS, T, C2).
xml_ciao_unserialize(CNS, C1, env(CNS:'struct', A, C), S, C2) :-
	member(functor=FN, A),
	member(arity=AN,   A),
	( atom(FN)
	->   Functor= FN
	; number_codes(FN, FNC),
	    atom_codes(Functor, FNC)
	),
	( number(AN)
	-> Arity= AN
	; atom_codes(AN, ANC),
	    number_codes(Arity, ANC)
	),
	xml_refine(C, RC),
	xml_unserialize_list(_, C1, RC, Args, C2),
	length(Args, Arity),
	S =.. [Functor|Args].

% ......................................................................

xml_unserialize_list(_,   C1, [],    [],      C1).
xml_unserialize_list(CNS, C1, [A|R], [UA|UR], C2) :-
	xml_serialize(_, C1, UA, A, C1a),
	xml_unserialize_list(CNS, C1a, R, UR, C2).

% ......................................................................

end_lookahead([], []).

% ======================================================================

:- pred xml_serialize_to_file(+Term, +Filename) : term * atom #
"Equivalent to @tt{xml_serialize_to_file( _, @var{Term},
	@var{Filename})}".

xml_serialize_to_file(Term, Filename) :-
	xml_serialize_to_file(_, Term, Filename).

% ======================================================================

:- pred xml_serialize_to_file(?NS, +Term, +Filename) : atom * term *
	atom # "Write XML-serialized form of @var{Term} to file
	@var{Filename}, matching or instatiating namespace URI
	@var{NS}.".

xml_serialize_to_file(NS, Term, Filename) :-
	open(Filename, write, OS),
	( catch((
		    xml_serialize(NS, Term, XML),
		    xml_format_stream(OS, [XML])
		),
		E,
		(
		    close(OS),
		    throw(E)
		))
	-> close(OS)
	; close(OS), fail
	).

% ======================================================================

:- pred xml_unserialize_from_file(-Term, +Filename) : term * atom #
"Equivalent to @tt{xml_unserialize_from_file( _, @var{Term},
	@var{Filename})}.".

xml_unserialize_from_file(Term, Filename) :-
	xml_unserialize_from_file(_, Term, Filename).

% ======================================================================

:- pred xml_unserialize_from_file(?NS, -Term, +Filename) : atom *
	term * atom # "Read XML-serialized form from file
	@var{Filename} into @var{Term}, matching or instantiating
	namespace URI @var{NS}.".

xml_unserialize_from_file(NS, Term, Filename) :-
	file_to_string(Filename, S),
	xml_fragment(Frag, S, _),
	xml_refine(Frag, [SerForm]),
	xml_serialize(NS, Term, SerForm).

% ======================================================================

% An example:

% ?- _In= fudo2(X, a*X^2+b*X+C, [1,a,2], "abcdef", [1,a|2]), xml_serialize( NS, _In, _Ser), xml_format( [_Ser]), xml_serialize( NS, Out, _Ser). 

% <struct xmlns="http://clip.dia.fi.upm.es/Ciao/2008/terms"
%      functor="fudo2" arity="5">
%     <var name="_1"/>
%     <struct functor="+" arity="2">
%         <struct functor="+" arity="2">
%             <struct functor="*" arity="2">
%                 <atom name="a"/>
%                 <struct functor="^" arity="2">
%                     <var name="_1"/>
%                     <number value="2"/>
%                 </struct>
%             </struct>
%             <struct functor="*" arity="2">
%                 <atom name="b"/>
%                 <var name="_1"/>
%             </struct>
%         </struct>
%         <var name="_2"/>
%     </struct>
%     <list>
%         <number value="1"/>
%         <atom name="a"/>
%         <number value="2"/>
%     </list>
%     <string>abcdef</string>
%     <cons>
%         <number value="1"/>
%         <cons>
%             <atom name="a"/>
%             <number value="2"/>
%         </cons>
%     </cons>
% </struct>
% NS = 'http://clip.dia.fi.upm.es/Ciao/2008/terms',
% Out = fudo2(_A,a*_A^2+b*_A+_,[1,a,2],[97,98,99,100,101,102],[1,a|2]) ? 
% yes
