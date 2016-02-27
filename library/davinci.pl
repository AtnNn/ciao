
:- module(davinci,
	[ davinci/0,
	  topd/0,
	  davinci_get/1,
	  davinci_get_all/1,
	  davinci_put/1,
	  davinci_quit/0,
	  davinci_ugraph/1,
	  davinci_lgraph/1,
	  ugraph2term/2,
	  formatting/2
	],
	[ assertions ]).

:- use_module(library(aggregates),[findall/3]).
:- use_module(library(prompt),[prompt_for/2]).
:- use_module(library(errhandle),[error_protect/1]).  
:- use_module(library(format),[format/3]).
:- use_module(library(read),  [read/1]).
:- use_module(library('graphs/ugraphs'),[vertices_edges_to_ugraph/3]).
:- use_module(library(write), [write/2,writeq/1]).
:- use_module(library(system),[exec/3,popen/3]).

:- comment(author,"Francisco Bueno").
:- comment(module,
	"This library allows connecting a Ciao Prolog application
	 with daVinci V2.X.

         The communication is based on a two-way channel: after daVinci
         is started, messages are sent in to it and read in from it
         on demand by different Prolog predicates. Messages are sent via
         writing the term as text; messages are received by reading text
         and returning an atom. Commands sent and answers received are
         treated as terms from the Prolog side, since for daVinci they
         are text but have term syntax; the only difficulty lies in
         strings, for which special Prolog syntax is provided.

         See accompanying file @tt{library('davinci/commands')} for
         examples on the use of this library.

         daVinci is developed by U. of Bremen, Germany.").

% daVinci's executable
command('daVinci -pipe').

:- data davinci/2,
	answer/1.

% a command which can be used to synchronize with daVinci
ask(nothing).

% answer from daVinci when it is ready on startup
% and after each command
ready(ok).
% another possible answer
return(answer_string(_)).

% answer from daVinci when it finds errors
error(communication_error(_)).

% the command to quit
quit(quit).

% -------------------------------------------------------------------------
% davinci/0
% start up a daVinci process
% -------------------------------------------------------------------------

davinci :-
	\+ davinci(_,_),
	command(Command),
	intercept(exec(Command,In,Out),error(Error,Where),
	          handle_error(Error,Where)),
%	popen(Command,write,In), Out=user_input,
%	In=user_output, Out=user_input,
	ready(Ok),
	prompt_for(Out,X),
	X=Ok,
	asserta_fact(davinci(In,Out)).

handle_error(Error, Where) :-
        current_output(S),
        set_output(user_error),
        display('{ERROR: '),
        display(Where),
        display(' - '),
        display(Error),
        display('}'),
        nl,
        set_output(S),
        fail.

% -------------------------------------------------------------------------
% topd/0
% a toplevel to send to daVinci commands from standard input
% -------------------------------------------------------------------------

topd :-
	davinci,
	repeat,
	  read(X),
	  ( X=end_of_file
	  -> true
	   ; X=in
	  -> davinci_get(M),
	     writeq(M),
	     nl,
	     fail
	   ; davinci_put(X),
	     fail
	  ), !,
	davinci_quit.

% -------------------------------------------------------------------------
% davinci_quit/0
% exit daVinci process
% all pending answers are lost!
% -------------------------------------------------------------------------

davinci_quit :-
	quit(Q),
	davinci_put0(Q),
	davinci(In,Out),
	close(In),
	close(Out),
	retract_fact(davinci(_,_)),
	retractall_fact(answer(_)).

% -------------------------------------------------------------------------
% davinci_put(+Term)
% send a command
% syntactically, a command is a term
% semantically, it has to correspond to a command understood by daVinci
% two terms are interpreted in a special way: string/1 and text/1
% string(Term) is given to daVinci as "Term"
% text(List) is given as "Term1\nTerm2\n...Term\n" for each Term in List
% if your term has functors string/1 and text/1 that you don't want to
%    be interpreted this way, use it twice, i.e.,
%    string(string(Term)) is given to daVinci as string(Term')
%    where Term' is the interpretation of Term
% -------------------------------------------------------------------------

davinci_put(Term):-
	davinci_put0(Term),
	davinci(_In,Out),
	repeat,
	  prompt_for(Out,Atom),
	  atom2term(Atom,Answer),
	  ( ready(Answer)
	  -> true
	   ; error(Answer)
	  -> throw(Answer)
	   ; assertz_fact(answer(Answer)),
	     ( return(Answer)
	     -> true
	      ; fail
	     )
	  ), !.

davinci_put0(Term):-
	davinci(In,_Out),
	formatting(Term,In),
	nl(In),
	flush_output(In).

formatting([], _In).
formatting(Term,In):-
	atomic(Term), !,
	write(In,Term).
formatting(L, In):-
        L = [_|_], !,
        format_list(L, In).
formatting(Functor, In):-
        format_struct(Functor, In).

format_list(L, In):-
        write(In, '['),
        format_list_elements(L, In),
        write(In, ']').

format_list_elements([Element], In):- !,
        format_internal_item(Element, In).
format_list_elements([This|Rest], In):-
        format_internal_item(This, In),
        write(In, ','),
        format_list_elements(Rest, In).

format_struct(Struct, In):-
        functor(Struct, Name, Arity),
        write(In, Name),
        write(In, '('),
        format_args(Struct, 1, Arity, In),
        write(In, ')').

format_args(Functor, N, N, In):- !,
        arg(N, Functor, Arg),
        format_internal_item(Arg, In).
format_args(Functor, N, M, In):-
        N < M,
        arg(N, Functor, Arg),
        format_internal_item(Arg, In),
        write(In, ','),
        N1 is N + 1,
        format_args(Functor, N1, M, In).

format_internal_item(Atom, In):-
        atomic(Atom), !,
        write(In, Atom).
format_internal_item(List, In):-
        List = [_|_], !,
        format_list(List, In).
format_internal_item(string(Text), In):- !,
        format(In, """~w""", [Text]).
format_internal_item(text(Text), In):- !,
        write(In, '"'), %% "
        write_list(Text, In),
        write(In, '"'). %% "
format_internal_item(Functor, In):-
        format_struct(Functor, In).

write_list([A|As],In):-
        format_internal_item(A, In),
	write_list(As,In).
write_list([],_In).

% -------------------------------------------------------------------------
% davinci_sync
% wait for daVinci to answer back
% Term is as in davinci_put/1, but has to be a valid question (question/3)
% Answer is a term corresponding to daVinci's answer
% -------------------------------------------------------------------------

davinci_sync:-
	ask(NullMess),
	davinci_put0(NullMess),
	davinci(_In,Out),
	repeat,
	  prompt_for(Out,Atom),
	  atom2term(Atom,Answer),
	  ( ready(Answer)
	  -> true
	   ; assertz_fact(answer(Answer)),
	     fail
	  ), !.

% -------------------------------------------------------------------------
% davinci_get(-Term)
% get a message from daVinci
% Term is a term corresponding to daVinci's message
% -------------------------------------------------------------------------

davinci_get(Term):-
	retract_fact(answer(Term)), !.
davinci_get(Term):-
	davinci_sync,
	( retract_fact(answer(Term))
	-> true
	 ; Term = nil
	).

% -------------------------------------------------------------------------
% davinci_get_all(-List)
% get all pending messages
% List is a list of terms as in davinci_get/1
% -------------------------------------------------------------------------

davinci_get_all(List):-
	davinci_sync,
	findall(Answer,retract_fact(answer(Answer)),List).

% -------------------------------------------------------------------------
% davinci_ugraph(+Graph)
% send a graph
% Graph is a term which denotes an ugraph as in SICStus3 library(ugraphs)
% vertices of the form node/2 are interpreted in a special way
% node(Term,List) is interpreted as a vertex Term with attributes List
% List is a list of terms conforming the syntax of davinci_put/1 and
%      corresponding to daVinci's graph nodes attributes
% if your vertex has functor node/2 and you don't want it to be interpreted
%      this way, use it twice, i.e.,
%      node(node(T1,T2),[]) is given to daVinci as vertex node(T1,T2)
% a vertex is used both as label and name of daVinci's graph node
% daVinci's graph edges have label V1-V2 where V1 is the source and
%      V2 the sink of the edge
% there is no support for multiple edges between the same two vertices
% -------------------------------------------------------------------------

davinci_ugraph(Graph):-
	ugraph2term(Graph,Term),
	davinci_put(graph(new(Term))).

ugraph2term([N-Ns|Graph],[Node|Term]):-
	node2term(N,V,As),
	Node=l(string(V),n(string(''),As,Edges)),
	edges(Ns,V,Edges),
	ugraph2term(Graph,Term).
ugraph2term([],[]).

node2term(node(N,As0),N,As):- !,
	( As0=[]
	-> As=[a(string('OBJECT'),string(N))]
	 ; As=As0
	).
node2term(N,N,As):-
	As=[a(string('OBJECT'),string(N))].

edges([N|Ns],Source,[Edge|Edges]):-
	node2term(N,V,_),
	Edge=l(string(Source-V),e(string(''),[],r(string(V)))),
	edges(Ns,Source,Edges).
edges([],_Source,[]).

 %% ugraph2davinciterm(U):-
 %%         ugraph2term(U, T),
 %%         formatting(T, user_output).

% -------------------------------------------------------------------------
% davinci_lgraph(+Graph)
% send a labeled graph
% Graph is a term which denotes a wgraph as in SICStus3 library(wgraphs)
%       except that the weights are labels, i.e.,
%       they do not need to be integers
% vertices of the form node/2 are interpreted in a special way
% edge labels are converted into special intermediate vertices
% duplicated labels are solved by adding dummy atoms ''
% there is no support for multiple edges between the same two vertices
% -------------------------------------------------------------------------

davinci_lgraph(Graph):-
	retractall_fact(label(_)),
	lgraph2ugraph(Graph,UGraph),
	davinci_ugraph(UGraph).

% This one is similar to wgraph_to_ugraph/2 in SICStus3 library(wgraphs)
% except that the labels (weights) are converted into new vertices
% These new vertices are special, in the sense that they have daVinci
% attributes so that they will appear as text instead of box nodes
lgraph2ugraph(LGraph,UGraph):-
	lgraph2edges(LGraph,Edges),
	vertices_edges_to_ugraph([],Edges,UGraph).

lgraph2edges([N-LNs|LGraph],Edges):-
	edgelist2edges(LNs,N,Edges,Edges0),
	lgraph2edges(LGraph,Edges0).
lgraph2edges([],[]).

edgelist2edges([N2-L|LNs],N0,[N0-N1,N1-N2|Edges],Edges0):-
	davinci_special_node(L,N1),
	edgelist2edges(LNs,N0,Edges,Edges0).
edgelist2edges([],_N0,Edges,Edges).

davinci_special_node(L0,node(L,Attrs)):-
	Attrs=[a(string('OBJECT'),Name),
	       a(string('_GO'),string(text))],
	solve_dup_label(L0,L),
	asserta_fact(label(L)),
	node_name(L0,Name).

:- data label/1.

solve_dup_label(L,L1):-
	label(L), !,
	add_dummy(L,L0),
	solve_dup_label(L0,L1).
solve_dup_label(L,L).

add_dummy(L,[' '|L]):- L=[_|_], !.
add_dummy(L,[' ',L]).

node_name(L,Name):-
	list(L), !,
	Name=text(L).
node_name(L,string(L)).

% -------------------------------------------------------------------------
% atom2term(+Atom,-Term)
% convert daVinci's messages into terms
% Atom is an atom, but must have term syntax
% Term is a term resulting from parsing Atom char by char
% Note: there should be a standard way to do this! This one is ad-hoc
% -------------------------------------------------------------------------

atom2term(Atom,Term):-
	name(Atom,String),
	parse(String,Term).

parse(String,Term):- parse_struct(String,Term,_).

parse_struct(String0,Term,String):-
	parse_functor(String0,Functor,String1),
	parse_args(String1,Args,String),
	Term=..[Functor|Args].

parse_functor(String,Functor,String1):-
	parse_functor0(String,FunctorString,String1),
	name(Functor,FunctorString).

parse_functor0([C|String],Functor,String1):-
	parse_functor1(C,String,Functor,String1).
parse_functor0([],[],[]).

parse_functor1(C,String,[],[C|String]):-
	open_parent(C), !.
parse_functor1(C,String,[],[C|String]):-
	close_parent(C), !.
parse_functor1(C,String,[],[C|String]):-
	close_list(C), !.
parse_functor1(C,String,[],[C|String]):-
	comma(C), !.
parse_functor1(C,String,[],String1):-
	blank(C), !,
	trail_blanks(String,String1).
parse_functor1(C,String,[C|Functor],String1):-
	parse_functor0(String,Functor,String1).

%parse_args([],[],[]).
parse_args([C0|String0],Args,String):-
	open_parent(C0), !,
	parse_args0(String0,Args,[C1|String]),
	close_parent(C1).
parse_args(String,[],String).

parse_args0(String0,[Arg|Args],String):-
	parse_term(String0,Arg,String1),
	parse_args1(String1,Args,String).

parse_args1([C|String0],Args,String):-
	comma(C), !,
	parse_args0(String0,Args,String).
parse_args1(String,[],String).

parse_term([],'',[]).
parse_term([C|String0],Term,String):-
	parse_term0(C,String0,Term,String).
	
parse_term0(C0,String0,Term,String):-
	open_list(C0), !,
	parse_args0(String0,Term,[C1|String]),
	close_list(C1).
parse_term0(C0,String0,Term,String):-
	quote(C0), !,
	parse_string(String0,Str,String),
	parse_term(Str,Term,[]).
parse_term0(C0,String0,Term,String):-
	parse_struct([C0|String0],Term,String).

parse_string([],[],[]).
parse_string([C|String],List,String1):-
	parse_string0(C,String,List,String1).

parse_string0(C,String,[],String):-
	quote(C), !.
parse_string0(C,String,[C|List],String1):-
	parse_string(String,List,String1).

trail_blanks([],[]).
trail_blanks([C|String],String1):-
	trail_blanks0(C,String,String1).

trail_blanks0(C,String,String1):-
	blank(C), !,
	trail_blanks(String,String1).
trail_blanks0(C,String,[C|String]).

open_parent(C):-  "("=[C].
close_parent(C):- ")"=[C].
open_list(C):-    "["=[C].
close_list(C):-   "]"=[C].
comma(C):-        ","=[C].
blank(C):-        " "=[C].
quote(C):-   name('"',[C]).


%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

