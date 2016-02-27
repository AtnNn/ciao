/*  Adapted from shared code written by D.H.D.Warren + Richard O'Keefe; */
/*  all changes                                                         */
/*  Copyright (C) 1997 1998 UPM-CLIP. */

:- module('read.new',[read/1, read/2, read_term/2, read_term/3,
                read_top_level/3, second_prompt/2],[assertions,isomodes]).

:- use_module(library(tokenize)).
:- use_module(library(operators)).
:- use_module(library(lists), [list_insert/2, dlist/3, length/2, append/3]).
:- use_module(library(dict)).
:- use_module(engine(internals)).

:- comment(title,"Term input").  

:- set_prolog_flag(multi_arity_warnings, off).

:- comment(define_flag/3,"Defines flags as follows:
	@includedef{define_flag/3}
	(See @ref{Changing system behaviour and various flags}).

        If flag is @tt{on} (it is @tt{off} by default), a variable
        followed by a parenthesized lists of arguments is read as a
        @pred{call/N} term, except if the variable is anonymous, in
        which case it is read as an anonymous predicate abstraction
        head. For example, @tt{P(X)} is read as @tt{call(P,X)} and
        @tt{_(X,Y)} as @tt{''(X,Y)}.").

:- multifile define_flag/3.

define_flag(read_hiord, [on,off], off).

:- pred read(+Stream,?Term) + iso
# "The next term, delimited by a full-stop (i.e., a @tt{.} followed by
   either a space or a control character), is read from @var{Stream}
   and is unified with @var{Term}. The syntax of the term must agree
   with current operator declarations. If the end of @var{Stream} has
   been reached, @var{Term} is unified with the term @tt{end_of_file}.
   Further calls to @tt{read/2} for the same stream will then cause an
   error, unless the stream is connected to the terminal (in which case
   a prompt is opened on the terminal).".

read(Stream, X) :-
        current_input(CurIn),
        read_internal(X, Stream, CurIn, _, _, _, read/2).

:- comment(read(Term), "Like @tt{read(Stream,Term)} with @var{Stream}
        associated to the current input stream.").

:- pred read(?Term) + iso.

read(X) :-
        current_input(Stream),
        read_internal(X, Stream, Stream, _, _, _, read/1).

:- pred read_term(+Stream,?Term,+Options) + iso.

read_term(Stream, X, Options) :-
        read_term_aux(Options, Stream, 3, X).

:- pred read_term(?Term,+Options) + iso.

read_term(X, Options) :-
        current_input(Stream),
        read_term_aux(Options, Stream, 2, X).

read_term_aux(Options, Stream, N, X) :-
        option_list(Options, Vs, Ns, Ss, Lns, VarDict, N),
        current_input(CurIn),
        read_internal(X, Stream, CurIn, VarDict, Tokens, Lns, read_term/N),
        extract_vars(Vs, Tokens),
        extract_names(Ns, Ss, VarDict).

read_top_level(Stream, Data, Variables) :-
        current_input(CurIn),
	read_internal(Data, Stream, CurIn, Variables, _, _, read_top_level/3).

option_list(V, _, _, _, _, _, Arg) :- var(V), !,
        throw(error(instantiation_error,read_term/Arg-Arg)).
option_list([], _, _, _, _, _, _) :- !.
option_list([O|Os], Vs, Ns, Ss, Ls, Dict, Arg) :- !,
        option(O, Vs, Ns, Ss, Ls, Dict, Arg),
        option_list(Os, Vs, Ns, Ss, Ls, Dict, Arg).
option_list(Os, _, _, _, _, _, Arg) :-
        throw(error(type_error(list,Os),read_term/Arg-Arg)).

option(V, _, _, _, _, _, Arg) :- var(V), !,
        throw(error(instantiation_error,read_term/Arg-Arg)).
option(variables(Vs), variables(Vs), _, _, _, _, _) :- !.
option(variable_names(Ns), _, variable_names(Ns), _, _, _, _) :- !.
option(singletons(Ss), _, _, singletons(Ss), _, _, _) :- !.
option(lines(L0,L1), _, _, _, lines(L0,L1), _, _) :- !.
option(dictionary(Dict), _, _, _, _, Dict, _) :- !.
option(Op, _, _, _, _, _, Arg) :-
        throw(error(domain_error(read_option,Op),read_term/Arg-Arg)).

extract_vars(V, _) :- var(V), !. % No need of computing it
extract_vars(variables(Vs), Tokens) :-
        extract_vars2(Tokens, Vs1), Vs = Vs1.

extract_vars2([], Vs) :-
        list(Vs), !.
extract_vars2([var(V,_)|Tokens], Vs) :- !,
        list_insert(Vs, V),
        extract_vars2(Tokens, Vs).
extract_vars2([_|Tokens], Vs) :-
        extract_vars2(Tokens, Vs).
        
extract_names(Ns, Ss, _) :- var(Ns), var(Ss), !. % No need of computing it
extract_names(variable_names(Ns), singletons(Ss), VarDict) :-
        extract_names2(VarDict, Ns1, [], Ss1, []),
        Ns = Ns1, Ss = Ss1.

extract_names2(D, Ns, Ns, Ss, Ss) :- var(D), !.
extract_names2(dic(Str,[Var|Sing],L,R), Ns, Ns_, Ss, Ss_) :-
        extract_names2(L, Ns, Ns1, Ss, Ss1),
        name(Name,Str),
        Eq = (Name=Var),
        Ns1 = [Eq|Ns2],
        ( var(Sing) ->
              Ss1 = [Eq|Ss2]
        ; Ss1 = Ss2
        ),
        extract_names2(R, Ns2, Ns_, Ss2, Ss_).

read_internal(Answer, Stream, CurIn, Variables, Tokens, Lines, Predicate) :-
        catch(read_internal(Answer, Stream, CurIn, Variables, Tokens, Lines),
              error(Error_Term,_),
              (set_input(CurIn), throw(error(Error_Term,Predicate)))).

read_internal(Answer, Stream, CurIn, Variables, Tokens, lines(Ln0,Ln1)) :-
	set_input(Stream),
        line_count(Stream, L0), Ln0 is L0+1,
        read_tokens(Tokens, _Variables),
        line_position(Stream, Pos),
        line_count(Stream, L1), Ln1 is L1+sign(Pos),
        ( Tokens = [] -> Term = end_of_file
        ; clearerr(Stream), % Just in case we have reached eof
          read(Tokens, 1200, Variables, Term, LeftOver),
          all_read(LeftOver) ->
            the_syntax_error([], 0, _, _) % erase any leftovers
        ; syntax_error_data(Tokens, Msg, ErrorLoc),
          throw(error(syntax_error(Ln0,Ln1,Msg,ErrorLoc),'while reading'))
        ),
        set_input(CurIn),
	Answer = Term.


%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.

all_read([.]) :- !.
all_read(S) :-
	syntax_error(['operator expected after expression'], S).


%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.

expect(Token, [Token|Rest], Out) :- !, Out=Rest.
expect(Token, S0, _) :-
	syntax_error([Token,' or operator expected'], S0).


%   I want to experiment with having the operator information held as
%   ordinary Prolog facts.  For the moment the following predicates
%   remain as interfaces to current_op.
%   current_prefixop(O -> Self, Rarg)
%   current_postfixop(O -> Larg, Self)
%   current_infixop(O -> Larg, Self, Rarg)

%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, ?Variables, -Ans, -LeftOver)

after_prefix_op(Op, Oprec, Aprec, S1, Precedence, Variables, Answer, S) :-
	S1 = [Token|_],
	\+ cant_start_expr(Token),
	(   Precedence < Oprec ->
	    syntax_error(['prefix operator ',Op,' in context with precedence ',
	                  Precedence],
		         S1)
	;   read(S1, Aprec, Variables, Arg, S2),
	    functor(Term, Op, 1),
	    arg(1, Term, Arg),
	    read_rest(S2, Oprec, Term, Precedence, Variables, Answer, S)
	).


cant_start_expr(')').
cant_start_expr(']').
cant_start_expr('}').
cant_start_expr('|').
cant_start_expr(',').
cant_start_expr(.).


%   read(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

read([Token|RestTokens], Precedence, Variables, Term, LeftOver) :-
	read(Token, RestTokens, Precedence, Variables, Term, LeftOver).
read([], _, _, _, _) :-
	syntax_error(['expression expected'], []).


%   read(+Token, +RestTokens, +Precedence, ?Variables, -Term, -LeftOver)

read(X, _, _, _, _, _) :- var(X), !, fail.		% space saver
read(var(_,Name), ['('|S1], Precedence, Variables, Answer, S) :-
        current_prolog_flag(read_hiord, on),
        !,
        add_var(Variables,Name,Variable),
	read(S1, 999, Variables, Arg1, S2),
	read_args(S2, Variables, RestArgs, S3), !,
        ( Name = "_" ->
          Term =.. ['',Arg1|RestArgs]
        ; Term =.. [call,Variable,Arg1|RestArgs]
        ),
	read_rest(S3, 0, Term, Precedence, Variables, Answer, S).
read(var(_,Name), S0, Precedence, Variables, Answer, S) :- !,
        add_var(Variables,Name,Variable),
	read_rest(S0, 0, Variable, Precedence, Variables, Answer, S).
read(atom(-), [number(Number)|S1], Precedence, Variables, Answer, S) :-
	number(Number), !,
	Negative is -Number,
	read_rest(S1, 0, Negative, Precedence, Variables, Answer, S).
read(atom(Functor), ['('|S1], Precedence, Variables, Answer, S) :- !,
	read(S1, 999, Variables, Arg1, S2),
	read_args(S2, Variables, RestArgs, S3), !,
        ( Term =.. [Functor,Arg1|RestArgs] ->
            read_rest(S3, 0, Term, Precedence, Variables, Answer, S)
        ; syntax_error(['maximum arity exceeded'],S3)
        ).
read(atom(Functor), S0, Precedence, Variables, Answer, S) :-
	current_prefixop(Functor, Prec, Right),
	after_prefix_op(Functor, Prec, Right, S0, Precedence, Variables,
                        Answer, S).
read(atom(Atom), S0, Precedence, Variables, Answer, S) :- !,
	read_rest(S0, 0, Atom, Precedence, Variables, Answer, S).
read(number(Number), S0, Precedence, Variables, Answer, S) :-
	number(Number), !,
	read_rest(S0, 0, Number, Precedence, Variables, Answer, S).
read('[', [']'|S1], Precedence, Variables, Answer, S) :- !,
	read(atom([]), S1, Precedence, Variables, Answer, S).
read('[', S1, Precedence, Variables, Answer, S) :- !,
	read(S1, 999, Variables, Arg1, S2),
	read_list(S2, Variables, RestArgs, S3), !,
	read_rest(S3, 0, [Arg1|RestArgs], Precedence, Variables, Answer, S).
read('(', S1, Precedence, Variables, Answer, S) :- !,
	read(S1, 1200, Variables, Term, S2),
	expect(')', S2, S3), !,
	read_rest(S3, 0, Term, Precedence, Variables, Answer, S).
read(' (', S1, Precedence, Variables, Answer, S) :- !,
	read(S1, 1200, Variables, Term, S2),
	expect(')', S2, S3), !,
	read_rest(S3, 0, Term, Precedence, Variables, Answer, S).
read('{', ['}'|S1], Precedence, Variables, Answer, S) :- !,
	read(atom({}), S1, Precedence, Variables, Answer, S).
read('{', S1, Precedence, Variables, Answer, S) :- !,
	read(S1, 1200, Variables2, Term, S2),
	expect('}', S2, S3), !,
	read_rest(S3, 0, NewTerm, Precedence, Variables, Answer, S),
        handle_predabs(Term, Variables2, Variables, NewTerm),
        dic_insert(Variables2, Variables).
read(string(List), ['|','|'|S0], Precedence, Variables, Answer, S) :- !,
        dlist(List,Head,Tail),
        read(S0, 0, Variables, Tail, S1),
	read_rest(S1, 0, Head, Precedence, Variables, Answer, S).
read(string(List), S0, Precedence, Variables, Answer, S) :- !,
	read_rest(S0, 0, List, Precedence, Variables, Answer, S).
read(badatom(_), S0, _, _, _, _) :- !,
        syntax_error(['atom too large'], S0).
read(Token, S0, _, _, _, _) :-
	syntax_error([Token,' cannot start an expression'], S0).

handle_predabs((H :- B), PAVars, RestVars, 'PA'(ShVs,H,B)) :- !.
handle_predabs(Term, _, _, {Term}).


%   read_rest(+Tokens, +LPrec, +Term, +Prec, ?Variables, -Answer, -LeftOver)
%   is called by read/4 after it has read a primary (the Term).  It
%   checks for following postfix or infix operators.  The tokens
%   Tokens-LeftOver are parsed and combined with Term into Answer;
%   LPrec and Prec are the lower and upper bounds for the precedence
%   of the topmost operator.


read_rest([atom(F)|S1], LPrec, Term, Precedence, Variables, Answer, S) :-
	current_infixop(F, L, O, R),
	Precedence >= O, LPrec =< L,
	(   current_postfixop(F, L1, O1),
	    Precedence >= O1, LPrec =< L1 -> true
	;   !
	),
	read(S1, R, Variables, Other, S2),
	functor(Expr, F, 2),
	arg(1, Expr, Term),
	arg(2, Expr, Other),
	read_rest(S2, O, Expr, Precedence, Variables, Answer, S).
read_rest([atom(F)|S1], LPrec, Term, Precedence, Variables, Answer, S) :-
	current_postfixop(F, L, O),
	Precedence >= O, LPrec =< L, !,
	functor(Expr, F, 1),
	arg(1, Expr, Term),
	read_rest(S1, O, Expr, Precedence, Variables, Answer, S).
read_rest([','|S1], LPrec, Term, Precedence, Variables, Answer, S) :-
	Precedence >= 1000, LPrec < 1000, !,
	read(S1, 1000, Variables, Next, S2),
	read_rest(S2, 1000, (Term,Next), Precedence, Variables, Answer, S).
read_rest(['|'|S1], LPrec, Term, Precedence, Variables, Answer, S) :-
	Precedence >= 1100, LPrec < 1100, !,
	read(S1, 1100, Variables, Next, S2),
	read_rest(S2, 1100, (Term;Next), Precedence, Variables, Answer, S).
read_rest(S, _, Term, _, _, Term, S).


%   read_args(+Tokens, ?Variables, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

read_args([','|S1], Variables, TermList, S) :- !,
	TermList = [Term|Rest],
	read(S1, 999, Variables, Term, S2),
	read_args(S2, Variables, Rest, S).
read_args([')'|S1],_Variables, TermList, S) :- !,
	TermList = [],
	S = S1.
read_args(S, _, _, _) :-
	syntax_error([', or ) expected in arguments'], S).


%   read_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list of terms.

read_list([','|S1], Variables, TermList, S) :- !,
	TermList = [Term|Rest],
	read(S1, 999, Variables, Term, S2),
	read_list(S2, Variables, Rest, S).
read_list(['|'|S1], Variables, Rest, S) :- !,
	read(S1, 999, Variables, Rest, S2),
	expect(']', S2, S).
read_list([']'|S1],_Variables, TermList, S) :- !,
	TermList = [],
	S = S1.
read_list(S, _, _, _) :-
	syntax_error([', | or ] expected in list'], S).

add_var(Dict, Name, Var) :-
        dic_lookup(Dict, Name, Node),
        ( var(Node) -> Node = [Var|_]
        ; Node = [Var|[]] % The [] marks it is not singleton
        ).

:- data 'syntax error'/2.

:- on_abort(retractall_fact('syntax error'(_,_))).

syntax_error(Message, List) :-
	length(List, Length),
	asserta_fact('syntax error'(Message,Length)), !,
	fail.

syntax_error_data(Tokens, Msg, ErrorLoc) :-
	the_syntax_error([], 1000000, Msg0, AfterError),
        tokens_items(Msg0, Msg),
	length(Tokens, Length),
	BeforeError is Length-AfterError,
        error_localization(Tokens, BeforeError, ErrorLoc).

the_syntax_error(Msg0, AfterError0, Msg, AfterError) :-
	current_fact('syntax error'(Msg1,AfterError1), Ptr), !,
	erase(Ptr),
	( AfterError0 > AfterError1 ->
	    the_syntax_error(Msg1, AfterError1, Msg, AfterError)
	;
	    the_syntax_error(Msg0, AfterError0, Msg, AfterError)
	).
the_syntax_error(Msg, AfterError, Msg, AfterError).

error_localization(L, 0, Msg) :- !,
        Msg = ['\n','** here **','\n'|Msg_],
        error_localization(L, 999999, Msg_).
error_localization([T|Ts], BeforeError, [I,' '|Is]) :-
	token_item(T, I),
	Left is BeforeError-1,
	error_localization(Ts, Left, Is).
error_localization([], _, []).

tokens_items([], []).
tokens_items([T|Ts], [I|Is]) :-
        token_item(T, I),
        tokens_items(Ts, Is).

token_item(atom(X),    X    ) :- !.
token_item(number(X),  X    ) :- !.
token_item(var(_,X),   $$(X)) :- !.
token_item(badatom(X), $$(X)) :- !.
token_item(string(X),  $$(S)) :- !, append([0'"|X], """", S). % "
token_item(X,          X).

% --

:- data second_prompt/1.

second_prompt('   ').

second_prompt(Old, New) :-
        (atom(New) ->
             retract_fact(second_prompt(Old)),
             asserta_fact(second_prompt(New))
        ; New = Old,
          current_fact(second_prompt(Old))
        ), !.

:- comment(version_maintenance,dir('../version')).

:- comment(version(1*7+43,2001/01/15,17:34*58+'CET'), "Changes to not
   require a layout char ending in prolog files.  (Daniel Cabeza
   Gras)").

:- comment(version(1*7+36,2000/12/31,18:35*26+'CET'), "Added read_hiord
   flag to disable higher order special syntax.  (Daniel Cabeza Gras)").

:- comment(version(1*5+100,2000/03/30,18:01*28+'CEST'), "Added option
   dictionary(Dict) to read_term to get the variables dictionary (as
   returned by read_top_level/3). (Daniel Cabeza Gras)").

:- comment(version(1*5+74,2000/03/20,16:49*45+'CET'), "Syntax _(Args) is
   now read as ''(Args).  (Daniel Cabeza Gras)").

:- comment(version(0*8+10,1998/11/24,17:52*05+'MET'), "Fixed bug when
   the last term of a file had syntax error (EOF was lost).  (Daniel
   Cabeza Gras)").

:- comment(version(0*8+4,1998/11/11,18:14*26+'MET'), "Changed read
   predicates to throw an error instead of outputing a message and
   repeating the read.  Added a read_option lines(Ln0, Ln1) to read_term/2-3
   (Daniel Cabeza Gras)").

:- comment(version(0*7+12,1998/09/25,16:12*18+'MEST'), "Changed the
   translation of X(Args) to put Args in a structure instead of in a
   list (Daniel Cabeza Gras)").

:- comment(version(0*5+14,1998/06/05,17:10*30+'MET DST'), "Changed
   syntax error messages (Daniel Cabeza Gras)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").



