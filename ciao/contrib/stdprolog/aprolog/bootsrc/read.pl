%   File   : READ.PL
%   Author : D.H.D.Warren + Richard O'Keefe
%   Updated: 5 July 1984
%   Purpose: Read Prolog terms in Dec-10 syntax.

:- module(read, []).
:- use_module(lists, [append/3, length/2]).
:- use_module(database, [retractall/1]).

/*
    Modified by Alan Mycroft to regularise the functor modes.
    This is both easier to understand (there are no more '?'s),
    and also fixes bugs concerning the curious interaction of cut with
    the state of parameter instantiation.

    Since this file doesn't provide "metaread", it is considerably
    simplified.  The token list format has been changed somewhat, see
    the comments in the RDTOK file.
*/

%   read_with_vars(?Answer, ?Variables)
%   reads a term from the current input stream and unifies it with
%   Answer.  Variables is bound to a list of [Atom=Variable] pairs.

read_with_vars(Answer, Variables) :-
	'rdtok:read_tokens'(Tokens, Variables),
	(   read(Tokens, 1200, Term, LeftOver), all_read(LeftOver)
        ;   syntax(Tokens)
	),
	!,
	Answer = Term.


%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.

all_read([]) :- !.
all_read(S) :-
	syntax([operator,expected,after,expression], S).


%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.

expect(Token, [Token|Rest], Rest) :- !.
expect(Token, S0, _) :-
	syntax([Token,or,operator,expected], S0).


%   I want to experiment with having the operator information held as
%   ordinary Prolog facts.  For the moment the following predicates
%   remain as interfaces to current_op.
%   prefixop(O -> Self, Rarg)
%   postfixop(O -> Larg, Self)
%   infixop(O -> Larg, Self, Rarg)


prefixop(Op, Prec, Prec) :-
	current_op(Prec, fy, Op), !.
prefixop(Op, Prec, Less) :-
	current_op(Prec, fx, Op), !,
	Less is Prec-1.


postfixop(Op, Prec, Prec) :-
	current_op(Prec, yf, Op), !.
postfixop(Op, Less, Prec) :-
	current_op(Prec, xf, Op), !, Less is Prec-1.


infixop(Op, Less, Prec, Less) :-
	current_op(Prec, xfx, Op), !, Less is Prec-1.
infixop(Op, Less, Prec, Prec) :-
	current_op(Prec, xfy, Op), !, Less is Prec-1.
infixop(Op, Prec, Prec, Less) :-
	current_op(Prec, yfx, Op), !, Less is Prec-1.


ambigop(F, L1, O1, R1, L2, O2) :-
	postfixop(F, L2, O2),
	infixop(F, L1, O1, R1), !.


%   read(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

read([Token|RestTokens], Precedence, Term, LeftOver) :-
	read(Token, RestTokens, Precedence, Term, LeftOver).
read([], _, _, _) :-
	syntax([expression,expected], []).


%   read(+Token, +RestTokens, +Precedence, -Term, -LeftOver)

read(var(Variable,_), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Variable, Precedence, Answer, S).

read(atom(-), [number(Number)|S1], Precedence, Answer, S) :- !, 
	check_negative(Number, Negative, S1),
	exprtl0(S1, Negative, Precedence, Answer, S).

read(atom(Functor), ['('|S1], Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_args(S2, RestArgs, S3),
	Term =.. [Functor,Arg1|RestArgs], !,
	exprtl0(S3, Term, Precedence, Answer, S).

read(atom(Functor), S0, Precedence, Answer, S) :-
	prefixop(Functor, Prec, Right), % !,  SzP
	after_prefix_op(Functor, Prec, Right, S0, Precedence, 
	    Answer, S), !.

read(atom(Atom), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Atom, Precedence, Answer, S).

read(number(Number), S0, Precedence, Answer, S) :- !,
	check_positive(Number, S0),
	exprtl0(S0, Number, Precedence, Answer, S).

read('[', [']'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, [], Precedence, Answer, S).

read('[', S1, Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_list(S2, RestArgs, S3), !,
	exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).

read('(', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).

read(' (', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).

read('{', ['}'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, '{}', Precedence, Answer, S).

read('{', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect('}', S2, S3), !,
	exprtl0(S3, '{}'(Term), Precedence, Answer, S).

read(string(List), S0, Precedence, Answer, S) :- !,
	current_prolog_flag(double_quotes, Type),
	(   Type = codes -> String = List
	;   Type = chars -> atom_codes(Atom, List), atom_chars(Atom, String)
	;   atom_codes(String, List)
	),
	exprtl0(S0, String, Precedence, Answer, S).

read(error(Err, Atom), S0, _, _, _) :- !,
	syntax([Err,in,Atom], S0).

read(Token, S0, _, _, _) :-
	syntax([Token,cannot,start,an,expression], S0).


%   read_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

read_args([','|S1], [Term|Rest], S) :- !,
	read(S1, 999, Term, S2), !,
	read_args(S2, Rest, S).
read_args([')'|S], [], S) :- !.
read_args(S, _, _) :-
	syntax([', or )',expected,in,arguments], S).


%   read_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list 
%   of terms.

read_list([','|S1], [Term|Rest], S) :- !,
	read(S1, 999, Term, S2), !,
	read_list(S2, Rest, S).
read_list(['|'|S1], Rest, S) :- !,
	read(S1, 999, Rest, S2), !,
	expect(']', S2, S).
read_list([']'|S], [], S) :- !.
read_list(S, _, _) :-
	syntax([', | or ]',expected,in,list], S).


%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, 
%       -Ans, -LeftOver)

after_prefix_op(Op, Oprec, _Aprec, S0, Precedence, _, _) :-
	Precedence < Oprec, !,
	syntax([prefix,operator,Op,in,context,with,precedence,Precedence], S0).

after_prefix_op(Op, Oprec, _Aprec, S0, Precedence, Answer, S) :-
	peepop(S0, S1),
	prefix_is_atom(S1, Oprec), % can't cut but would like to
	exprtl(S1, Oprec, Op, Precedence, Answer, S).

after_prefix_op(Op, Oprec, Aprec, S1, Precedence, Answer, S) :-
	read(S1, Aprec, Arg, S2),
	Term =.. [Op,Arg], !,
	exprtl(S2, Oprec, Term, Precedence, Answer, S).


%   The next clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority.

peepop([atom(F),'('|S1], [atom(F),'('|S1]) :- !.
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R). %%%% pts %%%% Dat: old conv.pl/modularize/2 converted 'infixop' to 'read:infixop' here; Imp: test case
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P). %%%% pts %%%% Dat: old conv.pl/modularize/2 converted 'postfixop' to 'read:postfixop' here; Imp: test case
peepop(S0, S0).


%   prefix_is_atom(+TokenList, +Precedence)
%   is true when the right context TokenList of a prefix operator
%   of result precedence Precedence forces it to be treated as an
%   atom, e.g. (- = X), p(-), [+], and so on.

prefix_is_atom([Token|_], Precedence) :-
	prefix_is_atom(Token, Precedence).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P. %%%% pts %%%% Dat: old conv.pl/modularize/2 converted 'infixop' to 'read:infixop' here; Imp: test case
prefix_is_atom(postfixop(_,L,_), P) :- L >= P. %%%% pts %%%% Dat: old conv.pl/modularize/2 converted 'postfixop' to 'read:postfixop' here; Imp: test case
prefix_is_atom(')', _).
prefix_is_atom(']', _).
prefix_is_atom('}', _).
prefix_is_atom('|', P) :- 1100 >= P.
prefix_is_atom(',', P) :- 1000 >= P.
prefix_is_atom([],  _).


%   exprtl0(+Tokens, +Term, +Prec, -Answer, -LeftOver)
%   is called by read/4 after it has read a primary (the Term).
%   It checks for following postfix or infix operators.

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	ambigop(F, L1, O1, R1, L2, O2), !,
	(   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, %%%% pts %%%% Dat: old conv.pl/modularize/2 converted 'infixop' to 'read:infixop' here; Imp: test case
		Answer, S)
	;   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, %%%% pts %%%% Dat: old conv.pl/modularize/2 converted 'postfixop' to 'read:postfixop' here; Imp: test case
		Answer, S)
	).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	infixop(F, L1, O1, R1), !,
	exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, %%%% pts %%%% Dat: old conv.pl/modularize/2 converted 'infixop' to 'read:infixop' here; Imp: test case
	    Answer, S).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	postfixop(F, L2, O2), !,
	exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, %%%% pts %%%% Dat: old conv.pl/modularize/2 converted 'postfixop' to 'read:postfixop' here; Imp: test case
	    Answer, S).

exprtl0([','|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1000, !,
	read(S1, 1000, Next, S2), !,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl0(['|'|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1100, !,
	read(S1, 1100, Next, S2), !,
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).

exprtl0([Thing|S1], _, _, _, _) :-
	cant_follow_expr(Thing, Culprit), !,
	syntax([Culprit,follows,expression], [Thing|S1]).

exprtl0(S, Term, _, Term, S).


cant_follow_expr(atom(_),	atom).
cant_follow_expr(var(_,_),	variable).
cant_follow_expr(number(_),	number).
cant_follow_expr(string(_),	string).
cant_follow_expr(' (',		bracket).
cant_follow_expr('(',		bracket).
cant_follow_expr('[',		bracket).
cant_follow_expr('{',		bracket).



exprtl([infixop(F,L,O,R)|S1], C, Term, Precedence, Answer, S) :- %%%% pts %%%% Dat: old conv.pl/modularize/2 converted 'infixop' to 'read:infixop' here; Imp: test case
	Precedence >= O, C =< L, !,
	read(S1, R, Other, S2),
	Expr =.. [F,Term,Other], /*!,*/
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl([postfixop(F,L,O)|S1], C, Term, Precedence, Answer, S) :- %%%% pts %%%% Dat: old conv.pl/modularize/2 converted 'postfixop' to 'read:postfixop' here; Imp: test case
	Precedence >= O, C =< L, !,
	Expr =.. [F,Term],
	peepop(S1, S2),
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl([','|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1000, C < 1000, !,
	read(S1, 1000, Next, S2), /*!,*/
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl(['|'|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1100, C < 1100, !,
	read(S1, 1100, Next, S2), /*!,*/
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).

exprtl(S, _, Term, _, Term, S).

% This is needed, so that MIN_INTEGER is handled correctly. 
%
check_negative(Number, Negative, _) :- float(Number), !, Negative is -Number.
check_negative(Number, Negative, _) :- Number >= 0, !, Negative is -Number.
check_negative(Number, Number, _) :- min_integer(Number), !.
check_negative(Number, _, Tokens) :-
	syntax([number,too,big], [number(Number)|Tokens]).

check_positive(Number, _) :- float(Number), !.
check_positive(Number, _) :- Number >= 0, !.
check_positive(Number, Tokens) :-
	syntax([number,too,big], [number(Number)|Tokens]).

min_integer(-1073741824).

%   This business of syntax errors is tricky.  When an error is 
%   detected, we have to write out a message.  We also have to note 
%   how far it was to the end of the input, and for this we are 
%   obliged to use the data-base.  Then we fail all the way back to 
%   read(), and that prints the input list with a marker where the 
%   error was noticed.  If subgoal_of were available in compiled code 
%   we could use that to find the input list without hacking the 
%   data base.  The really hairy thing is that the original code 
%   noted a possible error and backtracked on, so that what looked 
%   at first sight like an error sometimes turned out to be a wrong 
%   decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no
%   backtracking at all.  This goal has not yet been met, and it 
%   will still occasionally report an error message and then decide 
%   that it is happy with the input after all.  Sorry about that.


syntax(Message, List) :-
	length(List, Length),
	asserta('$syntax'(Message, Length)), !,
	fail.

syntax(List) :-
	retract('$syntax'(Message, AfterError)),
	retractall('$syntax'(_, _)),
	length(List, Length),
	BeforeError is Length-AfterError,
	length(LB, BeforeError),
	append(LB, LA, List),
	list_to_codes(Message, [_|ErrList], [0':|L1]),
	list_to_codes(LB, L1, L2),
	list_to_codes(['<<here>>'|LA], L2, []),
	atom_codes(ErrMsg, ErrList),
	throw(error(syntax_error(ErrMsg), read/1)).

list_to_codes([], L, L).
list_to_codes([Tok|Toks], L, LE) :-
	token_value(Tok, TokL),
	append([0' |TokL], L1, L),
	list_to_codes(Toks, L1, LE).

token_value(atom(X), LX) :- !, atom_codes(X, LX).
token_value(var(_V,X), LX) :- !, atom_codes(X, LX).
token_value(number(X), LX) :- !, number_codes(X, LX).
token_value(string(X), LX) :- !, LX = X.
token_value(error(_Err, Atom), LX) :- !, atom_codes(Atom, LX).
token_value(X, LX) :- atom_codes(X, LX).

