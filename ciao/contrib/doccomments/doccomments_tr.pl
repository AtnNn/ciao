:- module(doccomments_tr, [], [assertions]).

:- doc(title,"Comment-style syntax for machine-readable comments").
:- doc(author,"Jose F. Morales").
:- doc(author,"Manuel Hermenegildo").
:- doc(module,"

   This package allows using an alternative syntax for
   machine-readable comments. Essentially, with this package any
   comment of the form:

@begin{verbatim}
:- doc(@em{CommentType},@em{Body}).
@end{verbatim}

@noindent
can be written as:

@begin{verbatim}
@tt{\%!} @em{CommentType}@tt{:} @em{Body}
@end{verbatim}
 
@noindent
The first two characters (@tt{\%!}) must be in the first two
columns. @em{Body} can expand over several lines but each 
must have a @tt{\%} in the first column. For example, the 
following:

@begin{verbatim}
\%! title:  A nice module
\% 
\%! author: Pro Grammer
\% 
\%! module: This is a very nice module indeed. 
\%          It can be used for several purposes.
\%
\%! hide:   internal/3
@end{verbatim}

@noindent
is equivalent to:

@begin{verbatim}
:- doc(title, ""A nice module"").
:- doc(author,""Pro Grammer"").
:- doc(module,""This is a very nice module indeed. 
                    It can be used for several purposes."").
:- doc(hide,internal/3).
@end{verbatim}

This is still a relatively rough version for experimentation.").

:- doc(hide,doccomments_sentence/3).
:- export(doccomments_sentence/3).

:- use_module(library(atom2term),[parse_term/4]).

% For context. Not needed for now.
% :- data '$lastSentence'/2.

% doccomments_sentence(0, _, Mod) :- !, % no need for initialization
doccomments_sentence('\6\doccomment'(Chars, _Sentence), R, _Mod) :- !,
	translate_comments('\6\doccomment'(Chars, _Sentence), R, _Mod).
% doccomments_sentence(end_of_file, _, Mod) :- !,
% 	retractall_fact('$lastSentence'(Mod,_)).

translate_comments('\6\doccomment'(Chars, _Sentence), R, _Mod) :- !,
	split_at_bangs(Chars, Comments),
	process_comments(Comments,R,RE),
	translate_comments(_Sentence, RE, _Mod).
translate_comments(R, R, _Mod).

:- pred split_at_bangs(A, B) :: string * string 
# "Split string @var{A} at the places where the character @term{!}
   appears as the first character in the line.".

split_at_bangs(Xs, Ys) :-
	split_at_bangs_(Xs, Chunk, Chunk, Ys).

split_at_bangs_([], Chunk, Chunk0, Ys) :- !,
	split_at_bangs__accum([], Chunk, Chunk0, Ys).
split_at_bangs_([0'\n, 0'!|Xs], Chunk, Chunk1, Ys) :- !,
	Chunk1 = [0'\n|Chunk0],
	split_at_bangs__accum(Xs, Chunk, Chunk0, Ys).
split_at_bangs_([X|Xs], Chunk, [X|Chunk0], Ys) :- !,
	split_at_bangs_(Xs, Chunk, Chunk0, Ys).

split_at_bangs__accum(Xs, Chunk, Chunk0, Ys) :-
	Chunk0 = [], % close the list
	Ys = [Chunk|Ys0], % accumulate the chunk
	( Xs = [] ->
	    Ys0 = [] % nothing more
	; split_at_bangs(Xs, Ys0)
	).


:- pred process_comments(Comments, DComm, DCommTail) 
:: list(string) * list(doccomment) * list 
# "Processes each doc comment in the list @var{Comments}.".

process_comments([],End,End).
process_comments([H|Xs],[(:- doc(NHead,NBody))|Ys],End) :- 
	split_comment(H,Head,Body),
	process_comment(Head,Body,NHead,NBody),
	process_comments(Xs,Ys,End).

:- prop doccomment/1 + regtype.
doccomment((:- doc(_, _))).

:- pred split_comment(Comment, Head, Body ) 
:: string * string * string
# "Splits a doc @var{Comment} into its @var{Head} (the part before ':') and 
   @var{Body} (after ':') parts.". 

split_comment([],[],[]).
split_comment([0': | Body],[],Body) :- !.
split_comment([C|Cs],[C|Head],Body) :- 
	split_comment(Cs,Head,Body).
	
:- pred process_comment(Head, Body, NewHead, NewBody)
:: string * string * term * term 
# "Parse and transform the head and body of each comment.". 

process_comment(Head,[],Head,'contextual') :- !.
process_comment(Head,Body,THead,NBody) :- 
	parse_term(Head,THead,_,vars),
%%  	push_prolog_flag(write_strings,on),
%% 	( atom(THead) 
%% 	-> atom_codes(THead,CHead),
%% 	   to_lower_case(CHead,LHead),
%% 	   atom_codes(AHead,LHead)
%% 	;  AHead = THead ),
	process_body(THead,Body,NBody),
%% 	write('Head before: '), write(Head), nl,
%%  	write(':- doc('), write(THead), write(','),
%%     	                      write(NBody), write(').'), nl,
%%  	pop_prolog_flag(write_strings),
	true.

process_body(version_maintenance,Body,NBody) :- 
	!,
	% version_maintenance_type/1: on off dir(atom)
	parse_term(Body,NBody,_,novars).
process_body(doinclude,Body,NBody) :- 
	!,
	% Name/Arity or list(Name/Arity)
	parse_term(Body,NBody,_,novars).
process_body(hide,Body,NBody) :- 
	!,
	% Name/Arity or list(Name/Arity)
	parse_term(Body,NBody,_,novars).
process_body(filetype,Body,NBody) :- 
	!,
	% filetype/1: module|user|include|package|part
	parse_term(Body,NBody,_,novars).
process_body(nodoc,Body,NBody) :- 
	!,
	% nodoc atom 
	parse_term(Body,NBody,_,novars).
process_body(_Head,Body,Body).

%% to_lower_case([],[]).
%% to_lower_case([H|T],[NH|NT]) :-
%% 	is_upper_case(H),
%% 	!,
%% 	NH is H+32,
%% 	to_lower_case(T,NT).
%% to_lower_case([H|T],[H|NT]) :-
%% 	to_lower_case(T,NT).
%% 
%% % Just plain ascii for now
%% is_upper_case(Ch) :-
%% 	Ch >= 65, Ch =< 90.
