:- module(numlists,
	[ get_primes/2,
	  intlist/1,
	  numlist/1,
	  sum_list/2,
	  sum_list/3, 
	  sum_list_of_lists/2,
	  sum_list_of_lists/3
	],
	[ assertions, regtypes
	]).

:- use_module(library(lists),[ reverse/2 ]).

:- comment(title,"Lists of numbers").

:- comment(author,"The CLIP Group").

:- comment(module,"This module implements some kinds of lists of
   numbers.").

:- regtype numlist(X)    # "@var{X} is a list of numbers.".

numlist([]).
numlist([N|Ns]):-
	num(N),
	numlist(Ns).

:- regtype intlist(X)    # "@var{X} is a list of integers.".

intlist([]).
intlist([N|Ns]):-
	int(N),
	intlist(Ns).

:- pred sum_list(List,N) : numlist(List) => num(N)
   # "@var{N} is the total sum of the elements of @var{List}.".

sum_list(List,N):- sum_list(List,0,N).

:- pred sum_list(List,N0,N) : ( numlist(List), num(N0) ) => num(N)
   # "@var{N} is the total sum of the elements of @var{List}
      plus @var{N0}.".

sum_list([],N,N).
sum_list([X|Xs],N0,N):-
	N1 is N0 + X,
	sum_list(Xs,N1,N).

:- pred sum_list_of_lists(Lists,N) : list(List,numlist) => num(N)
   # "@var{N} is the total sum of the elements of the lists of @var{Lists}.".

sum_list_of_lists(Xss,N):- sum_list_of_lists(Xss,0,N).

:- pred sum_list_of_lists(Lists,N0,N)
	: ( list(List,numlist), num(N0) ) => num(N)
   # "@var{N} is the total sum of the elements of the lists of @var{Lists}
      plus @var{N0}.".

sum_list_of_lists([],N,N).
sum_list_of_lists([Xs|Xss],N0,N):-
	sum_list(Xs,N0,N1),
	sum_list_of_lists(Xss,N1,N).

:- push_prolog_flag(multi_arity_warnings,off).

:- pred get_primes(N,Primes) : int(N) => intlist(Primes)
   # "Computes the @var{N}th first prime numbers in ascending order.".

get_primes(N,Primes):-
	get_primes(0,N,2,[],Primes0),
	reverse(Primes0,Primes).

get_primes(N,N,_,Primes,Primes):- !.
get_primes(N0,N,Num,OldPrimes,Primes):-
	( prime(OldPrimes,Num) ->
	    N1 is N0 + 1,
	    NewPrimes = [Num|OldPrimes]
	;   N1 = N0,
	    NewPrimes = OldPrimes
	),
	Num1 is Num + 1,
	get_primes(N1,N,Num1,NewPrimes,Primes).

:- pop_prolog_flag(multi_arity_warnings).

prime([],_).
prime([X|Primes],Num):-
	Mod is Num mod X,
	Mod > 0,
	prime(Primes,Num).


:- comment(version(1*11+113,2003/12/22,18:22*36+'CET'), "Added comment
   author and module.  (Edison Mera)").

:- comment(version(0*9+99,1999/05/26,11:33*15+'MEST'), "Documented
   exported predicates.  (Francisco Bueno Carrillo)").

:- comment(version(0*7+21,1998/10/20,16:42*29+'MEST'), "Added type
   intlist.  (Francisco Bueno Carrillo)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:

