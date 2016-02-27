:- module(example7,_,[fuzzy]).

:- aggr average.

tall(john,0.8):~ .
fast(john,0.7):~ .

good_player(X,_):~ average
	tall(X,_),
        fast(X,_).

%average(X,Y,Z):- Z .=.(X + Y)/2.



average_initial(ListInitial,ListInitial).

average(X,Y,Z) :- Z .=. (X + Y).

average_final(ListTemp,ValorTemp,ValorFinal) :- ValorFinal .=. ValorTemp / 2.
