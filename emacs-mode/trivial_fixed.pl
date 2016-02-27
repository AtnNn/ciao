:- module(_,[lst/1,app/3],[assertions]).

:- entry lst/1 : list.

lst([]).
lst([_|X]) :- lst(X).

:- pred app/3 => list * list * num.

app([],X,X) :- lst(X).
app([X|Y],Z,[X|W]) :- app(Y,Z,W).


