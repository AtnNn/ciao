zebra(Englishman, Spaniard, Japanese, Ukrainian, Norwegian, Zebra, Water) :-
	Englishman = Red,
	Spaniard = Dog,
	Green = Coffee,
	Ukrainian = Tea,
	to_the_right(Green, Ivory),
	Winston = Snails,
	Kool = Yellow,
	Milk = third,
	Norwegian = first,
	next_to(Fox,   Chesterfield),
	next_to(Horse, Kool),
	Lucky = Juice,
	Japanese = Parliament,
	next_to(Norwegian, Blue),
	houses([Blue,       Green,      Red,      Yellow,       Ivory]),
	houses([Norwegian,  Englishman, Spaniard, Japanese,     Ukrainian]),
	houses([Dog,        Zebra,      Fox,      Snails,       Horse]),
	houses([Parliament, Kool,       Lucky,    Chesterfield, Winston]),
	houses([Milk,       Juice,      Water,    Tea,          Coffee]).

houses(Prop) :- domain(Prop, [first, second, third, fourth, fifth]).

domain([],       _).
domain([X|Rest], Domain) :-
	select(X, Domain, NewDomain),
	domain(Rest, NewDomain).

select(X, [X|R], R).
select(X, [Y|R], [Y|Rest]) :-
	select(X, R, Rest).

next_to(fifth,  fourth).
next_to(fourth, fifth).
next_to(fourth, third).
next_to(third,  fourth).
next_to(third,  second).
next_to(second, third).
next_to(second, first).
next_to(first,  second).

to_the_right(fifth,  fourth).
to_the_right(fourth, third).
to_the_right(third,  second).
to_the_right(second, first).
