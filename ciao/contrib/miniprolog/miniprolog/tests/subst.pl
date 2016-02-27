replacement('='(_1398, _1415)) :-
	arithexpression(_1398),
	arithexpression(_1415).
substitute('+'(_1680, _1701), _1718, '+'(_1741, _1768)) :-
	!,
	substitute(_1680, _1718, _1741),
	substitute(_1701, _1718, _1768).
substitute('*'(_2124, _2145), _2162, '*'(_2185, _2212)) :-
	!,
	substitute(_2124, _2162, _2185),
	substitute(_2145, _2162, _2212).
substitute('-'(_2568, _2589), _2606, '-'(_2629, _2656)) :-
	!,
	substitute(_2568, _2606, _2629),
	substitute(_2589, _2606, _2656).
substitute('='(_3012, _3033), _3050, '='(_3073, _3100)) :-
	!,
	substitute(_3012, _3050, _3073),
	substitute(_3033, _3050, _3100).
substitute('**'(_3474, _3497), _3516, '**'(_3541, _3497)) :-
	!,
	substitute(_3474, _3516, _3541).
substitute(_3835, _3852, _3875) :-
	find_replacement(_3835, _3852, _3875),
	!.
substitute(_4113, _4130, _4113).
find_replacement(_4257, ['='(_4257, _4290)|_4307], _4290).
find_replacement(_4457, [_4476|_4485],             _4506) :-
	find_replacement(_4457, _4485, _4506).

arithexpression(_A).
