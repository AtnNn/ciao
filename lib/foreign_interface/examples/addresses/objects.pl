:- module(objects,
	[object/2,
	 show_object/1,
	 null2/1
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 foreign_interface
	]).

null2(X) :- null(X).

:- true pred object(in(N),go(Object)) ::
	int * address + (foreign,returns(Object)).

:- true pred show_object(in(Object)) ::
	address + foreign.

:- use_foreign_source(objects_c).
:- extra_compiler_opts('-O2').

:- impl_defined([object/2,show_object/1]).








