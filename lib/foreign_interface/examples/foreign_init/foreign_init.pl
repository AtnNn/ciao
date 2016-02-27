:- module(foreign_init,
	[print_time/0,
	 init_p/0
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 foreign_interface
	]).

:- true pred init :: true + foreign(init).
:- true pred print_time :: true + foreign(print_time).

:- use_foreign_source(foreign).

init_p :- init.

:- impl_defined([init/0, print_time/0]).

:- initialization(init_p).






