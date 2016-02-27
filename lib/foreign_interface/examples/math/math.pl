:- module(math,
	[sin/2,
	 cos/2,
	 fabs/2
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 foreign_interface
	 ]).

:- true pred sin(in(X),go(Y)) :: num * num + (foreign,returns(Y)).
:- true pred cos(in(X),go(Y)) :: num * num + (foreign,returns(Y)).
:- true pred fabs(in(X),go(Y)) :: num * num + (foreign,returns(Y)).

% :- native misc:function([X:double, Y:double], double)#[return(X+Y)].

:- extra_compiler_opts(['-O2']).
:- extra_compiler_opts('LINUXi86',['-ffast-math']).
:- use_foreign_library(m).

:- impl_defined([sin/2,cos/2,fabs/2]).




