:- module(byte_lists,
	[obtain_list/3,
	 show_list/2
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 'foreign_interface/syntax'
	]).
	 
:- true pred obtain_list(in(N),go(Length),go(List)) :: int * int * byte_list
	+ (foreign,size_of(List,Length)).
:- true pred show_list(in(Length),in(List)) :: int * byte_list
	+ (foreign,size_of(List,Length)).

:- use_foreign_source(bytes_op).

:- impl_defined([obtain_list/3,show_list/2]).














