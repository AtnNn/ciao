:- module(hrtime_c,[
	main/0,
	get_hrtimef/2,
	get_hrvtimef/2,
	get_hrutimef/2,
	get_hrstimef/2,
	get_current_hrtimef/1,

	get_hrtime_selff/1,
	get_hrvtime_selff/1,
	get_hrutime_selff/1,
	get_hrstime_selff/1,

	hrtime_initl/1,
	get_hrtime_structl/3,
	free_hrtime_structl/2],[assertions,foreign_interface]).

% the f means that the result is double
:- true pred get_hrtimef(in(Hr), go(Dest)) :: address * num + (foreign).
:- true pred get_hrvtimef(in(Hr), go(Dest)) :: address * num + (foreign).
:- true pred get_hrutimef(in(Hr), go(Dest)) :: address * num + (foreign).
:- true pred get_hrstimef(in(Hr), go(Dest)) :: address * num + (foreign).
:- true pred get_current_hrtimef(go(Dest)) :: num + (foreign).

:- true pred get_hrtime_selff(go(Dest)) :: num + (foreign).
:- true pred get_hrvtime_selff(go(Dest)) :: num + (foreign).
:- true pred get_hrutime_selff(go(Dest)) :: num + (foreign).
:- true pred get_hrstime_selff(go(Dest)) :: num + (foreign).

% the l means that the result is long integer
:- true pred hrtime_initl(go(Error)) :: int + (foreign,returns(Error)).
:- true pred get_hrtime_structl(in(Pid), go(Dest), go(Error)) :: int * address * int + (foreign,returns(Error)).
:- true pred free_hrtime_structl(in(Hr), go(Error)) :: address * int + (foreign, returns(Error)).

:- use_foreign_library([hrtime]).
:- use_foreign_source([hrtime_c]).

:- impl_defined([
	get_hrtimef/2,
	get_hrvtimef/2,
	get_hrutimef/2,
	get_hrstimef/2,
	get_current_hrtimef/1,

	get_hrtime_selff/1,
	get_hrvtime_selff/1,
	get_hrutime_selff/1,
	get_hrstime_selff/1,

	hrtime_initl/1,
	get_hrtime_structl/3,
	free_hrtime_structl/2]).

main :-
	hrtime_initl(_E1),
	get_hrtime_structl(0,Ts,_E2),
	display(Ts),nl,
	get_hrstimef(Ts,A),
	display(A),nl,
	get_hrstimef(Ts,B),
	display(B),nl,
	free_hrtime_structl(Ts,_E3).

