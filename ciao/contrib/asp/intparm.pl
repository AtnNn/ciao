:- module(intparm, [buildParm/2]).

:- use_module(library(asp(defparm))).

buildParm(S,Parm) :-

	buildDefParm(S,Parm),

	write(S,'% change_param handler:'), nl(S),
	write(S,'change_parm(L) :- '), nl(S),
	write(S,'       var(L), !, parm(L).'), nl(S),
	write(S,'change_parm(L) :-!,'), nl(S),
	write(S,'      retractall(lparse_set(_,_)),\n'),
	write(S,'      parse_arg(L, C1,C2),'), nl(S),
	write(S,'      assertz(lparse_set(C1,C2)),\n'),
	write(S,'	(parm(OldParm)->true;OldParm=[]),\n'),
	write(S,'       this_module(TT),'), nl(S),
	write(S,'	module_concat(TT, bkparm(OldParm), M), '), nl(S),
	write(S,'        und(M),'), nl(S),
	write(S,'      retractall(parm(_)),'), nl(S),
	write(S,'      assertz(parm(L)),'), nl(S),
%	write(S,'      set_ftime,\n'),
write(S,'	self(ModuleName),\n'),
write(S,'	atoms_concat([ModuleName,'':'',setStateChanged], G),\n'),
write(S,'	''$meta_call''(G),\n'),
%	write(S,'	setStateChanged,\n'),
	write(S,'      incState.\n'), nl(S),

	write(S,'parse_arg((lparse(L),smodels(S)),C1,C2) :-\n'),
	write(S,'	parse_lparse_arg(L,C1),\n'),
	write(S,'	parse_smodels_arg(S,C2).\n\n'),

	write(S,'parse_lparse_arg([], []).\n'),
	write(S,'parse_lparse_arg([H|T],C) :-\n'),
	write(S,'	atom(H), !,\n'),
	write(S,'	name(H,String),\n'),
	write(S,'	cnv2atom_list(String,Atom),\n'),
	write(S,'	parse_lparse_arg(T,C1),\n'),
	write(S,'	append(Atom,C1,C).\n'),
	write(S,'parse_lparse_arg([(H,V)|T],C) :-\n'),
	write(S,'    !, cnv_atom(V,A),\n'),
	write(S,'    atoms_concat([H,''='', A], Atom),\n'),
	write(S,'    parse_lparse_arg(T,C1),\n'),
	write(S,'    append([''-c'', Atom], C1,C).\n\n'),

	write(S,'parse_smodels_arg([], []).\n'),
	write(S,'parse_smodels_arg([H|T],C) :-\n'),
	write(S,'	atom(H), !,\n'),
	write(S,'	name(H,String),\n'),
	write(S,'	cnv2atom_list(String,Atom),\n'),
	write(S,'	parse_smodels_arg(T,C1),\n'),
	write(S,'	append(Atom,C1,C).\n\n'),


write(S,'bkparm([]) :- !.\n'),
write(S,'bkparm(L) :- \n'),
	write(S,'      retractall(lparse_set(_,_)),\n'),
	write(S,'      parse_arg(L, C1,C2),'), nl(S),
	write(S,'      assertz(lparse_set(C1,C2)),\n'),
	write(S,'      retractall(parm(_)),'), nl(S),
	write(S,'      assertz(parm(L)),'), nl(S),
%	write(S,'      set_ftime,\n'),
write(S,'	self(ModuleName),\n'),
write(S,'	atoms_concat([ModuleName,'':'',setStateChanged], G),\n'),
write(S,'	''$meta_call''(G),\n'),
	write(S,'      decState.\n\n').
