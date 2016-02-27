:- module(defparm, [buildDefParm/2]).

:- use_module(library(asp(misc))).

buildDefParm(S, C) :-

%dmsg(1,['C=',C]), display(C), nl,
%dmsg(1,['S=',S,nl]),
write(S,':- data parm/1.\n'),
write(S, 'parm('),
write(S, C),
write(S, ').\n\n'),

parse_arg(C,LPARSE_PARAMETER,SMODELS_PARAMETER), !,
%dmsg(1,['LPARSE_PARAMETER=', LPARSE_PARAMETER, nl]),
write(S,'lparse_set(['),
save_parameter(S,LPARSE_PARAMETER,1),

write(S,'],['),
%dmsg(1,['SMODELS_PARAMETER=', SMODELS_PARAMETER, nl]),
save_parameter(S,SMODELS_PARAMETER,1),
write(S,']).\n').
%assertz_fact(lparse_set(LPARSE_PARAMETER, SMODELS_PARAMETER)),




parse_arg((lparse(L),smodels(S)),C1,C2) :-
	parse_lparse_arg(L,C1),
	parse_smodels_arg(S,C2).
parse_arg([], [], []).

parse_lparse_arg([], []).
parse_lparse_arg([H|T],C) :-
	atom(H), !,
	name(H,String),
	cnv2atom_list(String,Atom),
	parse_lparse_arg(T,C1),
	append(Atom,C1,C).
parse_lparse_arg([(H,V)|T],C) :-
    !, cnv_atom(V,A),
    atoms_concat([H,'=', A], Atom),
    parse_lparse_arg(T,C1),
    append(['-c', Atom], C1,C).

parse_smodels_arg([], []).
parse_smodels_arg([H|T],C) :-
	atom(H), !,
	name(H,String),
	cnv2atom_list(String,Atom),
	parse_smodels_arg(T,C1),
	append(Atom,C1,C).

% parmater saving
save_parameter(_,[],_).
save_parameter(S,[H|T],C) :- 
    put_comma(S,C),
    write(S,''''),
    write(S,H),
    write(S,''''),
    save_parameter(S,T,2).
    
