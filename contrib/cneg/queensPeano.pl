:- module(queensPeano, _,[.(cneg)]).
 
%:- module(queensPeano, [queens/2]).
 
%:- set_prolog_flag(multi_arity_warnings, off).
%:- use_module(dist,[dist/2]).

queens(N, Qs):-
        queens_list(N, Ns),
        queens1(Ns, [], Qs).     % To place, placed, result

queens1([], Qs, Qs).
queens1([X|Unplaced], Placed, Qs):-
        select(Q, [X|Unplaced], NewUnplaced),
        no_attack(Q, Placed),
        queens1(NewUnplaced, [Q|Placed], Qs).
 
no_attack(Q, Safe):- no_attack1(Safe, Q, s(0)). 
 
no_attack1([], _Queen, _Nb).
no_attack1([Y|Ys], Queen, Nb):-
	add(Y,Nb,YNb),
        dist(Queen,YNb),
 	subst(Y,Nb,NbY),
        dist(Queen,NbY),
        add(Nb,s(0),Nb1),
        no_attack1(Ys, Queen, Nb1).

select(X, [X|Ys], Ys).
select(X, [Y|Ys], [Y|Zs]):-
        select(X, Ys, Zs).

select1(X, Y, Z):- 
	Y=[X|Ys], 
	Z=Ys.
select1(X, Y1, Z):-
	Y1=[Y|Ys], 
	Z= [Y|Zs],
        select1(X, Ys, Zs).

queens_list(0, []).
queens_list(N, [N|Ns]):-
        greater(N,0),
        subst(N,s(0),N1),
        queens_list(N1, Ns).


add(0,X,X).
add(s(X),Y,s(Z)):-
	add(X,Y,Z).

subst(Z,X,Y):-
	greater(Z,X),
	add(X,Y,Z).
subst(Z,X,neg):-
	greater(X,Z).
subst(X,X,0).

greater(s(_),0).
greater(s(X),s(Y)):-
	greater(X,Y).

greater1(X1,0):-
	X1=s(_).
greater1(X1,Y1):-
	X1=s(X),
	Y1=s(Y),
	greater1(X,Y).

%queens(0,Q).
%queens(s(0),Q).
%queens(s(s(0)),Q).
%queens(s(s(s(0))),Q).
%queens(s(s(s(s(0)))),Q). 
%queens(s(s(s(s(s(0))))),Q).

no_queens(N,Q):-
 	cneg(queens(N,Q)).
no_queens1(N,L,Q):-
 	cneg(queens1(N,L,Q)).

no_queens_list(N,Q):-
 	cneg(queens_list(N,Q)).
no_greater(X,Y):-
 	cneg(greater(X,Y)).

no_greater1(X,Y):-
 	cneg(greater1(X,Y)).


 
p9(X):-X=s(_).

p91(s(_)).

no_p9(X):-
	cneg(p9(X)).
no_p91(X):-
   	cneg(p91(X)). 


no_subst(N,X,N1):-
  	cneg(subst(N,X,N1)).


p11(X):-
	greater(s(0),0),
	subst(s(0),s(0),Y),			   
	queens_list(Y,Z),
	select(H,[s(0)|Z],J),
	no_attack(H,[]),
	queens1(J,[H],X).
p12(X):-
	greater(s(0),0),
%	subst(s(0),s(0),Y),			   
	queens_list(0,Z),
	select(H,[s(0)|Z],J),
	no_attack(H,[]),
	queens1(J,[H],X).
p13(X):-
	greater(s(0),0),		   
	queens_list(0,Z),
	select(H,[s(0)|Z],J),
	no_attack(H,[]),
	queens1(J,[H],X).
p14(X):-
	greater(s(0),0),		   
	select(H,[s(0)],J),
	no_attack(H,[]),
	queens1(J,[H],X).

p15(X):-
	greater(s(0),0),		   
	select(H,[s(0)],J),
	no_attack(H,[]),
	queens1(J,[H],X).

p16(X):-	   
	select(H,[s(0)],J),
	no_attack(H,[]),
	queens1(J,[H],X).

p17(X):-	   
	select(H,[s(0)],J),
	queens1(J,[H],X).
p18(X):-	   
	selectK(H,[s(0)],J),
	queens1(J,[H],X).

p10(X):-
	greater(s(0),0),		   
	no_attack(s(0),[]),
	queens1([],[s(0)],X).

no_p10(X):-
	cneg(p10(X)).
no_p11(X):-
	cneg(p11(X)).
no_p12(X):-
	cneg(p12(X)).
no_p13(X):-
	cneg(p13(X)).
no_p14(X):-
	cneg(p14(X)).

no_p15(X):- 
	cneg(p15(X)). 

no_p16(X):-
	cneg(p16(X)).

no_p17(X):-
	cneg(p17(X)). 
no_p18(X):-
	cneg(p18(X)). 
no_p19(X):-
	cneg(p19(X)). 
  
no_select(H,I,J):-
	cneg(select(H,I,J)).
 
no_no_attack(H,L):-
       	cneg(no_attack(H,L)).

selectK(s(0),[s(0)],[]).
queens1K([],[s(0)],[s(0)]).

select19(s(0)).

queens19([],Q,Q).
queens19([X|U],R,e).

p19(X):-
	select19(H),
	queens19([],[H],X).
get(w).
 
put([],Q,Q).
put([_|_],_,e).

p20(X):-
	get(H),
	put([],[H],X).
no_p20(X):-
	cneg(p20(X)).
p21(X):-
	get(H),
	put([],H,X).
no_p21(X):-
	cneg(p21(X)).
no_p22(X):-
	cneg(p22(X)).

p22(X):-		   
	queens_list(0,Z),
	select(H,[s(0)|Z],J),
	no_attack(H,[]),
	queens1(J,[H],X).

no_p23(X):-
	cneg(p23(X)).

p23(J):-		   
	queens_list(0,Z),
	select(s(0),[s(0)|Z],J).
%	queens1(J,[s(0)],X).

no_p24(X):-
	cneg(p24(X)).

p24(J):-		   
	get1(Z),
	put1(Z,J).

get1([]).

put1(Ys, Ys).
put1(Ys, [Y|Zs]):-
        put1(Ys, Zs).

no_p25(X):-
	cneg(p25(X)).

p25(J):-		   
	get1(Z),
	put2([Z],J).

put2([Ys], Ys).
put2([_|Ys], [Y|Zs]):-
        put2(Ys, Zs).
p26(J):-		   
	get1(Z),
	select(s(0),[s(0)|Z],J).

no_p26(X):-
	cneg(p26(X)).

no_select1(X,Y,Z):-
	cneg(select1(X,Y,Z)).

no_p29(X):-
	cneg(p29(X)).

p29(J):-		%% Funciona   
	select(s(0),[s(0)],J).

no_p30(X):-
	cneg(p30(X)).

p30(J):-		 %% Falla  
	get1(Z),
	select(s(0),[s(0)|Z],J).

no_p31(X):-              % Funciona
	cneg(p31(X)).

p31(J):-		
	get1(Z),
	select2(s(0),[s(0)|Z],J).

select2(X, [X|Ys], Ys).
%select2(X, [Y|Ys], [Y|Zs]):-
%        select2(X, Ys, Zs).


no_p32(X):-              % Falla
	cneg(p32(X)).

p32(J):-		
	get1(Z),
	select3(s(0),[s(0)|Z],J).

%select2(X, [X|Ys], Ys).
select3(X, [Y|Ys], [Y|Zs]):-
        select2(X, Ys, Zs).
