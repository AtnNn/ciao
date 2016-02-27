negate_conj_frontier(

[
((queens_list(s(0),[s(0)|_12724]),queens1([_12862|_12863],[],_172)),
 [greater(s(0),0),
  subst(s(0),s(0),_12739),
  queens_list(_12739,_12724),
  select(_12867,[_12862|_12863],_12869),
  no_attack(_12867,[]),
  queens1(_12869,[_12867],_172)])
],

(queens_list(s(0),_2493),queens1(_2493,[],_172)),

[_172],

Posibles soluciones
[[dist(fA(_2493),[fA(_12862)|fA(_12863)])]])  ---- falla
[[_2493=[_12862|_12863],cneg(greater(s(0),0))]] ---- falla
[[_2493=[_12862|_12863],greater(s(0),0),cneg_aux((subst(s(0),s(0),fA(_12739)),queens_list(fA(_12739),fA(_12724)),select(fA(...),[_12862|_12863],fA(...)),no_attack(...),queens1(...)),[_2493,_12862,_12863,_172])]]  --- se cuelga

Mejor paso por paso:
?- p17(X).

X = [s(0)] ? ;

no
?- no_p17(X).

attach_attribute(X,formula(X,[[X/[fA(_A)]]])),
attach_attribute(_A,formula(_A,[[X/[fA(_A)]]])) ? ;

no

?- no_queens1([],[s(0)],X).

attach_attribute(X,formula(X,[[X/[s(0)]]])) ? ;

no
?- select(H,[s(0)],J).

H = s(0),
J = [] ? ;

no
?- no_select(H,[s(0)],J).

attach_attribute(_A,formula(_A,[[J/[s(0)|fA(_A)]]])),
attach_attribute(J,formula(J,[[J/[],J/[s(0)|fA(_A)]]])) ? ;

attach_attribute(J,formula(J,[[J/[s(0)|fA(_A)]]])),
attach_attribute(_A,formula(_A,[[J/[s(0)|fA(_A)]]])),
attach_attribute(H,formula(H,[[H/s(0)]])) ? ;

J = [s(0)|_] ? ;

J = [s(0)|_],
attach_attribute(H,formula(H,[[H/s(0)]])) ? ;

no
?