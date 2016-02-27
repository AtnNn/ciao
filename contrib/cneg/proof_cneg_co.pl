:- module(_proof_cneg,[stored_clause/2,stored_pred/2,cneg/1,cneg_aux/2,compound_to_term/2,is_eq_diseq/1,frontier/2,filter_frontier/3,conjunction/3,distributive/3,conj_clauses/3,negate_conj_frontier/4,negate_conj/4,varset_order/2,organization_conj/6,get_eq_head/3,obtain_eq_head/5,organization_body/4,detach_attribute_all/1,detach_att_if_needed/1,organization_body_diseq/6,organization_body_diseq_aux/6,replace_dist/2,remove_duplicates/2,obtain_diseq/2,normalization_conj/8,difference/3,union/3,eliminate_redundant_var/3,eliminate_repeated_eq/2,eliminate_irrelevant_disequalities/4,negation_conj/7,divide_formula/4,negation_formula/9,negation_formula1/7,negate_I/3,negate_eq/3,replace_free_vars/3,replace/4,replace_all/4,replace_list/4,rm_fA/2,rm_fA_all/2,negate_Dimp/2,negate_Rimp/2,negate_Dexp_Rexp/4,get_conjunction/2,combine/1,call_all/1,start_of_file/1,odd/1,not_odd_number/1,parent/2,grandparent/2,boole/1,binary_list/1,dist_list/1,p/4,p1/1,p2/1,p3/1,p4/2,q/1,r/2,positive/1,natural/1,pred2/2,pred1/1,iguales/2,r1/2,r2/2,r3/2,r4/2,r5/3,no_boole/1,no_binary_list/1,no_dist_list/1,no_p/4,no_q/1,no_r/2,no_p1/1,no_p2/1,no_p3/1,no_p4/2,no_positive/1,no_natural/1,no_parent/2,no_grandparent/2,no_pred2/2,no_pred1/1,no_iguales/2,no_r1/2,no_r2/2,no_r3/2,no_r4/2,no_r5/3],ciaopp).

:- meta_predicate cneg(goal).

:- meta_predicate stored_pred(goal,?).

:- use_module(library(metaterms),[varset/2,varsbag/3]).

:- use_module(library(lists),[append/3]).

:- use_module(library(idlists),[memberchk/2]).

:- use_module(library(aggregates),[setof/3]).

:- use_module(engine(internals),[term_to_meta/2]).

:- use_module(dist,[dist/2]).

:- data stored_clause/2.

:- data stored_pred/2.

cneg(Meta) :-
        term_to_meta(Goal,Meta),
        compound_to_term(Goal,Term),
        varset(Term,GoalVars),
        cneg_aux(Term,GoalVars).

cneg_aux(GoalI,GoalVarsI) :-
        rm_fA(GoalI,Goal),
        varset(GoalVarsI,GoalVars),
        frontier(Goal,Frontier),
        negate_conj_frontier(Frontier,Goal,GoalVars,LSolutions),
        combine(LSolutions).

compound_to_term((M1;M2),(G1;G2)) :-
        !,
        compound_to_term(M1,G1),
        compound_to_term(M2,G2).

compound_to_term((M1,M2),(G1,G2)) :-
        !,
        compound_to_term(M1,G1),
        compound_to_term(M2,G2).

compound_to_term(T,T) :-
        is_eq_diseq(T),
        !.

compound_to_term(T,G) :-
        term_to_meta(T,M),
        stored_pred(M,G),
        !.

is_eq_diseq(dist(_1,_2)).

is_eq_diseq(_1=_2).

frontier((G1;G2),Frontier) :-
        !,
        frontier(G1,F1),
        frontier(G2,F2),
        append(F1,F2,Front),
        filter_frontier(Front,(G1;G2),Frontier).

frontier((G1,G2),Frontier) :-
        !,
        frontier(G1,F1),
        frontier(G2,F2),
        conjunction(F1,F2,Front),
        filter_frontier(Front,(G1,G2),Frontier).

frontier(dist(X,Y),[(dist(X,Y),[dist(X,Y)])]) :- !.

frontier(X=_Y,[(X=X,[])]) :- !.

frontier(Goal,Frontier) :-
        setof((Goal,Body),stored_clause(Goal,Body),Frontier),
        !.

frontier(_Goal,[]).

filter_frontier([],_G,[]).

filter_frontier([(H,B)|Front],G,[(H,B)|Frontier]) :-
        copy_term(H,H1),
        copy_term(G,G1),
        G1=H1,
        !,
        filter_frontier(Front,G,Frontier).

filter_frontier([(_H,_B)|Front],G,Frontier) :-
        filter_frontier(Front,G,Frontier).

conjunction([],_F2,[]) :- !.

conjunction([Conj|F1],F2,F5) :-
        distributive(F2,Conj,F3),
        conjunction(F1,F2,F4),
        append(F3,F4,F5).

distributive([],_Conj,[]).

distributive([Conj2|L],Conj1,[Conj3|L3]) :-
        conj_clauses(Conj1,Conj2,Conj3),
        distributive(L,Conj1,L3).

conj_clauses((H1,B1),(H2,B2),((H1,H2),B3)) :-
        append(B1,B2,B3).

negate_conj_frontier([],_Goal,_GoalVars,[]).

negate_conj_frontier([Conj|Frontier],Goal,GoalVars,[SolConj|LSolutions]) :-
        negate_conj(Conj,Goal,GoalVars,SolConj),
        negate_conj_frontier(Frontier,Goal,GoalVars,LSolutions).

negate_conj(C,G,GoalVars,[]) :-
        organization_conj(C,G,GoalVars,[fail],_D,_R),
        !.

negate_conj(C,G,GoalVars,SolC) :-
        organization_conj(C,G,GoalVars,I,D,R),
        'negate_conj/4/2/$disj/1'(GoalVars,SolC,I,D,R).

'negate_conj/4/2/$disj/1'(GoalVars,SolC,I,D,R) :-
        I=[],
        D=[],
        R=[],
        !,
        fail.

'negate_conj/4/2/$disj/1'(GoalVars,SolC,I,D,R) :-
        normalization_conj(I,D,R,GoalVars,In,Dn,ImpVars,ExpVars),
        negation_conj(In,Dn,R,GoalVars,ImpVars,ExpVars,SolC).

varset_order(Term,Vars) :-
        varsbag(Term,VarsDup,[]),
        remove_duplicates(VarsDup,Vars).

organization_conj((Head,BodyList),G,_GoalVars,I,D,R) :-
        copy_term(G,G1),
        copy_term(Head,Head1),
        varset_order(G1,GoalVars1),
        varset_order(G,GVars),
        varset_order(Head,HeadVars),
        varset_order(Head1,HeadVars1),
        setof(GoalVars1,G1=Head1,[Values]),
        !,
        'organization_conj/6/1/$disj/1'(I,D,R,BodyList,GVars,HeadVars,HeadVars1,Values).

organization_conj((_Head,BodyList),_G,_GoalVars,I,D,R) :-
        organization_body(BodyList,I,D,R).

'organization_conj/6/1/$disj/1'(I,D,R,BodyList,GVars,HeadVars,HeadVars1,Values) :-
        Values=[],
        !,
        organization_body(BodyList,I,D,R).

'organization_conj/6/1/$disj/1'(I,D,R,BodyList,GVars,HeadVars,HeadVars1,Values) :-
        replace_list(HeadVars1,HeadVars,Values,Values1),
        get_eq_head(GVars,Values1,Ih),
        organization_body(BodyList,Ib,D,R),
        append(Ih,Ib,I).

get_eq_head(GoalVars,Values,Ih) :-
        obtain_eq_head(GoalVars,Values,_Values1,_RepVars,Ih).

obtain_eq_head([],[],[],[],[]).

obtain_eq_head([Var|GoalVars],[X|Vs],[X|NewVs],RepVars,[Var=X|I]) :-
        var(X),
        obtain_eq_head(GoalVars,Vs,NewVs,RepVars,I),
        memberchk(X,RepVars),
        !.

obtain_eq_head([Var|GoalVars],[X|Vs],[Var|NewVs],[Var|RepVars],I) :-
        var(X),
        var(Var),
        !,
        Var=X,
        obtain_eq_head(GoalVars,Vs,NewVs,RepVars,I).

obtain_eq_head([Var|GoalVars],[X|Vs],[X|NewVs],RepVars,[Var=X|I]) :-
        obtain_eq_head(GoalVars,Vs,NewVs,RepVars,I).

organization_body([],[],[],[]).

organization_body([T1=T2|BodyList],I,D,R) :-
        varset_order(T1,T1Vars),
        varset_order(T2,T2Vars),
        append(T1Vars,T2Vars,T12Vars),
        remove_duplicates(T12Vars,Vars),
        setof(Vars,T1=T2,[Values]),
        !,
        'organization_body/4/2/$disj/1'(I,D,R,BodyList,Vars,Values).

organization_body([_T1=_T2|_BodyList],[fail],_D,_R) :- !.

organization_body([dist(T1,T2)|BodyList],I,D,R) :-
        varset_order(dist(T1,T2),VarsI),
        copy_term(dist(T1,T2),DNew),
        varset_order(DNew,VarsNew),
        detach_attribute_all(VarsNew),
        remove_duplicates(VarsNew,Vars),
        setof(VarsNew,DNew,Values),
        !,
        'organization_body/4/4/$disj/1'(I,D,R,BodyList,VarsI,VarsNew,Vars,Values).

organization_body([dist(_T1,_T2)|_BodyList],[fail],_D,_R) :- !.

organization_body([Other|BodyList],I,D,[Other|R]) :-
        organization_body(BodyList,I,D,R).

'organization_body/4/2/$disj/1'(I,D,R,BodyList,Vars,Values) :-
        Values=[],
        !,
        organization_body(BodyList,I,D,R).

'organization_body/4/2/$disj/1'(I,D,R,BodyList,Vars,Values) :-
        get_eq_head(Vars,Values,Ih),
        organization_body(BodyList,Ib,D,R),
        append(Ih,Ib,I).

'organization_body/4/4/$disj/1'(I,D,R,BodyList,VarsI,VarsNew,Vars,Values) :-
        Values=[[]],
        !,
        organization_body(BodyList,I,D,R).

'organization_body/4/4/$disj/1'(I,D,R,BodyList,VarsI,VarsNew,Vars,Values) :-
        organization_body_diseq(Values,Vars,BodyList,I1,D1,R1),
        replace_list(VarsNew,VarsI,(I1,D1,R1),(I,D,R)).

detach_attribute_all([]).

detach_attribute_all([V|Vars]) :-
        detach_att_if_needed(V),
        detach_attribute_all(Vars).

detach_att_if_needed(Var) :-
        get_attribute(Var,_1),
        !,
        detach_attribute(Var).

detach_att_if_needed(_1).

organization_body_diseq([],_Vars,BodyList,I,D,R) :-
        !,
        organization_body(BodyList,I,D,R).

organization_body_diseq(Values,Vars,BodyList,I,D,R) :-
        member(ValueVars,Values),
        organization_body_diseq_aux(ValueVars,Vars,BodyList,I,D,R).

organization_body_diseq_aux([V|ValueVars],Vars,BodyList,I,D,R) :-
        obtain_diseq(V,D1),
        replace_list([V|ValueVars],Vars,D1,Dh),
        replace_dist(Dh,Daux),
        organization_body(BodyList,I,Db,R),
        append(Daux,Db,D).

replace_dist([],[]).

replace_dist([X/Y|List],[dist(X,Y)|ListD]) :-
        replace_dist(List,ListD).

remove_duplicates([],[]).

remove_duplicates([X|L1],L2) :-
        memberchk(X,L1),
        !,
        remove_duplicates(L1,L2).

remove_duplicates([X|L1],[X|L2]) :-
        remove_duplicates(L1,L2).

obtain_diseq(Value,D) :-
        get_attribute(Value,formula(Value,Form)),
        member(D,Form).

normalization_conj(I,D,R,GoalVars,I1,D1,ImpVars,ExpVars) :-
        varset(R,RVars),
        difference(RVars,GoalVars,RelVars),
        varset(I,IVars),
        union(IVars,GoalVars,ImpVars),
        difference(RVars,ImpVars,ExpVars),
        eliminate_redundant_var(I,GoalVars,Iv),
        eliminate_repeated_eq(Iv,I1),
        eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1).

difference([],_NFVars,[]).

difference([Var|Vars],NFVars,FreeVars) :-
        memberchk(Var,NFVars),
        !,
        difference(Vars,NFVars,FreeVars).

difference([Var|Vars],NFVars,[Var|FreeVars]) :-
        difference(Vars,NFVars,FreeVars).

union([],Vars2,Vars2).

union([Var|Vars1],Vars2,Vars) :-
        memberchk(Var,Vars2),
        !,
        union(Vars1,Vars2,Vars).

union([Var|Vars1],Vars2,[Var|Vars]) :-
        union(Vars1,Vars2,Vars).

eliminate_redundant_var([],_GoalVars,[]).

eliminate_redundant_var([Var=Value|I],GoalVars,Iv) :-
        var(Value),
        var(Var),
        \+memberchk(Var,GoalVars),
        !,
        Var=Value,
        eliminate_redundant_var(I,GoalVars,Iv).

eliminate_redundant_var([Var=Value|I],GoalVars,Iv) :-
        var(Var),
        var(Value),
        \+memberchk(Value,GoalVars),
        !,
        Var=Value,
        eliminate_redundant_var(I,GoalVars,Iv).

eliminate_redundant_var([Var=Value|I],GoalVars,[Var=Value|Iv]) :-
        eliminate_redundant_var(I,GoalVars,Iv).

eliminate_repeated_eq([],[]).

eliminate_repeated_eq([X=Y|L1],L2) :-
        X==Y,
        !,
        eliminate_repeated_eq(L1,L2).

eliminate_repeated_eq([X=Y|L1],L2) :-
        memberchk(X=Y,L1),
        !,
        eliminate_repeated_eq(L1,L2).

eliminate_repeated_eq([X=Y|L1],L2) :-
        memberchk(Y=X,L1),
        !,
        eliminate_repeated_eq(L1,L2).

eliminate_repeated_eq([E|L1],[E|L2]) :-
        eliminate_repeated_eq(L1,L2).

eliminate_irrelevant_disequalities([],_ImpVars,_RelVars,[]).

eliminate_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,D1) :-
        varset(Diseq,Vars),
        member(V,Vars),
        \+memberchk(V,ImpVars),
        \+memberchk(V,RelVars),
        !,
        eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1).

eliminate_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,[Diseq|D1]) :-
        eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1).

negation_conj(I,D,R,GoalVars,ImpVars,ExpVars,SolC) :-
        divide_formula(D,ExpVars,Dimp,Dexp),
        divide_formula(R,ExpVars,Rimp,Rexp),
        negation_formula(I,Dimp,Dexp,Rimp,Rexp,GoalVars,ImpVars,ExpVars,SolC).

divide_formula([],_ExpVars,[],[]).

divide_formula([Term|F],ExpVars,Fimp,[Term|Fexp]) :-
        varset(Term,Vars),
        member(V,Vars),
        memberchk(V,ExpVars),
        !,
        divide_formula(F,ExpVars,Fimp,Fexp).

divide_formula([Term|F],ExpVars,[Term|Fimp],Fexp) :-
        divide_formula(F,ExpVars,Fimp,Fexp).

negation_formula(I,_Dimp,_Dexp,_Rimp,_Rexp,GoalVars,_ImpV,_ExpV,[Sol]) :-
        negate_I(I,GoalVars,Sol).

negation_formula(I,Dimp,_Dexp,_Rimp,_Rexp,_GoalV,_ImpV,_ExpV,SolC) :-
        negate_Dimp(Dimp,Sol),
        append(I,Sol,SolC).

negation_formula(I,Dimp,Dexp,Rimp,Rexp,_GoalV,ImpVars,ExpsVars,SolC) :-
        append(I,Dimp,I_Dimp),
        negation_formula1(I_Dimp,Dexp,Rimp,Rexp,ImpVars,ExpsVars,SolC).

negation_formula1(I_Dimp,_Dexp,Rimp,_Rexp,_ImpsV,_ExpsV,SolC) :-
        negate_Rimp(Rimp,Sol),
        append(I_Dimp,Sol,SolC).

negation_formula1(I_Dimp,Dexp,Rimp,Rexp,ImpVars,ExpVars,SolC) :-
        append(I_Dimp,Rimp,I_Dimp_Rimp),
        append(Dexp,Rexp,DRexp),
        negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,Sol),
        append(I_Dimp_Rimp,Sol,SolC).

negate_I(I,GoalVars,Sol) :-
        member(Eq,I),
        negate_eq(Eq,GoalVars,Sol).

negate_eq(T1=T2,GoalVars,dist(T1f,T2f)) :-
        varset(T1=T2,Vars),
        difference(Vars,GoalVars,FreeVars),
        replace_free_vars(FreeVars,T1,T1f),
        replace_free_vars(FreeVars,T2,T2f).

replace_free_vars([],T,T).

replace_free_vars([X|FreeVars],T,Tf) :-
        replace(T,X,fA(X),Tf1),
        replace_free_vars(FreeVars,Tf1,Tf).

replace(Term,Var,Value,Value) :-
        Term==Var,
        !.

replace(Term,Var,_Value,Term) :-
        var(Term),
        Term\==Var,
        !.

replace(Term,Var,Value,Term1) :-
        Term=..[Functor|Args],
        replace_all(Args,Var,Value,Args1),
        Term1=..[Functor|Args1].

replace_all([],_1,_2,[]).

replace_all([Arg|Rest],Var,Value,[Arg1|Rest1]) :-
        replace(Arg,Var,Value,Arg1),
        replace_all(Rest,Var,Value,Rest1).

replace_list([],[],Term,Term).

replace_list([Var|LVars],[Value|LValues],Term1,Term2) :-
        var(Var),
        !,
        replace(Term1,Var,Value,TermAux),
        replace_list(LVars,LValues,TermAux,Term2).

replace_list([_Var|LVars],[_Value|LValues],Term1,Term2) :-
        replace_list(LVars,LValues,Term1,Term2).

rm_fA(Term,Term) :-
        ground(Term),
        !.

rm_fA(Term,Term) :-
        var(Term),
        !.

rm_fA(fA(Var),Var) :-
        var(Var),
        !.

rm_fA(Term,Term1) :-
        Term=..[Functor|Args],
        rm_fA_all(Args,Args1),
        Term1=..[Functor|Args1].

rm_fA_all([],[]).

rm_fA_all([Arg|Args],[Arg1|Args1]) :-
        rm_fA(Arg,Arg1),
        rm_fA_all(Args,Args1).

negate_Dimp([D|_Dimp],[T1=T2]) :-
        replace(D,fA(X),X,dist(T1,T2)).

negate_Dimp([D|Dimp],[D|SolC]) :-
        negate_Dimp(Dimp,SolC).

negate_Rimp([R|_Rimp],[cneg(R)]).

negate_Rimp([R|Rimp],[R|SolC]) :-
        negate_Rimp(Rimp,SolC).

negate_Dexp_Rexp([],_1,_2,_3) :-
        !,
        fail.

negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,[cneg_aux(DR,ImpVars)]) :-
        replace_free_vars(ExpVars,DRexp,DRList),
        get_conjunction(DRList,DR).

get_conjunction([X],X).

get_conjunction([X|L],(X,C)) :-
        get_conjunction(L,C).

combine([]).

combine([Sol|Rest]) :-
        call_all(Sol),
        combine(Rest).

call_all([]).

call_all([G|L]) :-
        call(G),
        call_all(L).

start_of_file('/home/susana/tesis/micodigo/cneg/version7/proof_cneg').

odd(s(0)).

odd(s(s(_1))) :-
        odd(_1).

not_odd_number(_1) :-
        cneg(odd(_1)).

parent(a1,p).

parent(a2,p).

parent(p,h1).

parent(p,h2).

grandparent(_1,_2) :-
        parent(_1,_3),
        parent(_3,_2).

boole(0).

boole(1).

binary_list([]).

binary_list([_1|_2]) :-
        boole(_1),
        binary_list(_2).

dist_list([]).

dist_list([(_2,_3)|_1]) :-
        dist(_2,_3),
        dist_list(_1).

p(1,2,_1,_1) :-
        dist(_1,3),
        dist(_1,_2),
        q(_2).

p1(_1) :-
        dist(_1,3),
        dist(_1,_2),
        q(_2).

p2(_1) :-
        dist(_1,3),
        dist(_1,_2).

p3(_1) :-
        dist(_1,3),
        dist(_1,5).

p4(_1,_2) :-
        dist(_1,3),
        dist(_2,5).

q(_1) :-
        dist(_1,0).

q(_1) :-
        r(_1,_2).

r(8,9).

positive(0).

positive(s(_1)) :-
        positive(_1).

natural(_1) :-
        dist(_1,0),
        positive(_1).

pred2(7,_1).

pred2(9,_1) :-
        dist(_1,5),
        pred1(_2).

pred1(2).

iguales(_1,_1).

r1(1,1).

r1(2,2).

r2(_1,_2) :-
        r(_1,_2).

r3(1,2).

r3(2,3).

r4(_1,_2) :-
        r3(_1,_3),
        r3(_3,_2).

r5(_1,_2,_3) :-
        r3(_1,_2),
        r3(_2,_3).

no_boole(_1) :-
        cneg(boole(_1)).

no_binary_list(_1) :-
        cneg(binary_list(_1)).

no_dist_list(_1) :-
        cneg(dist_list(_1)).

no_p(_1,_2,_3,_4) :-
        cneg(p(_1,_2,_3,_4)).

no_q(_1) :-
        cneg(q(_1)).

no_r(_1,_2) :-
        cneg(r(_1,_2)).

no_p1(_1) :-
        cneg(p1(_1)).

no_p2(_1) :-
        cneg(p2(_1)).

no_p3(_1) :-
        cneg(p3(_1)).

no_p4(_1,_2) :-
        cneg(p4(_1,_2)).

no_positive(_1) :-
        cneg(positive(_1)).

no_natural(_1) :-
        cneg(natural(_1)).

no_parent(_1,_2) :-
        cneg(parent(_1,_2)).

no_grandparent(_1,_2) :-
        cneg(grandparent(_1,_2)).

no_pred2(_1,_2) :-
        cneg(pred2(_1,_2)).

no_pred1(_1) :-
        cneg(pred1(_1)).

no_iguales(_1,_2) :-
        cneg(iguales(_1,_2)).

no_r1(_1,_2) :-
        cneg(r1(_1,_2)).

no_r2(_1,_2) :-
        cneg(r2(_1,_2)).

no_r3(_1,_2) :-
        cneg(r3(_1,_2)).

no_r4(_1,_2) :-
        cneg(r4(_1,_2)).

no_r5(_1,_2,_3) :-
        cneg(r5(_1,_2,_3)).

stored_clause(start_of_file('/home/susana/tesis/micodigo/cneg/version7/proof_cneg'),[]).

stored_clause(odd(s(0)),[]).

stored_clause(odd(s(s(_1))),[odd(_1)]).

stored_clause(not_odd_number(_1),[cneg(odd(_1))]).

stored_clause(parent(a1,p),[]).

stored_clause(parent(a2,p),[]).

stored_clause(parent(p,h1),[]).

stored_clause(parent(p,h2),[]).

stored_clause(grandparent(_1,_2),[parent(_1,_3),parent(_3,_2)]).

stored_clause(boole(0),[]).

stored_clause(boole(1),[]).

stored_clause(binary_list([]),[]).

stored_clause(binary_list([_1|_2]),[boole(_1),binary_list(_2)]).

stored_clause(dist_list([]),[]).

stored_clause(dist_list([(_2,_3)|_1]),[dist(_2,_3),dist_list(_1)]).

stored_clause(p(1,2,_1,_1),[dist(_1,3),dist(_1,_2),q(_2)]).

stored_clause(p1(_1),[dist(_1,3),dist(_1,_2),q(_2)]).

stored_clause(p2(_1),[dist(_1,3),dist(_1,_2)]).

stored_clause(p3(_1),[dist(_1,3),dist(_1,5)]).

stored_clause(p4(_1,_2),[dist(_1,3),dist(_2,5)]).

stored_clause(q(_1),[dist(_1,0)]).

stored_clause(q(_1),[r(_1,_2)]).

stored_clause(r(8,9),[]).

stored_clause(positive(0),[]).

stored_clause(positive(s(_1)),[positive(_1)]).

stored_clause(natural(_1),[dist(_1,0),positive(_1)]).

stored_clause(pred2(7,_1),[]).

stored_clause(pred2(9,_1),[dist(_1,5),pred1(_2)]).

stored_clause(pred1(2),[]).

stored_clause(iguales(_1,_1),[]).

stored_clause(r1(1,1),[]).

stored_clause(r1(2,2),[]).

stored_clause(r2(_1,_2),[r(_1,_2)]).

stored_clause(r3(1,2),[]).

stored_clause(r3(2,3),[]).

stored_clause(r4(_1,_2),[r3(_1,_3),r3(_3,_2)]).

stored_clause(r5(_1,_2,_3),[r3(_1,_2),r3(_2,_3)]).

stored_clause(no_boole(_1),[cneg(boole(_1))]).

stored_clause(no_binary_list(_1),[cneg(binary_list(_1))]).

stored_clause(no_dist_list(_1),[cneg(dist_list(_1))]).

stored_clause(no_p(_1,_2,_3,_4),[cneg(p(_1,_2,_3,_4))]).

stored_clause(no_q(_1),[cneg(q(_1))]).

stored_clause(no_r(_1,_2),[cneg(r(_1,_2))]).

stored_clause(no_p1(_1),[cneg(p1(_1))]).

stored_clause(no_p2(_1),[cneg(p2(_1))]).

stored_clause(no_p3(_1),[cneg(p3(_1))]).

stored_clause(no_p4(_1,_2),[cneg(p4(_1,_2))]).

stored_clause(no_positive(_1),[cneg(positive(_1))]).

stored_clause(no_natural(_1),[cneg(natural(_1))]).

stored_clause(no_parent(_1,_2),[cneg(parent(_1,_2))]).

stored_clause(no_grandparent(_1,_2),[cneg(grandparent(_1,_2))]).

stored_clause(no_pred2(_1,_2),[cneg(pred2(_1,_2))]).

stored_clause(no_pred1(_1),[cneg(pred1(_1))]).

stored_clause(no_iguales(_1,_2),[cneg(iguales(_1,_2))]).

stored_clause(no_r1(_1,_2),[cneg(r1(_1,_2))]).

stored_clause(no_r2(_1,_2),[cneg(r2(_1,_2))]).

stored_clause(no_r3(_1,_2),[cneg(r3(_1,_2))]).

stored_clause(no_r4(_1,_2),[cneg(r4(_1,_2))]).

stored_clause(no_r5(_1,_2,_3),[cneg(r5(_1,_2,_3))]).

stored_pred(start_of_file(_1),start_of_file(_1)).

stored_pred(odd(_1),odd(_1)).

stored_pred(odd(_1),odd(_1)).

stored_pred(not_odd_number(_1),not_odd_number(_1)).

stored_pred(parent(_1,_2),parent(_1,_2)).

stored_pred(parent(_1,_2),parent(_1,_2)).

stored_pred(parent(_1,_2),parent(_1,_2)).

stored_pred(parent(_1,_2),parent(_1,_2)).

stored_pred(grandparent(_1,_2),grandparent(_1,_2)).

stored_pred(boole(_1),boole(_1)).

stored_pred(boole(_1),boole(_1)).

stored_pred(binary_list(_1),binary_list(_1)).

stored_pred(binary_list(_1),binary_list(_1)).

stored_pred(dist_list(_1),dist_list(_1)).

stored_pred(dist_list(_1),dist_list(_1)).

stored_pred(p(_1,_2,_3,_4),p(_1,_2,_3,_4)).

stored_pred(p1(_1),p1(_1)).

stored_pred(p2(_1),p2(_1)).

stored_pred(p3(_1),p3(_1)).

stored_pred(p4(_1,_2),p4(_1,_2)).

stored_pred(q(_1),q(_1)).

stored_pred(q(_1),q(_1)).

stored_pred(r(_1,_2),r(_1,_2)).

stored_pred(positive(_1),positive(_1)).

stored_pred(positive(_1),positive(_1)).

stored_pred(natural(_1),natural(_1)).

stored_pred(pred2(_1,_2),pred2(_1,_2)).

stored_pred(pred2(_1,_2),pred2(_1,_2)).

stored_pred(pred1(_1),pred1(_1)).

stored_pred(iguales(_1,_2),iguales(_1,_2)).

stored_pred(r1(_1,_2),r1(_1,_2)).

stored_pred(r1(_1,_2),r1(_1,_2)).

stored_pred(r2(_1,_2),r2(_1,_2)).

stored_pred(r3(_1,_2),r3(_1,_2)).

stored_pred(r3(_1,_2),r3(_1,_2)).

stored_pred(r4(_1,_2),r4(_1,_2)).

stored_pred(r5(_1,_2,_3),r5(_1,_2,_3)).

stored_pred(no_boole(_1),no_boole(_1)).

stored_pred(no_binary_list(_1),no_binary_list(_1)).

stored_pred(no_dist_list(_1),no_dist_list(_1)).

stored_pred(no_p(_1,_2,_3,_4),no_p(_1,_2,_3,_4)).

stored_pred(no_q(_1),no_q(_1)).

stored_pred(no_r(_1,_2),no_r(_1,_2)).

stored_pred(no_p1(_1),no_p1(_1)).

stored_pred(no_p2(_1),no_p2(_1)).

stored_pred(no_p3(_1),no_p3(_1)).

stored_pred(no_p4(_1,_2),no_p4(_1,_2)).

stored_pred(no_positive(_1),no_positive(_1)).

stored_pred(no_natural(_1),no_natural(_1)).

stored_pred(no_parent(_1,_2),no_parent(_1,_2)).

stored_pred(no_grandparent(_1,_2),no_grandparent(_1,_2)).

stored_pred(no_pred2(_1,_2),no_pred2(_1,_2)).

stored_pred(no_pred1(_1),no_pred1(_1)).

stored_pred(no_iguales(_1,_2),no_iguales(_1,_2)).

stored_pred(no_r1(_1,_2),no_r1(_1,_2)).

stored_pred(no_r2(_1,_2),no_r2(_1,_2)).

stored_pred(no_r3(_1,_2),no_r3(_1,_2)).

stored_pred(no_r4(_1,_2),no_r4(_1,_2)).

stored_pred(no_r5(_1,_2,_3),no_r5(_1,_2,_3)).

stored_pred('dist:dist'(_1,_2),dist(_1,_2)).

