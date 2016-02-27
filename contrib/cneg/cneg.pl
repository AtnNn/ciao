%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Package  CNEG for including constructive in a program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% To be able to call Pred from cneg
:- meta_predicate
        cneg(goal). 
:- meta_predicate
        stored_pred(goal,?). 

%:- use_module(library(metaterms),[varset/2,varsbag/3]). % ask/2 
:- use_module(library(terms_vars),[varset/2,varsbag/3]). % ask/2 
:- use_module(library(lists),[append/3]).
:- use_module(library(idlists),[memberchk/2]). 
:- use_module(library(aggregates),[setof/3]).
:- use_module(engine(internals),[term_to_meta/2]).

%:- use_module('/home/susana/tesis/micodigo/ciao/actual/dist.pl',[dist/2]).
%:- use_module(library(dist),[dist/2]).
:- use_module( library( 'cneg/dist.pl' ) ,[dist/2,get_dist_attribute/2,detach_dist_attribute/1]).

%:- reexport(dist,[dist/2]).

% stored_clause/2 is going to be a dynamic predicate in the
% programs that are going to include this package
:- data stored_clause/2.

% stored_pred/2 is going to be a dynamic predicate in the
% programs that are going to include this package
:- data stored_pred/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cneg(Meta) makes the constructive negation of the goal Meta.
cneg(Meta):-  
	term_to_meta(Goal,Meta),
	compound_to_term(Goal,Term), 
	varset(Term,GoalVars),
	cneg_aux(Term,GoalVars). 
 
% cneg_aux(Goal,GoalVars) makes the constructive negation of the goal
% Goal considering the set of variables GoalVars as the variables
% of the goal.
cneg_aux(GoalI,GoalVarsI):-
	rm_fA(GoalI,Goal),
	varset(GoalVarsI,GoalVars),
	frontier(Goal,Frontier),
	negate_conj_frontier(Frontier,Goal,GoalVars,LSolutions),
	combine(LSolutions).

% compound_to_term(G,T) obtains the possibly compound term T
% fron the term G that comes from a metaterm and contains the module
% qualification
compound_to_term((M1;M2),(G1;G2)):- !,
	compound_to_term(M1,G1),
	compound_to_term(M2,G2).
compound_to_term((M1,M2),(G1,G2)):- !,
	compound_to_term(M1,G1),
	compound_to_term(M2,G2).
compound_to_term(T,T):- 
	is_eq_diseq(T),!.
compound_to_term(T,G):- % It's a simple term
	term_to_meta(T,M),
	stored_pred(M,G),!.

% is_eq_diseq(T) success if T is dist(_,_) or _=_
is_eq_diseq(dist(_,_)).
is_eq_diseq((_=_)). 
 
% frontier(Goal,Frontier) obtains in Frontier the frontier of the 
% goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.
frontier((G1;G2),Frontier):- !,
	frontier(G1,F1),
	frontier(G2,F2),
	append(F1,F2,Front),
	filter_frontier(Front,(G1;G2),Frontier).
frontier((G1,G2),Frontier):- !,
	frontier(G1,F1),
	frontier(G2,F2),
	conjunction(F1,F2,Front),
	filter_frontier(Front,(G1,G2),Frontier).
frontier(dist(X,Y),[(dist(X,Y),[dist(X,Y)])]):- !.
frontier((X=_Y),[(X=X,[])]):- !.
frontier(Goal,Frontier):-
	setof((Goal,Body),stored_clause(Goal,Body),Frontier),!.
frontier(_Goal,[]).

% filter_frontier(Front,G,Frontier) returns in Frontier the list
% with the elementos of Front whose head unifies with G.
filter_frontier([],_G,[]).
filter_frontier([(H,B)|Front],G,[(H,B)|Frontier]):-
	copy_term(H,H1),
	copy_term(G,G1),
	G1=H1,!,
 	filter_frontier(Front,G,Frontier).
% filter_frontier([(H,B)|Front],G,[(H,B)|Frontier]):-
% 	ask(H,G),!,
% 	filter_frontier(Front,G,Frontier).
% filter_frontier([(H,B)|Front],G,[(H,B)|Frontier]):-
% 	ask(G,H),!,
% 	filter_frontier(Front,G,Frontier).
filter_frontier([(_H,_B)|Front],G,Frontier):- 
	filter_frontier(Front,G,Frontier).

% conjuntcion(F1,F2,F3) returns F3 that is the conjunction of the list 
% of lists F1 and F2 after using the distributive rule
conjunction([],_F2,[]):-!.
conjunction([Conj|F1],F2,F5):-
        distributive(F2,Conj,F3),
        conjunction(F1,F2,F4),
        append(F3,F4,F5).

% distributive(L1,E,L2) returns L2 that is the list obtained after 
% using the distributive rule to the list of list L1 with respect
% the list E. 
% L1 and L2 are disjunction of conjunctions and E is a conjunction.
distributive([],_Conj,[]).
distributive([Conj2|L],Conj1,[Conj3|L3]):-
        conj_clauses(Conj1,Conj2,Conj3),
        distributive(L,Conj1,L3).

% conj_clauses(C1,C2,C3) joins in C3 clauses C1 and C2
conj_clauses((H1,B1),(H2,B2),((H1,H2),B3)):-
	append(B1,B2,B3).

% negate_conj_frontier(Frontier,Goal,LSolutions) returns in LSolutions
% a list with the same number of elements of the list Frontier.
% Each element of LSolutions will be one solution of negating the 
% conjuction that is in the same position in Frontier. The predicate 
% is a "map" where the fuction that is applied to each element of
% Frontier (each conjunction) is the negation.
% Frontier is the frontier of subgoals of deep 1 of Goal and we need
% it to keep the variables of the Goal and obtain the unifications
negate_conj_frontier([],_Goal,_GoalVars,[]).
negate_conj_frontier([Conj|Frontier],Goal,GoalVars,[SolConj|LSolutions]):-
	negate_conj(Conj,Goal,GoalVars,SolConj),
	negate_conj_frontier(Frontier,Goal,GoalVars,LSolutions).
 
% negate_conj(Conj,Goal,GoalVars,SolConj) returns in SolConj a solution
% of the negation of the conjunction of subgoals Conj of the goal Goal.
% A conjunction is a pair (H,B) with the head of the clause in H and
% the list of subgoals of the body in B.
% It fails if the negation of the conjunction has no solutions
negate_conj(C,G,GoalVars,[]):- % The clause fails, the negation is true
	organization_conj(C,G,GoalVars,[fail],_D,_R),!.
negate_conj(C,G,GoalVars,SolC):-
	organization_conj(C,G,GoalVars,I,D,R),
	((I=[],D=[],R=[]) ->
	    fail  % Its always true and the negation fails
	;
	    normalization_conj(I,D,R,GoalVars,In,Dn,ImpVars,ExpVars), 
	    negation_conj(In,Dn,R,GoalVars,ImpVars,ExpVars,SolC)
	).
	
% varset_order(Term,Vars) returns in Vars the variables of Term
% without duplicates and in the same order that appear in Term
varset_order(Term,Vars):-
	varsbag(Term,VarsDup,[]),
	remove_duplicates(VarsDup,Vars).

% organization_conj(C,G,I,D,R) returns in I the values that are asigned
% to the variables of GoalVars in the equalities, in D the list of the
% disequialities and in R the rest of subgoals of C
organization_conj((Head,BodyList),G,_GoalVars,I,D,R):-
	copy_term(G,G1),
	copy_term(Head,Head1),
	varset_order(G1,GoalVars1), 
	varset_order(G,GVars),
	varset_order(Head,HeadVars),
	varset_order(Head1,HeadVars1), 
	setof(GoalVars1,(G1=Head1),[Values]),!, % Si hay unificaciones en cabeza
	(Values=[] ->
	     organization_body(BodyList,I,D,R)
	;
	    replace_list(HeadVars1,HeadVars,Values,Values1),
	    get_eq_head(GVars,Values1,Ih),
	    organization_body(BodyList,Ib,D,R),
	    append(Ih,Ib,I)
	).
organization_conj((_Head,BodyList),_G,_GoalVars,I,D,R):-
	% Si no hay unificaciones en la cabeza
	organization_body(BodyList,I,D,R).

% get_eq_head(GoalVars,Values,Ih) retuns in Ih the list of equalities
% where Values is a list with one only list of values inside
get_eq_head(GoalVars,Values,Ih):-
	obtain_eq_head(GoalVars,Values,_Values1,_RepVars,Ih).

% % obtain_rep_head(Vs,NewVs,Ir) returns in Ir the list of all the equalities
% % that appears in the unifications of the head of the clauses and facts.
% % We introduce new auxiliar variables to avoid the repetition.
% % For example obtain_rep_head([X,Y,X,Y],[A,B,X,Y],[A=X,B=Y])
% obtain_rep_head([],[],[]).
% obtain_rep_head([X|Vs],[New|NewVs],[(New=X)|I]):-
% 	memberchk(X,Vs),!,
% 	obtain_rep_head(Vs,NewVs,I).
% obtain_rep_head([X|Vs],[X|NewVs],I):-
% 	obtain_rep_head(Vs,NewVs,I).


% obtain_rep_head(GoalVars,Vs,NewVs,_R,Ir) retuns the list Ir where 
% the element
% of the position i is the subgoal Vari=Valuei where Vari is the element
% of GoalVars in the position i and Valuei is the element of Vs
% in the position i.
% If an element of Values is a variable then it is renamed with the same
% name that in GoalVars and the equiality isn't stored in I.
% It must be call with a free variable in _R because it is an auxiliary
% argument that is used to store the repeated variables
obtain_eq_head([],[],[],[],[]).
obtain_eq_head([Var|GoalVars],[X|Vs],[X|NewVs],RepVars,[(Var=X)|I]):-
	var(X),
	obtain_eq_head(GoalVars,Vs,NewVs,RepVars,I),
	memberchk(X,RepVars),!.
obtain_eq_head([Var|GoalVars],[X|Vs],[Var|NewVs],[Var|RepVars],I):-
	var(X), %%% QUITAR EL ARGUMENTO GVars
	var(Var),!,
	Var=X,
	obtain_eq_head(GoalVars,Vs,NewVs,RepVars,I).
obtain_eq_head([Var|GoalVars],[X|Vs],[X|NewVs],RepVars,[(Var=X)|I]):-
	obtain_eq_head(GoalVars,Vs,NewVs,RepVars,I).

%% rename_vars(GoalVars,Values) the elements of Values that are variables
%% are renamed with the same name that the variable in the same place
%% in GoalVars.
%rename_vars([],[]).
%rename_vars([Var|GoalVars],[Value|Values]):-
%	var(Value),!,
%	Var=Value,
%	rename_vars(GoalVars,Values).
%rename_vars([_Var|GoalVars],[_Value|Values]):-
%	rename_vars(GoalVars,Values).

			
% % obtain_eq(GoalVars,Values,I) retuns the list I where the element of the
% % position i is the subgoal Vari=Valuei where Vari is the element
% % of GoalVars in the position i and Valuei is the element of Values
% % in the position i.
% % If an element of Values is a variable then it is renamed with the same
% % name that in GoalVars and the equiality isn't stored in I.
% obtain_eq_renamed([],[],[]).
% obtain_eq_renamed([Var|GoalVars],[Value|U],I):-
% 	var(Value),!,
% 	Var=Value,
% 	obtain_eq_renamed(GoalVars,U,I).
% obtain_eq_renamed([Var|GoalVars],[Value|U],[(Var=Value)|I]):-
% 	obtain_eq_renamed(GoalVars,U,I).

% organization_body(BodyList,Ib,D,R) returns in Ib the equalities,
% in D the disequalities and in R the rest of subgoals of BodyList
organization_body([],[],[],[]).
organization_body([T1=T2|BodyList],I,D,R):- 
	varset_order(T1,T1Vars),
	varset_order(T2,T2Vars),
	append(T1Vars,T2Vars,T12Vars),
	remove_duplicates(T12Vars,Vars),
	setof(Vars,(T1=T2),[Values]),!,  %%% Problems????????
	(Values=[] ->
	    organization_body(BodyList,I,D,R)
	;
	    get_eq_head(Vars,Values,Ih),
	    organization_body(BodyList,Ib,D,R),
	    append(Ih,Ib,I)
	).
organization_body([_T1=_T2|_BodyList],[fail],_D,_R):- !.
organization_body([dist(T1,T2)|BodyList],I,D,R):- 
	varset_order(dist(T1,T2),VarsI),
	copy_term(dist(T1,T2),DNew),
	varset_order(DNew,VarsNew),
        detach_attribute_all(VarsNew),
        remove_duplicates(VarsNew,Vars),
%	varset(T1,T1Vars),
%	varset(T2,T2Vars),
%	append(T1Vars,T2Vars,T12Vars),
%	remove_duplicates(T12Vars,Vars),
	setof(VarsNew,DNew,Values),!,
	(Values=[[]] -> % The disequality is true
	    organization_body(BodyList,I,D,R)
	;
	    organization_body_diseq(Values,Vars,BodyList,I1,D1,R1),
	    replace_list(VarsNew,VarsI,(I1,D1,R1),(I,D,R))
	).
organization_body([dist(_T1,_T2)|_BodyList],[fail],_D,_R):-!. 
organization_body([Other|BodyList],I,D,[Other|R]):- 
	organization_body(BodyList,I,D,R).

% detach_attribute_all(Vars) applies detach_attribute to all elements
% of the list of variables Vars.
detach_attribute_all([]).
detach_attribute_all([V|Vars]):-
	detach_att_if_needed(V),
	detach_attribute_all(Vars).

% detach_att_if_needed(Var) detaches the attribute of Var if it has it.
detach_att_if_needed(Var) :-
        get_dist_attribute(Var,_), !,
        detach_dist_attribute(Var).
detach_att_if_needed(_).

% organization_body_diseq_aux(ValueVars,Vars,BodyList,I,D,R) is the same
% that organization_body/4 but for the organization of the disequalities
organization_body_diseq([],_Vars,BodyList,I,D,R):- !, %% The disequality is true then the negation
	organization_body(BodyList,I,D,R). %% is fail and this solution is eliminated
organization_body_diseq(Values,Vars,BodyList,I,D,R):-
	member(ValueVars,Values),
	organization_body_diseq_aux(ValueVars,Vars,BodyList,I,D,R).

% organization_body_diseq_aux(ValueVars,Vars,BodyList,I,D,R) it is the same
% that organization_body_diseq/6 but with a list of values instead with
% a list of lists of values
organization_body_diseq_aux([V|ValueVars],Vars,BodyList,I,D,R):-
	% We take one value V because the atribute is the same 
        % in all of them and we take the first one e.g.
	obtain_diseq(V,D1),
	replace_list([V|ValueVars],Vars,D1,Dh),
	replace_dist(Dh,Daux),
	organization_body(BodyList,I,Db,R),
	append(Daux,Db,D).

% % first_both(V,X,Lv,Lx) returns V and X that are the first elements of
% % the list Lv and Lx
% first_both(V,X,[V|_],[X|_]).

% replace_dist(Form,Form1) returns Form1 that is the term
% Form but replacing aparitions of / /2 by dist/2
replace_dist([],[]).
replace_dist([X/Y|List],[dist(X,Y)|ListD]):-
	replace_dist(List,ListD).

% remove_duplicates(L1, L2) L2 tiene los elementos de L1 sin repetidos 
remove_duplicates([],[]).
remove_duplicates([X|L1],L2):-
	memberchk(X,L1),!,
	remove_duplicates(L1,L2).
remove_duplicates([X|L1],[X|L2]):-
	remove_duplicates(L1,L2).


% % obtain_diseq_sure(V,Var,D1) returns the same that obtain_diseq/2 but
% % cheching that D1 isn't in the attribute of Var (because then it is a
% % constraint over a variable Var of GoalVars that cannot be negated)
% % D1 is a list of disequalities
% obtain_diseq_sure(V,Var,D1):-
% 	copy_term(V,VNew),
% 	copy_term(Var,VarNew),
% 	obtain_diseq(V,VNew,D1,DN),
% 	VNew=VarNew,
% 	VN1=VN2.     % If this unification fails, then the disequality
%        No esuna desigualdad   % cannot be selected because it is incompatible
                     % with the constraints of the variable Var
	
% obtain_diseq(V,D1) returns the list D where the element of the
% position i is a list of disequalities of the form X/Y that are in a 
% solution obteined from the atributes of the corresponding variables 
% in Values. The attributes of all variables in Values will be the same.
obtain_diseq(Value,D):-
	get_dist_attribute(Value,formula(Value,Form)),
	member(D,Form). % It will generate all the solutions

% normalization_conj(I,D,R,GoalVars,In,Dn,ImpVars,ExVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalization_conj(I,D,R,GoalVars,I1,D1,ImpVars,ExpVars):-
	varset(R,RVars),
	difference(RVars,GoalVars,RelVars),
	varset(I,IVars),
	union(IVars,GoalVars,ImpVars),
	difference(RVars,ImpVars,ExpVars),
	eliminate_redundant_var(I,GoalVars,Iv),  
	eliminate_repeated_eq(Iv,I1),
	eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1). 
 
% difference(Vars,NFVars,FreeVars) retuns in FreeVars the sublist of elements
% of Vars that do not appear in NFVars
difference([],_NFVars,[]).
difference([Var|Vars],NFVars,FreeVars):-
	memberchk(Var,NFVars),!,
	difference(Vars,NFVars,FreeVars).
difference([Var|Vars],NFVars,[Var|FreeVars]):-
	difference(Vars,NFVars,FreeVars).

% union(Vars1,Vars2,Vars) retuns in Vars the union of elements
% of Vars1 and Vars2 
union([],Vars2,Vars2).
union([Var|Vars1],Vars2,Vars):-
	memberchk(Var,Vars2),!,
	union(Vars1,Vars2,Vars).
union([Var|Vars1],Vars2,[Var|Vars]):-
	union(Vars1,Vars2,Vars).

% eliminate_redundant_var(I,GoalVars,Iv) eliminates from I the equalities 
% X=Y that contain a variable, X or Y,that is not in GoalVars and makes 
% the unification.
eliminate_redundant_var([],_GoalVars,[]).
eliminate_redundant_var([(Var=Value)|I],GoalVars,Iv):-
	var(Value), % Only if the Value is a variable
	var(Var),
	\+ memberchk(Var,GoalVars),!,
	Var=Value,
	eliminate_redundant_var(I,GoalVars,Iv).
eliminate_redundant_var([(Var=Value)|I],GoalVars,Iv):-
	var(Var),
	var(Value),
	\+ memberchk(Value,GoalVars),!,
	Var=Value,
	eliminate_redundant_var(I,GoalVars,Iv).
eliminate_redundant_var([(Var=Value)|I],GoalVars,[(Var=Value)|Iv]):-
	eliminate_redundant_var(I,GoalVars,Iv).


% eliminate_repeated_eq(I,I1) eliminates from I the repeated equalities
% or the equalities between the same variable, X=X.
eliminate_repeated_eq([],[]).
eliminate_repeated_eq([X=Y|L1],L2):-
	X==Y,!, 
	eliminate_repeated_eq(L1,L2).
eliminate_repeated_eq([X=Y|L1],L2):-
	memberchk(X=Y,L1),!,
	eliminate_repeated_eq(L1,L2).
eliminate_repeated_eq([X=Y|L1],L2):-
	memberchk(Y=X,L1),!, %% Different order
	eliminate_repeated_eq(L1,L2).
eliminate_repeated_eq([E|L1],[E|L2]):-
	eliminate_repeated_eq(L1,L2).

% eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1) returns D1
% that is D but without disequalities that contains any variable that
% is not in ImpVars neither RelVars
eliminate_irrelevant_disequalities([],_ImpVars,_RelVars,[]).
eliminate_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,D1):-
	varset(Diseq,Vars),
	member(V,Vars),
	\+ memberchk(V,ImpVars),
	\+ memberchk(V,RelVars),!,
	eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1).
eliminate_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,[Diseq|D1]):-
	eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1).

% negation_conj(I,D,R,GoalVars,ImpVars,ExpVars,SolC) returns SolC
% that is one of the solutions of the conjunction that is divided in 
% I, D and R (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
negation_conj(I,D,R,GoalVars,ImpVars,ExpVars,SolC):-
	divide_formula(D,ExpVars,Dimp,Dexp),
	divide_formula(R,ExpVars,Rimp,Rexp),
	negation_formula(I,Dimp,Dexp,Rimp,Rexp,GoalVars,ImpVars,ExpVars,SolC).

% divide_formula(F,ExpVars,Fimp,Fexp) divide F between Fimp and Fexp.
% In Fexp are the elements of F with any variables of ExpVars and
% the rest of elements of F will be in Fimp
divide_formula([],_ExpVars,[],[]).
divide_formula([Term|F],ExpVars,Fimp,[Term|Fexp]):-
	varset(Term,Vars),
	member(V,Vars),
	memberchk(V,ExpVars),!,
	divide_formula(F,ExpVars,Fimp,Fexp).
divide_formula([Term|F],ExpVars,[Term|Fimp],Fexp):-
	divide_formula(F,ExpVars,Fimp,Fexp).

% negation_formula(I,Dimp,Dexp,Rimp,Rexp,GoalVars,ImpVars,ExpVars,SolC)
% returns SolC that is one of the solutions of the conjunction that is divided
% I, Dimp,Dexp,Rimp and Rexp (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
negation_formula(I,_Dimp,_Dexp,_Rimp,_Rexp,GoalVars,_ImpV,_ExpV,[Sol]):-
	negate_I(I,GoalVars,Sol).
negation_formula(I,Dimp,_Dexp,_Rimp,_Rexp,_GoalV,_ImpV,_ExpV,SolC):-
	negate_Dimp(Dimp,Sol),
	append(I,Sol,SolC).
negation_formula(I,Dimp,Dexp,Rimp,Rexp,_GoalV,ImpVars,ExpsVars,SolC):-
	append(I,Dimp,I_Dimp),
	negation_formula1(I_Dimp,Dexp,Rimp,Rexp,ImpVars,ExpsVars,SolC).

% negation_formula1(I_Dimp,Dexp,Rimp,Rexp,ImpVars,ExpVars,SolC)
% returns SolC that is one of the solutions of the conjunction that is divided
% I_Dimp,Dexp,Rimp and Rexp (equalities and disequalities and rest subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables.
% Only returns the solution of negating the rest of subgoals. 
negation_formula1(I_Dimp,_Dexp,Rimp,_Rexp,_ImpsV,_ExpsV,SolC):-
	negate_Rimp(Rimp,Sol),
	append(I_Dimp,Sol,SolC).
negation_formula1(I_Dimp,Dexp,Rimp,Rexp,ImpVars,ExpVars,SolC):-
	append(I_Dimp,Rimp,I_Dimp_Rimp),
	append(Dexp,Rexp,DRexp),
	negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,Sol),
	append(I_Dimp_Rimp,Sol,SolC). 

% negate_I(I,GoalVars,Sol) obtains in Sol a solution of negating I.
negate_I(I,GoalVars,Sol):-
	member(Eq,I),
	negate_eq(Eq,GoalVars,Sol).

% negate_eq(Eq,GoalVars,Sol) returns in Sol a solution of the negation
% of the equality Eq.
negate_eq(T1=T2,GoalVars,dist(T1f,T2f)):-
	varset(T1=T2,Vars),
	difference(Vars,GoalVars,FreeVars),
	replace_free_vars(FreeVars,T1,T1f),
	replace_free_vars(FreeVars,T2,T2f).

% replace_free_vars(FreeVars,T,Tf) returns Tf that is T but substituting
% each aparition of a variable X from FreeVars by fA(X)
% There is no repeated variables in FreeVars.
replace_free_vars([],T,T).
replace_free_vars([X|FreeVars],T,Tf):- 
	replace(T,X,fA(X),Tf1), % Free variables will be universally
	replace_free_vars(FreeVars,Tf1,Tf). % quantified at the negation

% replace(Form,Var,Value,Form1) returns Form1 that is the term
% Form but replacing aparitions of Var by Value
replace(Term,Var,Value,Value):-
	Term==Var,!.
% replace(Term,_Var,_Value,Term):-
% 	ground(Term),!.
% replace(Term,Var,Value,Value):-
% 	var(Term),
% 	Term==Var,!.
replace(Term,Var,_Value,Term):-
	var(Term),
	Term\==Var,!.
replace(Term,Var,Value,Term1):-
	Term=..[Functor|Args],
	replace_all(Args,Var,Value,Args1),
	Term1=..[Functor|Args1].

% replace_all(L,Var,Value,L1) applies replace/4 to all the elements
% of the list L.
replace_all([],_,_,[]).
replace_all([Arg|Rest],Var,Value,[Arg1|Rest1]):-
	replace(Arg,Var,Value,Arg1),
	replace_all(Rest,Var,Value,Rest1).

% replace_list(LVars,LValues,Term1,Term2) returns in Term2 the same term
% Term1 but substituting each variable Vi from the position i of LVars
% by the value Valuei from the position i of LValues
replace_list([],[],Term,Term).
replace_list([Var|LVars],[Value|LValues],Term1,Term2):-
	var(Var),!,
	replace(Term1,Var,Value,TermAux),
	replace_list(LVars,LValues,TermAux,Term2).
replace_list([_Var|LVars],[_Value|LValues],Term1,Term2):-
	replace_list(LVars,LValues,Term1,Term2).

% rm_fA(G1,G2) returns G2 that is G1 eliminating the universal 
% quantification of free variables
rm_fA(Term,Term):-
	ground(Term),!.
rm_fA(Term,Term):-
	var(Term),!.
rm_fA(fA(Var),Var):-
	var(Var),!. % It is not necessary to check it 
rm_fA(Term,Term1):-
	Term=..[Functor|Args],
	rm_fA_all(Args,Args1),
	Term1=..[Functor|Args1].

% rm_fA_all(Args,Args1) its a map using rm_fA/2 over the arguments of Args
rm_fA_all([],[]).
rm_fA_all([Arg|Args],[Arg1|Args1]):-
	rm_fA(Arg,Arg1),
	rm_fA_all(Args,Args1).

% negate_Dimp(Dimp,SolC) obtain in SolC a solution of negating Dimp.
negate_Dimp([D|_Dimp],[(T1=T2)]):-
	replace(D,fA(X),X,dist(T1,T2)). %% Quantif's change at the negation
negate_Dimp([D|Dimp],[D|SolC]):-
	negate_Dimp(Dimp,SolC).

% negate_Rimp(Rimp,SolC) obtain in SolC a solution of negating Rimp.
negate_Rimp([R|_Rimp],[cneg(R)]).
negate_Rimp([R|Rimp],[R|SolC]):-
	negate_Rimp(Rimp,SolC).

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_Dexp_Rexp([],_,_,_):- !,fail.
negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,[cneg_aux(DR,ImpVars)]):-
	replace_free_vars(ExpVars,DRexp,DRList),
	get_conjunction(DRList,DR).

% get_conjunction(List,Conj) obtain the conjunction Conj with the
% same elements that List
get_conjunction([X],X).
get_conjunction([X|L],(X,C)):-
	get_conjunction(L,C).

% combine(LSolutions) combines all its elements that are the solutions
% of the negation of the conjunctions of the Frontier to obtain a 
% solution of the negation of the main Goal. The rest of solutions will
% be obtained by backtracking.
combine([]).
combine([Sol|Rest]):-
	call_all(Sol),
	combine(Rest).

% call_all(L) calls all the subgoals of the list L
call_all([]).
call_all([G|L]):-
%	varset(G,GVars),
%	copy_term(G,G1),
%	varset(G1,GVars1),
	call(G),
	call_all(L).

%%% If this gives us problems could be because of metaterms and them

%combine([Sol|Rest]):-
%	term_to_meta(Sol,MSol),
%	call(MSol),
%	combine(Rest).

% and at the begining:
% :- use_module(engine(internals),[term_to_meta/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cneg_tr contains predicates to obtain stored_clause/2 facts
% that we need to aply constructive negation
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
:- load_compilation_module(.('cneg_tr')).


% flat_dpred/3 adds a stored_clause/2 fact for each clause or
% fact from the program that includes package cneg.pl but
% it does it discontinously
%:- add_sentence_trans(flat_dpred/3).

% flat_cpred/3 adds a stored_clause/2 fact for each clause or
% fact from the program that includes package cneg.pl but
% it does it continously
:- add_sentence_trans(flat_cpred/3).

