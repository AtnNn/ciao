Sale este warning siempre para todos los predicados pero espero que
cuando haga bien la expansion del paquete deje de salir.

WARNING: Predicate proof_cneg:dist_list/1 undefined in module proof_cneg

&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


Con :

:- module(proof_cneg,_,[.(cneg)]).
  
%:- module(proof_cneg,[odd/1,not_odd_number/1],[cneg]).

:- use_module(dist,[dist/2]). 
...


Me sale:
?-   use_module('/home/susana/tesis/micodigo/ciao/cneg/version4/proof_cneg.pl').
{Compiling /home/susana/tesis/micodigo/ciao/cneg/version4/proof_cneg.pl
WARNING: (lns 101-105) Predicate (:-)/1 undefined in source
WARNING: (lns 101-105) Predicate dist:dist/2 undefined in source
}

yes

Mientras que si comento:
:- module(proof_cneg,_,[.(cneg)]).
  
%:- module(proof_cneg,[odd/1,not_odd_number/1],[cneg]).

%:- use_module(dist,[dist/2]). 

Me sale:
?-   use_module('/home/susana/tesis/micodigo/ciao/cneg/version4/proof_cneg.pl').
{Compiling /home/susana/tesis/micodigo/ciao/cneg/version4/proof_cneg.pl
WARNING: (lns 101-105) Predicate dist:dist/2 undefined in source
}

yes

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
9/2/2002

?- no_queens(s(0),Qs).

Ciao interruption (h for help)? a
{ Execution aborted }


