:- module(graphs,
	[ dgraph/1,
	  dlgraph/1,
	  dgraph_to_ugraph/2,
	  dlgraph_to_lgraph/2,
	  edges_to_ugraph/2,
	  edges_to_lgraph/2
	],
	[ assertions, basicmodes, regtypes ]).

:- use_module(library(sort)).
:- use_module(library('graphs/ugraphs'),
	[ ugraph/1,
	  vertices_edges_to_ugraph/3
	]).
:- use_module(library('graphs/lgraphs'),
	[ lgraph/2,
	  vertices_edges_to_lgraph/3
	]).

:- comment(title,"Graphs").

:- comment(author,"Francisco Bueno").

:- comment(module,"This module implements utilities for work with
   graphs").

% -------------------------------------------------------------------------

:- regtype dgraph(Graph)
	# "@var{Graph} is a directed graph.".
:- comment(dgraph(Graph),
	"A directed graph is a term @tt{graph(V,E)} where @tt{V} is a
	 list of vertices and @tt{E} is a list of edges (none necessarily
	 sorted). Edges are pairs of vertices which are directed, i.e.,
	 @tt{(a,b)} represents @tt{a->b}. Two vertices @tt{a} and @tt{b}
	 are equal only if @tt{a==b}.").

dgraph(graph(V,E)):-
	list(V),
	list(E,pair).

:- comment(doinclude,pair/1).
:- regtype pair(P) # "@var{P} is a pair @tt{(_,_)}.".

pair((_,_)).

:- regtype dlgraph(Graph)
	# "@var{Graph} is a directed labeled graph.".
:- comment(dlgraph(Graph),
	"A labeled directed graph is a directed graph
	 where edges are triples of the form @tt{(a,l,b)} where @tt{l}
	 is the label of the edge @tt{(a,b)}.").

dlgraph(graph(V,E)):-
	list(V),
	list(E,triple).

:- comment(doinclude,triple/1).
:- regtype triple(P) # "@var{P} is a triple @tt{(_,_,_)}.".

triple((_,_,_)).

% -------------------------------------------------------------------------

:- pred dgraph_to_ugraph(+Graph,-UGraph)
	: dgraph * var => dgraph * ugraph
        # "Converts @var{Graph} to @var{UGraph}.".

dgraph_to_ugraph(graph(V,E),UG):-
	direct_edges(E,UE),
	vertices_edges_to_ugraph(V,UE,UG).

direct_edges([(V1,V2)|E],[V1-V2|DE]):-
	direct_edges(E,DE).
direct_edges([],[]).

:- pred edges_to_ugraph(+Edges,-UGraph)
	: list(pair) * var => list(pair) * ugraph
        # "Converts @var{Graph} to @var{UGraph}.".

edges_to_ugraph(E,UG):-
	direct_edges_v(E,UE,V0),
	sort(V0,V),
	vertices_edges_to_ugraph(V,UE,UG).

direct_edges_v([(V1,V2)|E],[V1-V2|DE],[V1,V2|Vs]):-
	direct_edges_v(E,DE,Vs).
direct_edges_v([],[],[]).

% -------------------------------------------------------------------------

:- pred dlgraph_to_lgraph(+Graph,-LGraph)
	: dlgraph * var => dlgraph * lgraph(term)
        # "Converts @var{Edges} to @var{LGraph}.".

dlgraph_to_lgraph(graph(V,E),WG):-
	direct_ledges(E,WE),
	vertices_edges_to_lgraph(V,WE,WG).

direct_ledges([(V1,L,V2)|E],[V1-(V2-L)|DE]):-
	direct_ledges(E,DE).
direct_ledges([],[]).

:- pred edges_to_lgraph(+Edges,-LGraph)
	: list(triple) * var => list(triple) * lgraph(term)
        # "Converts @var{Edges} to @var{LGraph}.".

edges_to_lgraph(E,UG):-
	direct_ledges_v(E,UE,V0),
	sort(V0,V),
	vertices_edges_to_lgraph(V,UE,UG).

direct_ledges_v([(V1,L,V2)|E],[V1-(V2-L)|DE],[V1,V2|Vs]):-
	direct_ledges_v(E,DE,Vs).
direct_ledges_v([],[],[]).


% -------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*11+114,2003/12/22,18:25*48+'CET'), "Added comment
   module.  (Edison Mera)").

:- comment(version(1*5+83,2000/03/23,19:23*58+'CET'), "Added
   @lib{graphs} to the distribution (was left out by mistake).
   (Manuel Hermenegildo)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global Ciao version.  (Manuel Hermenegildo)").

% -------------------------------------------------------------------------
