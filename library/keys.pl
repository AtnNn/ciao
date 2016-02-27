
:- module(keys,
	[ keys_and_values/3,
	  keys_and_values/4,
	  key_lookup/4
	],
	[ assertions
	] ).

keys_and_values([K|Ks],[V|Vs],[K=V|KVs]):-
	keys_and_values(Ks,Vs,KVs).
keys_and_values([],[],[]).

keys_and_values([K|Ks],[V|Vs],[K=V|KVs],KVs0):-
	keys_and_values(Ks,Vs,KVs,KVs0).
keys_and_values([],[],KVs,KVs).

key_lookup(Key,[K=V|Dic],Value,Rest):-
	compare(R,Key,K),
	key_lookup_unscrambled(R,Key,K,V,Dic,Value,Rest).
%key_lookup(_Key,[],_Value,_Rest).           %% not found

key_lookup_unscrambled(=,_Key,_Key,Value,Dic,Value,Dic).
%key_lookup(<,_Key,_K,_V,_Dic,_Value,_Rest). %% not found
key_lookup_unscrambled(>,Key,K,V,Dic,Value,[K=V|Rest]):-
	key_lookup(Key,Dic,Value,Rest).


:- comment(version(0*9+64,1999/04/29,12:25*58+'MEST'), "Changed name
of key_lookup/7 (MCL)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:

