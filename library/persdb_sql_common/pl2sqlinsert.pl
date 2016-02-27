:- module(pl2sqlinsert,[pl2sqlInsert/2]).
:- include(library(assertions)).
:- use_module(library(lists),[append/3]).

:- multifile [relation/3,attribute/4].
:- data [relation/3,attribute/4].

pl2sqlInsert(ConstantTuple,SQLInsertString):-
	ConstantTuple=..[PredName|AttrValues],
	constants_list(AttrValues,AttrValues),
	%% all elements must be constant to be inserted
        relation(PredName,_Arity,TableName),
	attributes_list(TableName,AList),
	sqlInsertString(TableName,AList,AttrValues,SQLInsertString).

sqlInsertString(TableName,AttrList,AttrValues,InsertString) :-
	atom_codes(TableName,TableString),
	stringsList2stringEnumeration(AttrList,AttributesString),
	betweenBrackets(AttributesString,AttributesTuple),
	append(TableString," ",TabStrSp),
	append(TabStrSp,AttributesTuple,StringAfterInto),
	append("INSERT INTO ",StringAfterInto,IntoString),
	valuesList2stringEnumeration(AttrValues,StringAfterValues),
	betweenBrackets(StringAfterValues,ValuesTuple),
	append(ValuesTuple,";",ValStr),   %% SQL sentence ended by ';'
	append(" VALUES ",ValStr,ValuesString),
	append(IntoString,ValuesString,InsertString).

stringsList2stringEnumeration([],"").
stringsList2stringEnumeration([Str],Str):-
	!.
stringsList2stringEnumeration([Str1|Rest],NewStr):-
	stringsList2stringEnumeration(Rest,Str),
	append(Str1,",",Str_Comma),
	append(Str_Comma,Str,NewStr).

valuesList2stringEnumeration([],"").
valuesList2stringEnumeration([Value],String):-
	number(Value),
	!,
	number_codes(Value,String).
valuesList2stringEnumeration([Value],String):- 
	atom(Value),
	!,
	atom_codes(Value,StringValue),
	betweenApostrophes(StringValue,String).
	
valuesList2stringEnumeration([Val1|Rest],NewStr):-
	valuesList2stringEnumeration([Val1],Str1),
	append(Str1,",",Str1_Comma),
	valuesList2stringEnumeration(Rest,Str),
	append(Str1_Comma,Str,NewStr).

betweenBrackets(Str,BrackStr):-
	append("(",Str,OpenStr),
	append(OpenStr,")",BrackStr).
betweenApostrophes(Str,ApStr):-
	append("'",Str,Str1),
	append(Str1,"'",ApStr).

constants_list([],[]).
constants_list([Head|Tail],[Head|CLTail]):-
%%	atom(Head),!,
	nonvar(Head),!,
	constants_list(Tail,CLTail).
constants_list([_Head|Tail],CLTail):-
	constants_list(Tail,CLTail).

attributes_list(TableName,[]):-
	relation(_PredName,0,TableName),!.
attributes_list(TableName,List):-
	relation(_PredName,Arity,TableName), %% Arity is >0
	attrs_list(TableName,0,Arity,List).

attrs_list(_TableName,Arity,Arity,[]):-
	!.
attrs_list(TableName,Location,Arity,[AttStringName|List]) :-
	LocationPlus1 is Location+1,
	attribute(LocationPlus1,TableName,AttName,_AttType),
	atom_codes(AttName,AttStringName),
	attrs_list(TableName,LocationPlus1,Arity,List).

%% The version comment(s) below can be moved elsewhere in the file.
%% Subsequent comments will be placed above the last one inserted.
%% Note that the "assertions" library needs to be included in order
%% to support the ":- comment(_,_)." declarations.

:- comment(version(0*1+0,1998/07/09,18:10*22+'MET DST'), "Translate an
   external Prolog predicate with constant arguments, into a SQL
   insertion sentence. Tables will be defined using relation/3,
   attribute/4. Outstanding: errors checking (Ignacio Caballero
   Blanco)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:





