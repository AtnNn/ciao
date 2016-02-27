:- module(pl2sqlinsert,[pl2sqlInsert/2]).
:- include(library(assertions)).
:- use_module(library(lists),[append/3]).

:- multifile [sql__relation/3,sql__attribute/4].
:- data [sql__relation/3,sql__attribute/4].

pl2sqlInsert(ConstantTuple,SQLInsertString):-
	ConstantTuple=..[PredName|AttrValues],
	constants_list(AttrValues,AttrValues),
	%% all elements must be constant to be inserted
        sql__relation(PredName,_Arity,TableName),
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
%	append("(",Str,OpenStr),
	append([0'(|Str],")",BrackStr).
betweenApostrophes(Str,ApStr):-
	replaceEscapeSeqs(Str,Str0),
%	append("'",Str0,Str1),
	append([0''|Str0],"'",ApStr).

replaceEscapeSeqs([],[]).
replaceEscapeSeqs([0'\\,0''|Xs],[0'\\,0''|Ys]):- % Do not escape it if it is
	replaceEscapeSeqs(Xs,Ys), !.           % already escaped.
replaceEscapeSeqs([0'\\,0'"|Xs],[0'\\,0'"|Ys]):- % Do not escape it if it is
	replaceEscapeSeqs(Xs,Ys), !.           % already escaped.
replaceEscapeSeqs([0'\\,0'\\|Xs],[0'\\,0'\\|Ys]):- % Do not escape it if it is
	replaceEscapeSeqs(Xs,Ys), !.           % already escaped.
replaceEscapeSeqs([0''|Xs],[0'\\,0''|Ys]):-
	replaceEscapeSeqs(Xs,Ys), !.
replaceEscapeSeqs([0'"|Xs],[0'\\,0'"|Ys]):-
	replaceEscapeSeqs(Xs,Ys), !.
replaceEscapeSeqs([0'\\|Xs],[0'\\,0'\\|Ys]):-
	replaceEscapeSeqs(Xs,Ys), !.
replaceEscapeSeqs([X|Xs],[X|Ys]):-
	replaceEscapeSeqs(Xs,Ys).

constants_list([],[]).
constants_list([Head|Tail],[Head|CLTail]):-
%%	atom(Head),!,
	nonvar(Head),!,
	constants_list(Tail,CLTail).
constants_list([_Head|Tail],CLTail):-
	constants_list(Tail,CLTail).

attributes_list(TableName,[]):-
	sql__relation(_PredName,0,TableName),!.
attributes_list(TableName,List):-
	sql__relation(_PredName,Arity,TableName), %% Arity is >0
	attrs_list(TableName,0,Arity,List).

attrs_list(_TableName,Arity,Arity,[]):-
	!.
attrs_list(TableName,Location,Arity,[AttStringName|List]) :-
	LocationPlus1 is Location+1,
	sql__attribute(LocationPlus1,TableName,AttName,_AttType),
	atom_codes(AttName,AttStringName),
	attrs_list(TableName,LocationPlus1,Arity,List).

%% ---------------------------------------------------------------------------
:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*9+345,2004/05/04,15:31*00+'CEST'), "Added checks
   for apostrophes/quotes in string values (they must be \'ed).
   (Jesus Correas Fernandez)").

:- comment(version(1*9+115,2003/11/27,23:50*45+'CET'), "Names of
   multifile predicates relation/3 and attribute/4 changed to
   sql__relation/3 and sql__attribute/4.  (Jesus Correas Fernandez)").

:- comment(version(0*1+0,1998/07/09,18:10*22+'MET DST'), "Translate an
   external Prolog predicate with constant arguments, into a SQL
   insertion sentence. Tables will be defined using relation/3,
   attribute/4. Outstanding: errors checking (Ignacio Caballero
   Blanco)").

