
:- use_package([assertions,iso]).
:- comment(nodoc,assertions).

:- comment(filetype,package).

:- comment(title,"ISO-Prolog package").

:- comment(author, "The CLIP Group").

:- comment(module,"This library package allows the use of the ISO-Prolog
   predicates in Ciao programs. It is included by default in modules
   starting with a @decl{module/2} declaration or user files without a
   starting @decl{use_package/1} declaration.").


%% *** Delete this comment after reading: it is only a reminder! ***
%% 
%% The "assertions" library needs to be included in order to support
%% ":- comment(...,...)." declarations such as below, i.e., insert: 
%% 
%% :- module(_,_,[assertions]).
%% 
%% At the beginning of the file:
%% The following version comment(s) can be moved elsewhere in the 
%% file. Subsequent version comments will always be placed above 
%% the last one inserted.


:- comment(version_maintenance,dir('../version')).

:- comment(version(1*11+74,2003/12/19,17:01*05+'CET'), "Added comment
   author.  (Edison Mera)").

