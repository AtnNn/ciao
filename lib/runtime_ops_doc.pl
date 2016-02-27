:- use_package([assertions]).
:- comment(nodoc,assertions).

:- comment(title,"Enabling operators at run-time").

:- comment(author, "Daniel Cabeza").

:- comment(module,"This library package allows the use of the statically
   defined operators of a module for the reading performed at run-time
   by the program that uses the module. Simply by using this package the 
   operator definitions appearing in the module are enabled during the 
   execution of the program.").

:- include(library(runtime_ops)).

:- comment(version_maintenance,dir('../version/')).

:- comment(version(1*11+80,2003/12/20,14:33*04+'CET'), "Added comment
   author.  (Edison Mera)").

:- comment(version(1*7+140,2001/11/12,17:27*51+'CET'), "Added
   documentation file.  (Francisco Bueno Carrillo)").

