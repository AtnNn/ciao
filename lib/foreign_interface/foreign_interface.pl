:- use_package([assertions,regtypes]).
:- comment(nodoc,assertions).
:- comment(nodoc,regtypes).

:- comment(use_foreign_source(Files),"@var{Files} is the (list of) foreign
	file(s) that will be linked with the glue-code file."). 
:- new_declaration(use_foreign_source/1,on).

:- comment(use_foreign_source(OsArch,Files),"@var{Files} are the OS and
	architecture dependant foreign files.").
:- new_declaration(use_foreign_source/2,on).

:- comment(use_foreign_library(Libs),"@var{Libs} is the (list of) librari(es)
	needed for the linkage.").
:- new_declaration(use_foreign_library/1,on).

:- comment(use_foreign_library(OsArch,Libs),"@var{Libs} are the OS and
	architecture dependant libraries.").
:- new_declaration(use_foreign_library/2,on).

:- comment(extra_compiler_opts(Opts),"@var{Opts} is the list of additional
        compiler options that will be used during the compilation.").
:- new_declaration(extra_compiler_opts/1,on).

:- comment(extra_compiler_opts(OsArch,Opts),"@var{Opts} are the OS and
	architecture dependant additional compiler options.").
:- new_declaration(extra_compiler_opts/2,on).

:- comment(extra_linker_opts(Opts),"@var{Opts} is the list of additional
        linker options that will be used during the linkage.").
:- new_declaration(extra_linker_opts/1,on).

:- comment(extra_linker_opts(OsArch,Opts),"@var{Opts} are the OS and
	architecture dependant additional linker options.").
:- new_declaration(extra_linker_opts/2,on).


:- regtype address(Address) # "@var{Address} is a valid memory address.".

address('$address'(Address)) :-
	int(Address).

:- regtype byte(Byte) # "@var{Byte} is a byte".

byte(Byte) :- int(Byte).

:- regtype byte_list(List)
 # "@var{List} is a list of bytes.".

byte_list(List) :- list(List,byte).

:- regtype int_list(List)
 # "@var{List} is a list of integers.".

int_list(List) :- list(List,int).


:- prop size_of(Name,ListVar,SizeVar)
 # "For predicate @var{Name}, the size of the argument of type
    @regtype{byte_list/1}, @var{ListVar}, is given by the argument of type
    integer @var{SizeVar}.".

size_of(_,_,_).

:- prop do_not_free(Name,Var)
 # "For predicate @var{Name}, the C argument passed to (returned from) the
    foreign function will not be freed after calling the foreign function.".

do_not_free(_,_).

:- prop returns(Name,Var)
 # "The result of the foreign function that implements the Prolog predicate
    @pred{Name} is unified with the Prolog variable @var{Var}. Cannot be
    used without @prop{foreign/1} or @prop{foreign/2}.".

returns(_,_).

:- push_prolog_flag(multi_arity_warnings,off).

:- prop foreign(Name)
 # "The Prolog predicate @pred{Name} is implemented using the foreign
    function @code{Name}.".

foreign(_).

:- prop foreign(PrologName,ForeignName)
 # "The Prolog predicate @pred{PrologName} is implemented using the foreign
    function @code{ForeignName}.".

foreign(_,_).

:- prop native(Name)
 # "The Prolog predicate @pred{Name} is implemented using the native
    function @code{Name}.".

native(_).

:- prop native(PrologName,ForeignName)
 # "The Prolog predicate @pred{PrologName} is implemented using the native
    function prolog_@code{ForeignName}.".

native(_,_).

:- pop_prolog_flag(multi_arity_warnings).



:- comment(version_maintenance,dir('../../version/')).


%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*5+137,2000/05/10,11:29*26+'CEST'), "Added
   int_list as external type.  (jfran)").

