:- module(foreign_interface_properties, [
	int_list/1,
	byte_list/1,
	byte/1,
	null/1,
	address/1,
	any_term/1,
	native/1,
	native/2,
	size_of/3,
	foreign/1,
	foreign/2,
	returns/2,
	do_not_free/2
	], [assertions,regtypes]).

:- push_prolog_flag(multi_arity_warnings,off).

:- comment(title, "Foreign Language Interface Properties").

:- comment(module, "The foreign language interface uses some
   properties to specify linking regimes, foreign files to be
   compiled, types of data available, memory allocation policies,
   etc.").

:- comment(summary, "The foreign language interface uses some
   properties to specify linking regimes, foreign files to be
   compiled, types of data available, memory allocation policies, etc.
   These are all the properties available and a brief explanation of
   their meaning.  Some of them can depend on the operating system and
   architecture, and can be selected stating it with an atom which
   represents that.  See @ref{Foreign Language Interface Guidelines
   and Usage} for a longer explanation and some examples.").

:- comment(author,"Jose Morales").
:- comment(author,"Manuel Carro").

:- comment(doinclude,use_foreign_source/1).
:- true decl use_foreign_source(Files) : atm_or_atm_list
        # "@var{Files} is the (list of) foreign
	file(s) that will be linked with the glue-code file.". 

:- comment(doinclude,use_foreign_source/2).
:- true decl use_foreign_source(OsArch, Files) : atm * atm_or_atm_list
        # "@var{Files} are the OS and architecture dependant foreign files.
          This allows compiling and linking different files depending on the 
          O.S. and architecture.".

:- comment(doinclude,use_foreign_library/1).
:- true decl use_foreign_library(Libs) : atm_or_atm_list
        # "@var{Libs} is the (list of) external library(es) needed to link 
          the C files.  Only the short name of the library (i.e., what would 
          follow the @tt{-l} in the linker is needed.".

:- comment(doinclude,use_foreign_library/2).
:- true decl use_foreign_library(OsArch,Libs) : atm * atm_or_atm_list
        # "@var{Libs} are the OS and
	architecture dependant libraries.".

:- comment(doinclude,extra_compiler_opts/1).
:- true decl extra_compiler_opts(Opts) : atm_or_atm_list
        # "@var{Opts} is the list of additional compiler options 
          (e.g., optimization options) that will be used during the 
          compilation.".  

:- comment(doinclude,extra_compiler_opts/2).
:- true decl extra_compiler_opts(OsArch,Opts) : atm * atm_or_atm_list
        # "@var{Opts} are the OS and architecture dependant additional
          compiler options.".

:- comment(doinclude,use_compiler/1).
:- true decl use_compiler(Compiler) : atm # "@var{Compiler} is the
compiler to use in this file.  When this option is used, the default
(Ciao-provided) compiler options are not used; those specified in
@pred{extra_compiler_options} are used instead.".

:- comment(doinclude,use_compiler/2).

:- true decl use_compiler(OsArch, Compiler) : atm * atm #
"@var{Compiler} is the compiler to use in this file when compiling for
the architecture @var{OsArch}.  The option management is the same as
in @pred{use_compiler/2}.".

:- comment(doinclude,extra_linker_opts/1).
:- true decl extra_linker_opts(Opts) : atm_or_atm_list
        # "@var{Opts} is the list of additional linker options that will be 
          used during the linkage.".

:- comment(doinclude,extra_linker_opts/2).
:- true decl extra_linker_opts(OsArch,Opts) : atm * atm_or_atm_list
        # "@var{Opts} are the OS and architecture dependant additional linker
          options.".

:- comment(doinclude,use_linker/1).
:- true decl use_linker(Linker) : atm
        # "@var{Linker} is the linker to use in this file. When this option is used, the default
(Ciao-provided) linker options are not used; those specified in
@pred{extra_linker_options/1} are used instead.".

:- comment(doinclude,use_linker/2).
:- true decl use_linker(OsArch, Linker) : atm * atm
        # "@var{Compiler} is the linker to use in this file when compiling for the architecture @var{OsArch}.   The option management is the same as
in @pred{use_compiler/2}.". 


:- regtype any_term(X) # "@var{X} is any term. The foreign interface passes it to C functions as a general term.".

any_term(_).

:- regtype address(Address) # "@var{Address} is a memory address.".

address('$address'(Address)) :-
	int(Address).

:- regtype null(Address) # "@var{Address} is a null adress.".

null('$address'(0)).

:- regtype byte(Byte) # "@var{Byte} is a byte.".

byte(Byte) :- 
        int(Byte), 
        0 =< Byte,
        Byte =< 255.


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


:- prop ttr(Name,Var,TTr)
 # "For predicate @var{Name}, the C argument will be translated ussing @tt{TTr}
    as term translator.".

ttr(_,_,_).


:- prop returns(Name,Var)
 # "The result of the foreign function that implements the Prolog predicate
    @pred{Name} is unified with the Prolog variable @var{Var}. Cannot be
    used without @prop{foreign/1} or @prop{foreign/2}.".

returns(_,_).


:- prop needs_state(Name)
 # "The foreign function which implementes the predicate @pred{Name} needs 
    a @tt{ciao_state} as its first argument.".

needs_state(_).


:- prop foreign(Name)
 # "The Prolog predicate @pred{Name} is implemented using the foreign
    function @tt{Name}.".

foreign(_).


:- prop foreign(PrologName,ForeignName)
 # "The Prolog predicate @pred{PrologName} is implemented using the foreign
    function @tt{ForeignName}.".

foreign(_,_).


:- prop native(Name) # "The Prolog predicate @pred{Name} is
implemented using the function @tt{Name}.  The implementation is not
a common C one, but it accesses directly the internal Ciao Prolog data
structures and functions, and therefore no glue code is generated for
it.".

native(_).


:- prop native(PrologName,ForeignName) # "The Prolog predicate
@pred{PrologName} is implemented using the function
@tt{ForeignName}.  The same considerations as above example
are to be applied.".

native(_,_).


:- pop_prolog_flag(multi_arity_warnings).



:- comment(version_maintenance,dir('../../version/')).


%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*9+229,2003/12/22,17:34*59+'CET'), "Added comment
   module. (Edison Mera)").

:- comment(version(1*7+77,2001/03/26,18:45*31+'CEST'), "Improved
   documentation (MCL)").

:- comment(version(1*7+64,2001/03/05,12:54*14+'MET'), "Added title and
   summary. (Manuel Carro)").

:- comment(version(1*5+137,2000/05/10,11:29*26+'CEST'), "Added
   int_list as external type.  (jfran)").

:- comment(bug, "The @tt{size_of/3} property has an empty definition").
