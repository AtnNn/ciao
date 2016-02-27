:- module(foreign_interface,
	[build_foreign_interface/1,
	 rebuild_foreign_interface/1,
	 build_foreign_interface_explicit_decls/2,
	 build_foreign_interface_object/1,
	 rebuild_foreign_interface_object/1,
	 do_interface/1
	],
	[assertions,
	 basicmodes
	]).

:- comment(title,"Foreign interface").

:- comment(author,"Jose Morales").
:- comment(author,"Manuel Carro").

:- comment(summary, "This module provides predicates for building
	automatically the shared object necessary for accessing C
	functions as Ciao Prolog predicates. Regular C functions do
	not have their data in the internal format required by Ciao
	Prolog, and thus an intermediate translation is
	necessary. Besides, run-time errors (such as wrong
	instantiation states) must be handled and, finally, C code
	must be compiled into a dynamically loadable object code form,
	which is OS-dependent. All the corresponding details are
	automatically handled by this module, which takes a Ciao
	Prolog module skeleton, defining:

	  @begin{itemize}
          @item A set of C files
          @item A definition of types and modes for the Ciao Prolog
                predicates
          @item An (optional) set of flags required by the compilation of the
                files
          @end{itemize}

        and produces a dynamically loadable object file.
").

:- comment(module, "This module exports two predicates,
   @pred{build_foreign_interface/1} and
   @pred{rebuild_foreign_interface/1}. These predicates know about the
   correspondence between C types and Ciao Prolog types through the 
   assertions documenting them.

   Each predicate implemented as a foreign C function must appear in the
   module as:

   @begin{verbatim} 
     :- true pred prolog_predicate(m1(Arg1), ... mN(ArgN)) :: type1 *
          ... * typeN + (foreign(foreign_function_name), returns(ArgR)).

     :- impl_defined([..., prolog_predicate/N, ...]).
   @end{verbatim}

     where @tt{m1}, ..., @tt{mN} and @tt{type1}, ..., @tt{typeN} are 
     respectively the modes and types of the arguments.

     @pred{prolog_predicate/N} is implemented by @code{foreign_function_name},
     whose result is unified with @tt{ArgR}.

     This notation can be simplified if the name of the foreign function is
     the same as the name of the Ciao Prolog predicate, replacing
     @tt{foreign(foreign_function)} with just @tt{foreign}. @tt{returns(ArgR)}
     specifies that the result of the function corresponds to the @tt{ArgR}
     argument of the Ciao Prolog predicate. If the foreign function does not
     return anything, then @tt{returns(ArgR)} must be removed. Note that
     @tt{returns} cannot be used without @tt{foreign}.

   @begin{verbatim} 
     :- true pred prolog_predicate(m1(Arg1), ... mN(ArgN)) :: type1 *
          ... * typeN + foreign.
   @end{verbatim}

@section{Equivalence between Ciao Prolog and C modes}
   
   (This section has not been written yet)

@section{Equivalence between Ciao Prolog and C types}

   The translation between Ciao Prolog and C types is defined only for some
   simple types. The translation to be performed is solely defined by types
   of the arguments in the Ciao Prolog predicate, obeying the table below:

   @begin{itemize}
   @item num <-> double
   @item int <-> int
   @item atm <-> char *
   @item string <-> char * (with trailing zero)
   @item byte_list <-> char * (a buffer, with associated length)
   @item address <-> void *
   @end{itemize}

   Strings, atoms, and lists of bytes are passed to (and from) C as
   dynamically (malloc) created arrays of characters (bytes). Those arrays
   are freed by Ciao Prolog upon return of the foreign function unless
   @prop{do_not_free/2} is specified (see examples below).

   Empty lists of bytes are converted into @tt{NULL}, and strings @tt{[]} or
   atoms '' are converted into @em{\"\"} (a zero ended C string).
   @em{NULL} is converted into an empty list or the @em{''} atom.

   @tt{NULL} strings and empty buffers are converted into the empty list or
   the null atom (@tt{''}). Empty Ciao Prolog strings (@tt{""} or @tt{[]})
   and null atoms are converted into zero-terminated strings of zero
   length. Empty Ciao Prolog lists are converted into @tt{NULL}.

   The type @regtype{byte_list/1} requires an additional property,
   @prop{size_of/3}, to indicate which argument represents its size.

@section{Examples}

@subsection{Mathematical functions}

@em{math.pl}

@begin{verbatim} 
:- module(math,
	[sin/2,
	 cos/2,
	 fabs/2
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 'foreign_interface/syntax'
	]).

:- true pred sin(in(X),go(Y)) :: num * num + (foreign,returns(Y)).
:- true pred cos(in(X),go(Y)) :: num * num + (foreign,returns(Y)).
:- true pred fabs(in(X),go(Y)) :: num * num + (foreign,returns(Y)).

:- extra_compiler_opts(['-O2']).
:- extra_compiler_opts('LINUXi86',['-ffast-math']).
:- use_foreign_library([m]).

:- impl_defined([sin/2,cos/2,fabs/2]).
@end{verbatim}

@subsection{Addresses and C pointers}

@em{objects.pl}

@begin{verbatim} 
:- module(objects,
	[object/2,
	 show_object/1
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 'foreign_interface/syntax'
	]).
	 
:- true pred object(in(N),go(Object)) :: int * address +
	(foreign,returns(Object)).

:- true pred show_object(in(Object)) :: address + foreign.

:- use_foreign_source(objects_c).
:- extra_compiler_opts('-O2').

:- impl_defined([object/2,show_object/1]).
@end{verbatim}

@em{objects_c.c}


@begin{verbatim} 
#include <stdio.h>

struct object @{
  char *name;
  char *colour;
@};

#define OBJECTS 3

struct object objects[OBJECTS] =
@{ @{\"ring\",\"golden\"@},
  @{\"table\",\"brown\"@},
  @{\"bottle\",\"green\"@} @};

struct object *object(int n) @{
  return &objects[n % OBJECTS];
@}

void show_object(struct object *o) @{
  printf(\"I show you a %s %s\\n\", o->colour, o->name);
@}
@end{verbatim}

@subsection{Lists of bytes and buffers}

@em{byte_lists.pl}

@begin{verbatim} 
:- module(byte_lists,
	[obtain_list/3,
	 show_list/2
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 'foreign_interface/syntax'
	]).
	 
:- true pred obtain_list(in(N),go(Length),go(List)) :: int * int * byte_list
	+ (foreign,size_of(List,Length)).
:- true pred show_list(in(Length),in(List)) :: int * byte_list
	+ (foreign,size_of(List,Length)).

:- use_foreign_source(bytes_op).

:- impl_defined([obtain_list/3,show_list/2]).
@end{verbatim}

@em{bytes_op.c}

@begin{verbatim} 
#include <malloc.h>
#include <stdio.h>

void obtain_list(int n, int *l, char **s) @{
  int i;
  int c;
  if (n < 0) n = 0;
  *l = n;
  *s = (char *)malloc(*l);
  for (i = 0; i < *l; i++) @{
    (*s)[i] = i;
  @}
@}

void show_list(int l, char *s) @{
  if (s) @{
    int n;
    printf(\"From C: [\");
    for (n = 0; n < l; n++) @{
      printf(\" %d\", s[n]);
    @}
    printf(\"]\\n\");
  @} else @{
    printf(\"From C: []\\n\");
  @}
@}
@end{verbatim}

@subsection{Strings and atoms}

@em{strings_and_atoms.pl}

@begin{verbatim} 
:- module(strings_and_atoms,
	[lookup_string/2,
	 lookup_atom/2,
	 a_string/1,
	 show_string/1,
	 show_atom/1
	],
	[assertions,
	 basicmodes,
	 regtypes,
	 'foreign_interface/syntax'
	]).


:- true pred a_string(go(S)) ::
	string + (foreign(get_static_str),returns(S),do_not_free(S)).
	 
:- true pred lookup_string(in(N),go(S)) ::
	int * string + (foreign(get_str),returns(S)).
:- true pred lookup_atom(in(N),go(S)) ::
	int * atm + (foreign(get_str),returns(S)).

:- true pred show_string(in(S)) :: string + foreign(put_str).
:- true pred show_atom(in(S)) :: atm + foreign(put_str).

:- use_foreign_source(str_op).

:- impl_defined([lookup_string/2,lookup_atom/2,show_string/1,show_atom/1,
                 a_string/1]).
@end{verbatim}

@em{str_op.c}

@begin{verbatim} 
#include <malloc.h>
#include <stdio.h>

char *get_static_str() @{
  return \"this is a string Ciao Prolog should not free\";
@}

char *get_str(int n) @{
  char *s;
  int size;
  int i;
  int c;
  if (n < 0) n = -n;
  size = (n%4) + 5;
  s = (char *)malloc(size+1);
  for (i = 0, c = ((i + n) % ('z' - 'a' + 1)) + 'a'; i < size; i++,c++) @{
    if (c > 'z') c = 'a'; 
    s[i] = c;
  @}
  s[i] = 0;
  return s;
@}

void put_str(char *s) @{
  if (s) @{
    printf(\"From C: \\\"%s\\\"\\n\", s);
  @} else @{
    printf(\"From C: null\\n\");
  @}
@}
@end{verbatim}").


%% ---------------------------------------------------------------------------

:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(lists),
	[select/3, append/3, difference/3, contains1/2, nocontainsx/2,
         length/2, nonsingle/1]).
:- use_module(library(llists),[flatten/2]).
:- use_module(library(aggregates),[findall/3]).
:- use_module(library(system),
	[delete_file/1,system/2,modif_time0/2,file_exists/1]).
:- use_module(library(format),[format/2,format/3]).
:- use_module(library(messages),
	[error_message/1,error_message/2,error_message/3,
	 warning_message/2,warning_message/3]).
:- use_module(library('assertions/assrt_lib'),
	[get_code_and_related_assertions/5,
	 cleanup_code_and_related_assertions/0,
	 clause_read/7,
	 assertion_read/9]).
:- use_module(library(foreign_compilation),
	[compiler_and_opts/2,linker_and_opts/2]).
:- use_module(library('compiler/c_itf')).
:- use_module(library(ctrlcclean),[ctrlc_clean/1]).
:- use_module(library(errhandle)).  

:- set_prolog_flag(multi_arity_warnings,off).

%% ---------------------------------------------------------------------------

:- prop absname(File) + regtype
        # "@var{File} is an absolute filename with extension.".

absname(File) :- atm(File).

%% ---------------------------------------------------------------------------

:- pred build_foreign_interface(in(File)) :: sourcename
 # "Reads assertions from @var{File}, generates the gluecode for the Ciao
    Prolog interface, compiles the foreign files and the gluecode file, and
    links everything in a shared object. Checks modification times to
    determine automatically which files must be generated/compiled/linked.".

build_foreign_interface(File) :-
	get_decls(File,Decls),
	build_foreign_interface_explicit_decls(File,Decls).

%% ---------------------------------------------------------------------------

:- pred rebuild_foreign_interface(in(File)) :: sourcename
 # "Like @pred{build_foreign_interface/1}, but it does not check the 
    modification time of any file.".

rebuild_foreign_interface(File) :-
	get_decls(File,Decls),
	build_foreign_interface_explicit_decls(yes,File,Decls).

%% ---------------------------------------------------------------------------

:- pred build_foreign_interface_explicit_decls(in(File),in(Decls)) ::
	sourcename * list(term)
 # "Like @pred{build_foreign_interface/1}, but use declarations in @var{Decls}
    instead of reading the declarations from @var{File}.".

build_foreign_interface_explicit_decls(File,Decls) :-
	build_foreign_interface_explicit_decls(no,File,Decls).

%% ---------------------------------------------------------------------------

:- pred build_foreign_interface_explicit_decls(in(Rebuild),in(File),
	in(Decls)) ::
	atm * sourcename * list(term)
 # "Like @pred{build_foreign_interface_explicit_decls/1}, but it does not
    check the modification time of any file if @var{Rebuild} is @tt{yes}.".

build_foreign_interface_explicit_decls(Rebuild,File,Decls) :-
	( do_interface(Decls) ->
	    absolute_file_name(File,[],'.pl','.',PrologFile,Base,Dir),
	    generate_gluecode(Rebuild,Base,PrologFile),
	    compile_and_link(Rebuild,Dir,Base,Decls)
	; true
	).

%% ---------------------------------------------------------------------------

:- pred do_interface(in(Decls)) :: list(term) # "Given the declarations in
	@var{Decls}, this predicate succeeds if these declarations involve
        the creation of the foreign interface".

do_interface(Decls) :-
	contains1(Decls,use_foreign_library(_)), !.
do_interface(Decls) :-
	contains1(Decls,use_foreign_library(_,_)), !.
do_interface(Decls) :-
	contains1(Decls,use_foreign_source(_)), !.
do_interface(Decls) :-
	contains1(Decls,use_foreign_source(_,_)).

%% ---------------------------------------------------------------------------

:- pred get_decls(in(File),go(Decls)) :: sourcename * list(term)
	#"@var{Decls} is the list of declarations in @var{File}".	

get_decls(File,Decls) :-
	absolute_file_name(File,[],'.pl','.',PrologFile,Base,_),
        error_protect(ctrlc_clean(
		process_files_from(PrologFile,in,module,get_decls_2(Decls), 
                                   false,false,'='(Base)))).

get_decls_2(Base,Decls) :-
	findall(D,decl(Base,D),Decls).

%% ---------------------------------------------------------------------------

:- pred generate_gluecode(in(Rebuild),in(Base),in(PrologFile)) :: atm * atm *
	absfile
	# "Generates the gluecode for @var{PrologFile}.".

generate_gluecode(Rebuild,Base,PrologFile) :-
	get_gluecode_base(Base,CFileBase),
	atom_concat(CFileBase,'.c',CFile),
	has_changed(PrologFile,CFile),
	!,
	generate_gluecode_2(Rebuild,PrologFile,CFile).
generate_gluecode(_,_,_).

generate_gluecode_2(Rebuild,PrologFile,CFile) :-
	( Rebuild = yes ->
	    ensure_autogenerated(CFile,alert),
	    delete_files([CFile])
	; true
	),
	cleanup_code_and_related_assertions,
	get_code_and_related_assertions(PrologFile,Module,_,_,_),
	read_foreign_predicates(PrologFile,Predicates),
	( Predicates = [] ->
	    warning_message("no foreign predicate found in '~w'",[PrologFile])
	; true
	),
	generate_gluecode_file(PrologFile,Module,CFile,Predicates),
	cleanup_code_and_related_assertions,
	!.
generate_gluecode_2(_,_,CFile) :-
	cleanup_code_and_related_assertions,
	ensure_autogenerated(CFile,no_alert),
       	delete_files([CFile]),
	fail.

%% ---------------------------------------------------------------------------

:- pred get_gluecode_base(in(Base),go(CFileBase)) :: atm * atm
	# "@var{CFileBase} is the base name of the gluecode file.".

get_gluecode_base(Base,CFileBase) :-
	get_os_arch(OsArch),
	atom_concat([Base,'_',OsArch,'_glue'],CFileBase).

%% ---------------------------------------------------------------------------

:- pred has_changed(in(SourceFile),in(TargetFile)) ::
	absname * absname
 # "It succeeds if @var{SourceFile} is earlier than @var{TargetFile} or
    @var{TargetFile} does not exist. That means that @var{TargetFile} must
    be (re)built.".

has_changed(SourceFile,TargetFile) :-
	modif_time0(SourceFile,TS),
	modif_time0(TargetFile,TT),
	TS > TT.

%% ---------------------------------------------------------------------------

:- pred get_os_arch(go(OsArch)) :: atm
 # "@var{OsArch} is the name of the operating system and architecture 
    currently running.".

get_os_arch(OsArch) :-
	get_os(Os),
	get_arch(Arch),
	atom_concat(Os,Arch,OsArch).

%% ---------------------------------------------------------------------------

:- pred ensure_autogenerated(in(CFile),in(M)) :: absname * atm #
	"It succeeds if @var{CFile} does not exist or is headed by the
	autogenerated mark. If @var{M} is @tt{alert} then it shows an error
	message before failing.".

ensure_autogenerated(CFile,Alert) :-
	( file_exists(CFile) ->
	    open(CFile,read,Stream),
	    autogenerated_mark(H),
	    ( match_string(Stream,H) ->
	        true
	    ; ( Alert=alert ->
		  error_message("~w does not seem to be an " ||
			        "autogenerated file and it will not be" ||
				"overwritten",[CFile])
	      ; true
	      ),
	      Ok=no
	    ),
	    close(Stream),
	    Ok=yes
	; true
	).

%% ---------------------------------------------------------------------------

:- pred match_string(in(Stream),in(String)) :: stream * string # "Success
	if @var{String} is read from standard input.".

match_string(_,[]) :-
	!.
match_string(Stream,[Code|Codes]) :-
	get_code(Stream,Code),
	match_string(Stream,Codes).

%% ---------------------------------------------------------------------------

:- pred autogenerated_mark(go(String)) :: string # "@var{String} is the
	autogenerated mark.".

autogenerated_mark("\/\* Do not edit - this file is automatically generated\n"
	        || "   by foreign_inteface.pl ! \*\/\n").

%% ---------------------------------------------------------------------------

:- pred delete_files(in(Files)) :: list(atm)
 # "Delete the files in @var{Files}.".

delete_files([]) :- !.
delete_files([F|Fs]) :-
	( file_exists(F) ->
	    delete_file(F)
	; true
	),
	delete_files(Fs).

%% ---------------------------------------------------------------------------

:- pred read_foreign_predicates(in(PrologFile),go(Predicate)) ::
	absname * foreign_predicate_description
 # "It binds @var{Predicate} with the description of a foreign predicate
    declared in the assertions of the Ciao Prolog module @var{PrologFile}.
    You must read the assertions with @pred{get_code_and_related_assertions/5}
    before using this predicate.".

:- data foreign_predicate/1.
:- data foreign_predicate_error/0.

	
read_foreign_predicates(PrologFile,_) :-
        cleanup_foreign_predicates,

	% Reads the assertions.
	assertion_read(_,_,Status,_,Body,VarNames,PrologFile,LB,LE),

	% Extracts PrologName, Arity and ForeignName.
	Body = ::(Pr,=>(DP:CP,AP+GP#_)),
	functor(Pr,PrologName,Arity),
	PredName = PrologName/Arity,
	Pr =.. [_|Arguments],
	Loc = loc(PrologFile,LB,LE),

	% Get the foreign name. If no foreign name is found, fails (without
        % producing any error).
	get_foreign_name(Loc,PredName,GP,ForeignName),

	% Checks the correctness of the assertion.
	check_unique(Loc,PredName),
	check_all_arguments(Loc,PredName,Arguments,DP),
	check_all_arguments(Loc,PredName,Arguments,CP),
	check_all_arguments(Loc,PredName,Arguments,AP),
	get_returns(Loc,PredName,CP,GP,Arguments,VarNames,Res),
	check_byte_list_correctness(Loc,PredName,DP,GP,Arguments,VarNames),
	check_do_not_free_correctness(Loc,PredName,GP,Arguments),
	check_status(Loc,PredName,Status),

	% Fill the predicate's description structure.
	PredDescription = pred(PrologName/Arity,CName,ForeignName,
	                       In,Out,Res,SizeLinks,CallArgs,NoFree),
	atom_concat('prolog_',PrologName,CName),
	Arity1 is Arity - 1,
	numbers_between(0,Arity1,Arguments),
	findall(X,member(ground(X),CP),InN),
	difference(Arguments,InN,OutN),
	assign_types(Loc,PredName,VarNames,DP,OutN,Out),
	assign_types(Loc,PredName,VarNames,DP,InN,In),
	difference(Arguments,Res,CallArgsN),
	assign_modes(InN,CallArgsN,CallArgs),
	findall(X,(member(Y,GP),Y=size_of(_,A,B),X=size_of(A,B)),SizeLinks),
	findall(X,(member(Y,GP),Y=do_not_free(_,X)),NoFree),
	assertz_fact(foreign_predicate(PredDescription)),
	
        % Backtracks over the next predicate.
	fail. 
read_foreign_predicates(_,Predicates) :-
	findall(Predicate,foreign_predicate(Predicate),Predicates),
	cleanup_foreign_predicates,
	% Fails if error.
	\+ retract_fact(foreign_predicate_error).

cleanup_foreign_predicates :-
	retractall_fact(foreign_predicate(_)).

check_unique(Loc,PredName) :-
	foreign_predicate(pred(PredName,_,_,_,_,_,_,_,_)), !,
	error_message(Loc,"duplicated assertions for predicate ~w (foreign)",
	             [PredName]),
        fail.
check_unique(_,_).

check_all_arguments(_,_,_,[]) :- !.
check_all_arguments(Loc,PredName,Arguments,[X|_]) :-
	X =.. [_,Y],
	nocontainsx(Arguments,Y),
	!,
	error_message(Loc,"invalid argument name in predicate ~w",[PredName]),
        set_fact(foreign_predicate_error),
	fail.
check_all_arguments(Loc,PredName,Arguments,[_|Xs]) :-
	check_all_arguments(Loc,PredName,Arguments,Xs).

get_foreign_name(Loc,PredName,GP,ForeignName) :-
	select(foreign(_),GP,GP0), !,
	no_more_foreign_name(Loc,PredName,GP0),
	PredName = ForeignName/_.
get_foreign_name(Loc,PredName,GP,ForeignName) :-
	select(foreign(_,ForeignName),GP,GP0),
	valid_foreign_name(Loc,PredName,ForeignName),
	no_more_foreign_name(Loc,PredName,GP0).

valid_foreign_name(_,_,ForeignName) :-
	atom(ForeignName), !.
valid_foreign_name(Loc,PredName,_) :-
	error_message(Loc,"invalid foreign function name in predicate ~w",
                      [PredName]),
        set_fact(foreign_predicate_error),
	fail.

no_more_foreign_name(_,_,GP) :-
	\+ member(foreign(_),GP),
	\+ member(foreign(_,_),GP),
	!.
no_more_foreign_name(Loc,PredName,_) :-
	error_message(Loc,"more than one foreign/1 or foreign/2 property" ||
                      "in predicate ~w",[PredName]),
	set_fact(foreign_predicate_error),
	fail.

get_returns(Loc,PredName,CP,GP,Arguments,VarNames,[Argument]) :-
	select(returns(_,Argument),GP,GP0), !,
	valid_returns_argument(Loc,PredName,Arguments,Argument),
	no_more_returns(Loc,PredName,GP0),
	returns_in_output_arg(Loc,PredName,CP,VarNames,Argument).
get_returns(_,_,_,_,_,_,[]).

valid_returns_argument(Loc,PredName,Arguments,Argument) :-
	nocontainsx(Arguments,Argument),
	error_message(Loc,"returns/2 with invalid argument in predicate ~w",
                      [PredName]),
	set_fact(foreign_predicate_error),
	fail.
valid_returns_argument(_,_,_,_).

no_more_returns(_,_,GP) :-
	\+ member(returns(_,_),GP),
	!.
no_more_returns(Loc,PredName,_) :-
	error_message(Loc,"more than one returns/2 property in predicate ~w",
                      [PredName]),
        set_fact(foreign_predicate_error),
	fail.

returns_in_output_arg(_,_,CP,_,Argument) :-
	nocontainsx(CP,ground(Argument)),
	!.
returns_in_output_arg(Loc,PredName,_,VarNames,Argument) :-
	var_name(Argument,VarNames,VarName),
	error_message(Loc,"~w is not an output argument in predicate ~w",
	              [VarName,PredName]),
        set_fact(foreign_predicate_error),
	fail.		      

check_byte_list_correctness(Loc,PredName,DP,GP,Arguments,VarNames) :-
	one_byte_list_for_each_size_of(Loc,PredName,DP,GP,Arguments),
	one_size_of_for_each_byte_list(Loc,PredName,DP,GP,VarNames).

one_byte_list_for_each_size_of(Loc,PredName,DP,GP,Arguments) :-
	member(size_of(_,ListVar,SizeVar),GP),
	\+ valid_size_of_property(Arguments,ListVar,SizeVar,DP),
	!,
	error_message(Loc,"invalid size_of property in predicate ~w",
	              [PredName]),
        set_fact(foreign_predicate_error),
        fail.
one_byte_list_for_each_size_of(_,_,_,_,_).

valid_size_of_property(Arguments,ListVar,SizeVar,DP) :-
	\+ nocontainsx(Arguments,ListVar),
	\+ nocontainsx(Arguments,SizeVar),
	\+ nocontainsx(DP,byte_list(ListVar)),
	\+ nocontainsx(DP,int(SizeVar)).

one_size_of_for_each_byte_list(Loc,PredName,DP,GP,VarNames) :-
	member(byte_list(ListVar),DP),
	findall(Y,(member(size_of(_,Y,_),GP),Y==ListVar),S),
	nonsingle(S),
	!,
	var_name(ListVar,VarNames,VarName),
	error_message(Loc,"variable ~w in predicate ~w needs a (only one) " ||
		      "size_of/3 property",[VarName,PredName]),
        set_fact(foreign_predicate_error),
	fail.
one_size_of_for_each_byte_list(_,_,_,_,_).

var_name(Var,VarNames,Name) :-
	findall(N,(member(N=X,VarNames),X==Var),[Name]).

check_do_not_free_correctness(Loc,PredName,GP,Arguments) :-
	member(do_not_free(_,Var),GP),
	nocontainsx(Arguments,Var),
	!,
	error_message(Loc,"invalid do_not_free/2 property in predicate ~w",
                      [PredName]),
	fail.
check_do_not_free_correctness(_,_,_,_).

numbers_between(A,B,[]) :-
	A > B, !.
numbers_between(A,B,[A|Ns]) :-
	A1 is A + 1,
	numbers_between(A1,B,Ns).

assign_types(_,_,_,_,[],[]) :- !.
assign_types(Loc,PredName,VarNames,CP,[N|Ns],[arg_type(Type,N)|Types]) :-
	find_type(N,CP,Type),
	!,
	assign_types(Loc,PredName,VarNames,CP,Ns,Types).
assign_types(Loc,PredName,VarNames,_,[N|_],_) :-
	var_name(N,VarNames,VarName),
	error_message(Loc,"no valid type found for variable ~w in " ||
		     "predicate ~w",[VarName,PredName]),
        set_fact(foreign_predicate_error),
	fail.

find_type(N,[Prop|_],Type) :-
	Prop =.. [Type,N],
	type(Type,_,_,_,_,_), !.
find_type(N,[_|Props],Type) :-
	find_type(N,Props,Type).

assign_modes(_,[],[]) :- !.
assign_modes(InN,[N|Ns],[A|As]) :-
	( contains1(InN,N) ->
	    A = in(N)
	; A = out(N)
	),
	assign_modes(InN,Ns,As).

check_status(_,_,true) :- !.
check_status(_,_,trust) :- !.
check_status(Loc,PredName,_) :-
	warning_message(Loc,"assertions of predicate ~w cannot be checked " ||
		       "(foreign)",[PredName]).

%% ---------------------------------------------------------------------------

:- pred generate_gluecode_file(in(PrologFile),in(Module),in(CFile),
	in(Predicates))
 :: absname * atm * absname * list(foreign_predicate_description)
 # "Generates the @var{CFile} file from the assertions read in 
    @var{PrologFile}.".

generate_gluecode_file(PrologFile,Module,CFile,Predicates) :-
	ensure_autogenerated(CFile,alert),
	open(CFile,write,Stream),
	autogenerated_mark(Mark),
	format(Stream,"~s",[Mark]),
	( generate_includes(Stream),
	  generate_foreign_predicates_interface(Stream,Predicates),
	  generate_init(Stream,Module,Predicates),
	  generate_end(Stream,Module,Predicates) ->
	    Ok=yes
	; error_message("generation of the interface gluecode for " ||
		        "Prolog file '~w' failed", [PrologFile])
	),
	close(Stream),
	\+ Ok=no.

%% ---------------------------------------------------------------------------

:- pred type(PrologType,CType,PreProlog2C,Prolog2C,C2Prolog,CTypeFree) ::
	prolog_type * c_type * atm * atm * atm * atm 

# "For each Ciao Prolog type, @var{PrologType}, defines the names of
   the functions needed for generating the C interface code. The
   conversion from Prolog to C is split into two phases. Conversions
   that do not allocate memory pre-conversions tests will be performed
   during the first one. At this point, conversions should not
   fail. The second phase performs the rest of conversions and should
   be fail-safe. Note that the type @tt{byte_list} needs the previous
   conversion of the integer arguments because some of them represents
   the length of the lists.".

type(int,'long','GET_INTEGER','','MakeInteger','').
type(num,'double','GET_NUMBER','','MakeFloat','').
type(atm,'char *','','GET_CSTRING_FROM_ATOM','MAKE_ATOM','FREE').
type(string,'char *','STRING_TEST','GET_CSTRING_FROM_LIST','MAKE_STRING',
     'FREE').
type(byte_list,'char *','BYTES_TEST','GET_BYTES','MakeList','FREE').
type(address,'void *','GET_ADDRESS','','MakeAddress','').

%% ---------------------------------------------------------------------------

:- prop prolog_type(Type) + regtype
        # "@var{Type} is a Prolog type which has an equivalent C type".

prolog_type(Type) :- type(Type,_,_,_,_,_).

%% ---------------------------------------------------------------------------

:- prop c_type(Type) + regtype
        # "@var{Type} is a C type which has an equivalent Prolog type".

c_type(Type) :-	type(_,Type,_,_,_,_).

%% ---------------------------------------------------------------------------

:- prop arg_type(T) + regtype # "@var{T} is a structure which relates a Ciao
    Prolog type and the number of the argument with that type.  This invariant
    holds throughout the whole library, although the type itself does not
    enforce it.".

arg_type(arg_type(Type,N)) :-
	prolog_type(Type),
	int(N).

%% ---------------------------------------------------------------------------

:- prop arg_mode(M) + regtype
        # "@var{M} is either @tt{in(N)} or @tt{out(N)}, where N is the
          argument number.".

arg_mode(in(N)) :- int(N).
arg_mode(out(N)) :- int(N).

%% ---------------------------------------------------------------------------

:- prop size_link(L) + regtype
        # "@var{L} represents a size-list relation between two arguments.".

size_link(size_of(ListN,SizeN)) :-
	int(ListN),
	int(SizeN).

%% ---------------------------------------------------------------------------

:- prop foreign_predicate_description(P) + regtype
	# "@var{P} is a foreign predicate description.".

:- comment(foreign_predicate_description/1, "@begin{verbatim}
   pred(PrologNameAndArity,CName,ForeignName,In,Out,Res,SizeLinks,CallArgs,
   NoFree) @end{verbatim} is a foreign predicate description. 
   @var{PrologNameAndArity} is defined externaly by
   @var{ForeignName} and will be accessed from Ciao Prolog using
   @var{CName}. @var{In} is the type list of the input arguments. @var{Out}
   is the type list of the output arguments. @var{Res} is empty if the
   foreign function does not returns anything or a singleton list
   containing the number (index) of the returned argument. @var{CallArgs}
   is the mode list of the arguments that must be passed to the foreign
   function (without the return argument). @var{SizeLinks} is the list of
   relations size-list between the arguments.").

foreign_predicate_description(P) :-
	P = pred(PrologNameAndArity,CName,ForeignName,In,Out,Res,SizeLinks,
                 CallArgs,NoFree),
	predname(PrologNameAndArity),
	atom(CName),
	atom(ForeignName),
	list(arg_type,In),
	list(arg_type,Out),
	list(int,Res),
	list(size_link,SizeLinks),
	list(arg_mode,CallArgs),
	list(int,NoFree).

%% ---------------------------------------------------------------------------

:- pred generate_includes(in(Stream)) :: stream #
	"It writes the header of the foreign C file.".

generate_includes(Stream) :-
	include_base_dir(Dir),
	format(Stream,"#include \"~w/datadefs.h\"~n", [Dir]),
	format(Stream,"#include \"~w/support.h\"~n", [Dir]),
	absolute_file_name(library('foreign_interface/foreign_interface.h'),
                           [],'.h',Dir,_,ForeignInterfaceHBase,_),
	format(Stream,"#include \"~w.h\"~n",[ForeignInterfaceHBase]).

%% ---------------------------------------------------------------------------

:- pred include_base_dir(go(Dir)) :: atm
 # "@var{Dir} is the directory where the header files @tt{datadefs.h} and
    @tt{support.h} are found.".

include_base_dir(Dir) :-
	get_os_arch(OsArch),
	ciaolibdir(Dir0),
	atom_concat([Dir0,'/include/',OsArch],Dir).

%% ---------------------------------------------------------------------------

:- pred generate_foreign_predicates_interface(in(Stream),in(Predicates)) ::
	stream * list(foreign_predicate_description)
 # "Generates the C interface code for each predicate in @var{Predicates}.".

generate_foreign_predicates_interface(_,[]) :- !.
generate_foreign_predicates_interface(Stream,[P|Ps]) :-
	generate_foreign_predicate_interface(Stream,P),
	generate_foreign_predicates_interface(Stream,Ps).

%% ---------------------------------------------------------------------------

:- pred generate_foreign_predicate_interface(in(Stream),in(Predicate)) ::
	stream * foreign_predicate_description
 # "It generates the C interface code for the predicate @var{Predicate}.".
	
generate_foreign_predicate_interface(Stream,P) :-
	P = pred(_,CName,ForeignName,InVars,OutVars,ResVar,SizeLinks,
                 CallArgs,NoFreeVars),
	generate_foreign_prototype(Stream,ForeignName,InVars,OutVars,ResVar,
	                           CallArgs),
	generate_header(Stream,CName),
	generate_c_variables_declaration(Stream,InVars),
	generate_nonreg_c_variables_declaration(Stream,OutVars),
	generate_tagged_variables_declaration(Stream,OutVars),
	generate_pl2c_conversions(Stream,phase1,InVars),
	generate_pl2c_compound_conversions(Stream,phase1,InVars,SizeLinks),
	generate_pl2c_conversions(Stream,phase2,InVars),
	generate_pl2c_compound_conversions(Stream,phase2,InVars,SizeLinks),
	generate_call(Stream,ForeignName,CallArgs,ResVar),
	generate_c2pl_conversions(Stream,OutVars),
	generate_c2pl_compound_conversions(Stream,OutVars,SizeLinks),
	generate_freeings(Stream,InVars,NoFreeVars),
	generate_freeings(Stream,OutVars,NoFreeVars),
	generate_bindings(Stream,OutVars),
	generate_end_of_function(Stream).

%% ---------------------------------------------------------------------------

:- pred generate_foreign_prototype(in(Stream),in(ForeignName),in(In),
	in(Out), in(Res),in(CallArgs)) :: stream * atm * list(arg_type) *
	list(arg_type) * list(int) * list(arg_mode) # "Generates the
	foreign function prototype.".

generate_foreign_prototype(Stream,ForeignName,In,Out,[ResN],CallArgs) :- !,
	contains1(Out,arg_type(ResType,ResN)),
	type(ResType,ResCType,_,_,_,_),
	format(Stream,"~w ",[ResCType]),
	generate_foreign_prototype(Stream,ForeignName,In,Out,CallArgs).
generate_foreign_prototype(Stream,ForeignName,In,Out,[],CallArgs) :-
	format(Stream,"void ",[]),
	generate_foreign_prototype(Stream,ForeignName,In,Out,CallArgs).

%% ---------------------------------------------------------------------------

:- pred generate_foreign_prototype(in(Stream),in(ForeignName),in(In),in(Out),
	in(CallArgs)) ::
	stream * atm * list(arg_type) * list(arg_type) * list(arg_mode)
 # "It generates the body of the prototype of the foreign function.".

generate_foreign_prototype(Stream,ForeignName,In,Out,CallArgs) :-
	format(Stream,"~w(",[ForeignName]),
	generate_foreign_prototype_args(Stream,In,Out,CallArgs),
	format(Stream,");~n",[]).

%% ---------------------------------------------------------------------------

:- pred generate_foreign_prototype_args(in(Stream),in(In),in(Out),
	in(CallArgs)) ::
	stream * list(arg_type) * list(arg_type) * list(arg_mode)
 # "It generates the arguments of the prototype of the foreign funtion.".

generate_foreign_prototype_args(_,_,_,[]).
generate_foreign_prototype_args(Stream,I,O,[A|As]) :- !,
	generate_ctype(Stream,I,O,A),
	generate_foreign_prototype_args_2(Stream,I,O,As).

generate_foreign_prototype_args_2(_,_,_,[]).
generate_foreign_prototype_args_2(Stream,I,O,[A|As]) :- !,
	format(Stream,",",[]),
	generate_ctype(Stream,I,O,A),
	generate_foreign_prototype_args_2(Stream,I,O,As).

%% ---------------------------------------------------------------------------

:- pred generate_ctype(in(Stream),in(In),in(Out),in(Mode)) ::
	stream * list(arg_type) * list(arg_type) * arg_mode
 # "It writes the correct C type according to the mode of the argument.".

generate_ctype(Stream,In,_,in(N)) :- !,
	contains1(In,arg_type(Type,N)),
	type(Type,CType,_,_,_,_),
	format(Stream,"~w",CType).
generate_ctype(Stream,_,Out,out(N)) :-
	contains1(Out,arg_type(Type,N)),
	type(Type,CType,_,_,_,_),
	format(Stream,"~w*",CType).

%% ---------------------------------------------------------------------------

:- pred generate_header(in(Stream),in(CName)) :: stream * atm
 # "It generates the header of the gluecode function.".

generate_header(Stream,CName) :-
	format(Stream,"BOOL ~w(Arg)~n  Argdecl;~n{~n", [CName]).

%% ---------------------------------------------------------------------------

:- pred generate_c_variables_declaration(in(Stream),in(Types)) ::
	stream * list(arg_type)
 # "Generates the declaration of the register variables for the arguments in
    @var{Types}. These variables will be called @tt{vN}, where @tt{N} is the
    argument number.".

generate_c_variables_declaration(_,[]) :- !.
generate_c_variables_declaration(Stream,[arg_type(PType,N)|Vs]) :-
	type(PType,CType,_,_,_,_),
	format(Stream,"  REGISTER ~w v~d;~n",[CType,N]),
	generate_c_variables_declaration(Stream,Vs).

%% ---------------------------------------------------------------------------

:- pred generate_nonreg_c_variables_declaration(in(Stream),in(Types)) ::
	stream * list(arg_type)
 # "Generates the declaration of the variables for the arguments in
    @var{Types}. These variable will be called @tt{vN}, where @tt{N} is the
    argument number.".

generate_nonreg_c_variables_declaration(_,[]) :- !.
generate_nonreg_c_variables_declaration(Stream,[arg_type(PType,N)|Vs]) :-
	type(PType,CType,_,_,_,_),
	format(Stream,"  ~w v~d;~n",[CType,N]),
	generate_nonreg_c_variables_declaration(Stream,Vs).

%% ---------------------------------------------------------------------------

:- pred generate_tagged_variables_declaration(in(Stream),in(Types)) ::
	stream * list(arg_type)
 # "Generates the declaration of the @tt{TAGGED} variables for the arguments
    in @var{Types}. These variables will be called @tt{pN}, where @tt{N} is
    the argument number.".

generate_tagged_variables_declaration(_,[]) :- !.
generate_tagged_variables_declaration(Stream,[arg_type(_,N)|Vs]) :-
	format(Stream,"  REGISTER TAGGED p~d;~n",[N]),
	generate_tagged_variables_declaration(Stream,Vs).

%% ---------------------------------------------------------------------------

:- pred generate_pl2c_conversions(in(Stream),in(Phase),in(Types)) ::
	stream * atm * list(arg_type)
 # "Generates the Ciao Prolog to C converter code for each non compound
    argument in @var{Types}.".

generate_pl2c_conversions(_,_,[]) :- !.
generate_pl2c_conversions(Stream,Phase,[arg_type(PType,N)|Vs]) :-
	( Phase = phase1 ->
	    type(PType,_,Pl2c,_,_,_)
	; type(PType,_,_,Pl2c,_,_)
	),
	( (PType = byte_list ; Pl2c = '') ->
	    true
	; format(Stream,"  ~w(~d,v~d);~n", [Pl2c,N,N])
	),
	generate_pl2c_conversions(Stream,Phase,Vs).

%% ---------------------------------------------------------------------------

:- pred generate_pl2c_compound_conversions(in(Stream),in(Phase),in(Types),
	in(SizeLinks)) :: stream * atm * list(arg_type) * list(size_link)
 # "Generates the Ciao Prolog to C converter code for each compound
	argument in @var{Types}.".

generate_pl2c_compound_conversions(_,_,[],_) :- !.
generate_pl2c_compound_conversions(Stream,Phase,[arg_type(PType,N)|Vs],
	                           SizeLinks) :-
        ( Phase = phase1 ->
	    type(PType,_,Pl2c,_,_,_)
	; type(PType,_,_,Pl2c,_,_)
	),
	( (PType = byte_list, Pl2c \== '') ->
	    contains1(SizeLinks,size_of(N,M)),
	    format(Stream,"  ~w(~d,v~d,v~d);~n", [Pl2c,N,N,M])
	; true
	),
	generate_pl2c_compound_conversions(Stream,Phase,Vs,SizeLinks).

%% ---------------------------------------------------------------------------

:- pred generate_c2pl_conversions(in(Stream),in(Types)) :: stream *
	list(arg_type) # "Generates the code that converts the Ciao Prolog
	argument to C and stores the result in @tt{pN}, where @tt{N} is the
	argument number.".

generate_c2pl_conversions(_,[]) :- !.
generate_c2pl_conversions(Stream,[arg_type(PType,N)|Vs]) :-
	( PType = byte_list ->
	    true
	; type(PType,_,_,_,C2pl,_),
	  format(Stream,"  p~d = ~w(Arg,v~d);~n", [N,C2pl,N])
	),
        generate_c2pl_conversions(Stream,Vs).

%% ---------------------------------------------------------------------------

:- pred generate_c2pl_compound_conversions(in(Stream),
	in(Types),in(SizeLinks)) :: stream * list(arg_type) *
	list(size_links) # "Generates the code that converts the Ciao
	Prolog argument to C and stores the result in pN, where N is
	the argument number (compound version).".

generate_c2pl_compound_conversions(_,[],_) :- !.
generate_c2pl_compound_conversions(Stream,[arg_type(PType,N)|Vs],SizeLinks) :-
	( PType = byte_list ->
	    type(PType,_,_,_,C2pl,_),
	    contains1(SizeLinks,size_of(N,M)),
	    format(Stream,"  p~d = ~w(Arg,v~d,v~d);~n", [N,C2pl,N,M])
	; true
	),
        generate_c2pl_compound_conversions(Stream,Vs,SizeLinks).

%% ---------------------------------------------------------------------------

:- pred generate_call(in(Stream),in(ForeignName),in(CallArgs),in(Res)) ::
	stream * atm * list(arg_mode) * list(int)
 # "Generates the code that calls the foreign function.".

generate_call(Stream,ForeignName,CallArgs,Res) :-
	format(Stream,"  ",[]),
	( Res = [N] ->
	    format(Stream,"v~d = ",[N])
	; true
	),
	format(Stream,"~w(",[ForeignName]),
	generate_call_args(Stream,CallArgs),
	format(Stream,");~n",[]).

%% ---------------------------------------------------------------------------

:- pred generate_call_args(in(Stream),in(Types)) :: stream * list(arg_mode)
 # "Generates the arguments of the call to the foreign function.".

generate_call_args(_,[]) :- !.
generate_call_args(Stream,[in(N)|As]) :- !,
	format(Stream,"v~d",[N]),
	generate_call_args_2(Stream,As).
generate_call_args(Stream,[out(N)|As]) :-
	format(Stream,"&v~d",[N]),
	generate_call_args_2(Stream,As).

generate_call_args_2(_,[]) :- !.
generate_call_args_2(Stream,[in(N)|As]) :- !,
	format(Stream,",v~d",[N]),
	generate_call_args_2(Stream,As).
generate_call_args_2(Stream,[out(N)|As]) :-
	format(Stream,",&v~d",[N]),
	generate_call_args_2(Stream,As).

%% ---------------------------------------------------------------------------

:- pred generate_freeings(in(Stream),in(Types),in(NoFreeNs)) :: stream *
	list(arg_types) * list(int) # "Generates the code that frees (if
	necessary) the data that is allocated by calling the foreign
	function or converting a Ciao Prolog argument to C.".

generate_freeings(_,[],_) :- !.
generate_freeings(Stream,[arg_type(PType,N)|Vs],NoFreeNs) :-
	type(PType,_,_,_,_,Free),
	( (Free \== '', \+ contains1(NoFreeNs,N)) ->
	    format(Stream,"  ~w(v~d);~n",[Free,N])
	; true
	),
	generate_freeings(Stream,Vs,NoFreeNs).

%% ---------------------------------------------------------------------------

:- pred generate_bindings(in(Stream),in(Types)) :: stream * list(arg_types)
	# "Generates the code that binds the value of the output argument to
	the Ciao Prolog argument for each argument in Types.".

generate_bindings(_,[]) :- !.
generate_bindings(Stream,[arg_type(_,N)|Vs]) :-
	format(Stream,"  DEREF(X(~d),X(~d));~n",[N,N]),
	format(Stream,"  if (!cunify(Arg,X(~d),p~d))~n    return FALSE;~n",
	       [N,N]),
	generate_bindings(Stream,Vs).

%% ---------------------------------------------------------------------------

:- pred generate_end_of_function(in(Stream)) :: stream # "Generates the end
	of the gluecode function.".

generate_end_of_function(Stream) :-
	format(Stream,"  return TRUE;~n",[]),
	format(Stream,"}~n",[]).

%% ---------------------------------------------------------------------------

:- pred generate_init(in(Stream),in(Module),in(Predicates)) ::
	stream * atm * list(foreign_predicate_description)
 # "Generates the init function of the gluecode C file. This function is
    called when the shared object is loaded.".

generate_init(Stream,Module,Predicates) :-
	format(Stream,"void ~w_init(module)~n  char *module;~n{~n",[Module]),
	format(Stream,"  FUNCTOR_DEFINITION_CODE~n",[]),
	generate_define_c_mod_predicates(Stream,Predicates),
	format(Stream,"}~n",[]).

%% ---------------------------------------------------------------------------

:- pred generate_define_c_mod_predicates(in(Stream),in(Predicates)) ::
	stream * list(foreign_predicate_declaration)
 # "Generates the @code{define_c_mod_predicate} statement for each predicate
    in @var{Predicates}.".

generate_define_c_mod_predicates(_,[]) :- !.
generate_define_c_mod_predicates(Stream,[P|Ps]) :-
	P = pred(PrologName/Arity,CName,_,_,_,_,_,_,_),
	format(Stream,"  define_c_mod_predicate(module,\"~w\",~w,~d);~n",
	       [PrologName,CName,Arity]),
	generate_define_c_mod_predicates(Stream,Ps).

%% ---------------------------------------------------------------------------

:- pred generate_end(in(Stream),in(Module),in(Predicates)) ::
	stream * atm * list(foreign_predicate_description)
 # "Generates the end function of the gluecode C file. This function is
    called when the shared object is unloaded.".

generate_end(Stream,Module,Predicates) :-
	format(Stream,"void ~w_end(module)~n  char *module;~n{~n",[Module]),
	generate_undefine_c_mod_predicates(Stream,Predicates),
	format(Stream,"}~n",[]).

%% ---------------------------------------------------------------------------

:- pred generate_undefine_c_mod_predicates(in(Stream),in(Predicates)) ::
	stream * list(foreign_predicate_declaration)
 # "Generates the @code{define_c_mod_predicate} statement for each predicate
    in @var{Predicates}.".

generate_undefine_c_mod_predicates(_,[]) :- !.
generate_undefine_c_mod_predicates(Stream,[P|Ps]) :-
	P = pred(PrologName/Arity,_,_,_,_,_,_,_,_),
	format(Stream,"  undefine_c_mod_predicate(module,\"~w\",~d);~n",
	       [PrologName,Arity]),
	generate_undefine_c_mod_predicates(Stream,Ps).


%% ---------------------------------------------------------------------------

:- pred get_options(in(Decls),in(Option),go(Xs)) :: list(term) * atm *
	list(term)
	# "Collects in @var{Xs} the options for the currect architecture and
           Operating System from @var{Decls}.".

get_options(Decls,Option,Xs) :-
	get_os_arch(OsArch),
	OsArchDependantOption =.. [Option,OsArch,X],
	findall(X,member(OsArchDependantOption,Decls),Xs0),
	\+ Xs0 = [], % If empty, try the default options.
	flatten(Xs0,Xs), !.
get_options(Decls,Option,Xs) :-
	DefaultOption =.. [Option,X],
	findall(X,member(DefaultOption,Decls),Xs0),
	flatten(Xs0,Xs), !.

%% ---------------------------------------------------------------------------

:- pred compile_and_link(in(Rebuild),in(Dir),in(Base),in(Decls)) ::
	atm * atm * absfile * list(term)
	# "Ensures that every file needed for the foreign interface is
           compiled/linked".

compile_and_link(Rebuild,Dir,Base,Decls) :-
	get_foreign_files(Dir,Base,Decls,CFiles,OFiles,SOFile,_),
	( Rebuild = yes ->
	    delete_files([SOFile|OFiles])
	; true
	),
	compile_and_link_2(Decls,CFiles,OFiles,SOFile).

compile_and_link_2(Decls,CFiles,OFiles,SOFile) :-
	get_options(Decls,extra_compiler_opts,ExtraCompilerOpts),
	compile_foreign(ExtraCompilerOpts,CFiles,OFiles),
	get_options(Decls,extra_linker_opts,ExtraLinkerOpts),
	get_options(Decls,use_foreign_library,Libs),
	link_foreign(ExtraLinkerOpts,Libs,OFiles,SOFile),
	!.
compile_and_link_2(_,_,OFiles,SOFile) :-
	delete_files([SOFile|OFiles]),
	fail.

%% ---------------------------------------------------------------------------

:- pred get_foreign_files(in(Dir),in(Base),in(Decls),go(CFiles),go(OFiles),
	go(SOFile),go(UniqueOFile)) ::
	atm * atm * list(term) * list(absfile) * list(absfile) * absfile *
        absfile
 # "Get the absolute file name of the @tt{.c} files, @tt{.o} files and
    @tt{.so} files".

get_foreign_files(Dir,Base,Decls,CFiles,OFiles,SOFile,UniqueOFile) :-
	get_options(Decls,use_foreign_source,Files),
	absolute_base_names(Dir,Files,AbsFiles),
	get_gluecode_base(Base,CFileBase),
	get_os_arch(OsArch),
	atom_concat('_',OsArch,OsArchSuffix),
	atom_concat(CFileBase,'.c',CFile),
	append_suffix(AbsFiles,'.c',CFiles0),
	CFiles = [CFile|CFiles0],
	atom_concat(CFileBase,'.o',OFile),
	atom_concat(OsArchSuffix,'.o',OSuffix),
	append_suffix(AbsFiles,OSuffix,OFiles0),
	OFiles = [OFile|OFiles0],
	atom_concat(OsArchSuffix,'.so',SOSuffix),
	atom_concat(Base,SOSuffix,SOFile),
	atom_concat(Base,OSuffix,UniqueOFile).

%% ---------------------------------------------------------------------------

:- pred absolute_base_names(in(Dir),in(Files),go(AbsBases)) ::
	atm * list(sourcename) * list(atm)
 # "It obtains the list of absolute file name bases @var{AbsBases} from
    @var{Files}, using as default directory @var{Dir}.".

absolute_base_names(_,[],[]) :- !.
absolute_base_names(Dir,[File|Files],[AbsBase|AbsBases]) :-
	absolute_file_name(File,[],'.c',Dir,_,AbsBase,_),
	absolute_base_names(Dir,Files,AbsBases).

%% ---------------------------------------------------------------------------

:- pred append_suffix(in(Atoms0),in(Suffix),go(Atoms)) ::
	list(atm) * atm * list(atm)
 # "@var{Atoms} is the list obtained appending the suffix @var{Suffix} to each
    element of @var{Atoms0}.".

append_suffix([],_,[]) :- !.
append_suffix([A0|As0],Suffix,[A|As]) :-
	atom_concat(A0,Suffix,A),
	append_suffix(As0,Suffix,As).

%% ---------------------------------------------------------------------------

:- pred compile_foreign(in(ExtraOpts),in(CFiles),in(OFiles)) ::
	list(atm) * list(absname) * list(absname)
 # "Compile the @var{CFiles} C files with @var{ExtraOpts} additional compile
    options generating the @var{OFiles}.".

compile_foreign(ExtraOpts,CFiles,OFiles) :-
	compiler_and_opts(Compiler,Opts),
	append(Opts,ExtraOpts,TotalOpts),
	atom_concat_with_blanks([Compiler,'-c'|TotalOpts],CommandHead),
	compile_foreign_2(CommandHead,CFiles,OFiles).

compile_foreign_2(_,[],[]) :- !.
compile_foreign_2(CommandHead,[CFile|CFiles],[OFile|OFiles]) :-
	( has_changed(CFile,OFile) ->
	    atom_concat_with_blanks([CommandHead,'-o',OFile,CFile],Command),
	    system(Command,0)
	; true
	),
	compile_foreign_2(CommandHead,CFiles,OFiles).

%% ---------------------------------------------------------------------------

:- pred link_foreign(in(ExtraOpts),in(Libs),in(OFiles),in(SOFile)) ::
	list(atm) * list(atm) * list(absname) * absname
 # "Links @var{OFiles} with the additional link options @var{ExtraOpts}, using
    @var{Libs} libraries and generating the shared object @var{SOFile}.".

link_foreign(ExtraOpts,Libs0,OFiles,SOFile) :-
	( member(OFile,OFiles),
	  has_changed(OFile,SOFile) ->
	    linker_and_opts(Linker,Opts),
	    append_prefix('-l',Libs0,Libs),
	    flatten([Opts,ExtraOpts,['-o',SOFile|OFiles],Libs],Args), !,
	    atom_concat_with_blanks([Linker|Args],Command),
	    system(Command,0)
	; true
	).

%% ---------------------------------------------------------------------------

:- pred append_prefix(in(Prefix),in(Atoms0),go(Atoms)) ::
	atm * list(atm) * list(atm)
 # "@var{Atoms} is the list obtained appending the prefix @var{Suffix} to each
    element of @var{Atoms0}.".

append_prefix(_,[],[]) :- !.
append_prefix(Prefix,[A0|As0],[A|As]) :-
	atom_concat(Prefix,A0,A),
	append_prefix(Prefix,As0,As).

%% ---------------------------------------------------------------------------

:- pred atom_concat_with_blanks(in(List),go(Atom)) :: list(atm) * atm
 # "Concatenates the atoms in @var{List} into @var{Atom}, exactly as
    @pred{atom_concat/2}, but separating each atom with a blank.".

atom_concat_with_blanks(L,A) :-
	separate_with_blanks(L,L0),
	atom_concat(L0,A).

separate_with_blanks([],[]) :- !.
separate_with_blanks([A],[A]) :- !.
separate_with_blanks([A,B|Cs],[A,' '|Ds]) :-
	separate_with_blanks([B|Cs],Ds).

%% ---------------------------------------------------------------------------

:- pred build_foreign_interface_object(in(File)) ::
	sourcename
 # "Compiles the gluecode file with the foreign source files producing an
    unique object file.".

build_foreign_interface_object(File) :-
	build_foreign_interface_object(no, File).

%% ---------------------------------------------------------------------------

:- pred rebuild_foreign_interface_object(in(File)) ::
	sourcename
 # "Compiles (again) the gluecode file with the foreign source files
    producing an unique object file.".

rebuild_foreign_interface_object(File) :-
	build_foreign_interface_object(yes, File).

build_foreign_interface_object(Rebuild,File) :-
	get_decls(File,Decls),
	( do_interface(Decls) ->
	    absolute_file_name(File,[],'.pl','.',PrologFile,Base,Dir),
	    generate_gluecode(Rebuild,Base,PrologFile),
	    compile_unique_object(Rebuild,Dir,Base,Decls)
	; true
	).

compile_unique_object(Rebuild,Dir,Base,Decls) :-
	get_foreign_files(Dir,Base,Decls,CFiles,_,_,UniqueOFile),
	( Rebuild = yes ->
	    delete_files([UniqueOFile])
	; true
	),
	compile_unique_object_2(Decls,CFiles,UniqueOFile).

compile_unique_object_2(Decls,CFiles,UniqueOFile) :-
	( member(CFile,CFiles),
	  has_changed(CFile,UniqueOFile) ->
	    compiler_and_opts(Compiler,Opts),
	    get_options(Decls,extra_compiler_opts,ExtraCompilerOpts),
	    flatten([Opts,ExtraCompilerOpts,'-c','-o',UniqueOFile|CFiles],
	            Args), !,
	    atom_concat_with_blanks([Compiler|Args],Command),
	    system(Command,0)
	; true
	),
	!.
compile_unique_object_2(_,_,UniqueOFile) :-
	delete_files([UniqueOFile]),
	fail.
	

% -----------------------------------------------------------------------------
:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*3+123,1999/11/27,03:03*55+'MET'), "Minor changes
   to documentation. Incorporated into reference manual.  (Manuel
   Hermenegildo)").

:- comment(version(1*3+122,1999/11/27,03:02*54+'MET'), "Added new
   foreign interface.  (Jose Morales, Daniel Cabeza)").

% -----------------------------------------------------------------------------
