:- module(build_foreign_interface,
	[build_foreign_interface/1,
	 rebuild_foreign_interface/1,
	 build_foreign_interface_explicit_decls/2,
	 rebuild_foreign_interface_explicit_decls/2,
	 build_foreign_interface_object/1,
	 rebuild_foreign_interface_object/1,
	 do_interface/1
	],
	[assertions,
	 basicmodes,
	 dcg
	]).

:- comment(title, "Foreign Language Interface Builder").

:- comment(summary, "Low-level utilities for building foreign
interfaces.  End-users should not need to use them, as the Ciao Prolog
Compiler reads the user assertions and calls appropriately the
predicates in this module.").

:- comment(author, "Jose Morales").
:- comment(author, "Manuel Carro").

:- use_module(library(c)).
:- use_module(library(streams)).
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

% --------------------------------------------------------------------------- %

:- pred build_foreign_interface(in(File)) :: sourcename
 # "Reads assertions from @var{File}, generates the gluecode for the Ciao
    Prolog interface, compiles the foreign files and the gluecode file, and
    links everything in a shared object. Checks modification times to
    determine automatically which files must be generated/compiled/linked.".
build_foreign_interface(File) :-
	get_decls(File, Decls),
	build_foreign_interface_explicit_decls(File, Decls).

% --------------------------------------------------------------------------- %

:- pred build_foreign_interface(in(File)) :: sourcename
 # "Like @pred{build_foreign_interface/1}, but it does not check the 
    modification time of any file.".
rebuild_foreign_interface(File) :-
	get_decls(File, Decls), 
	build_foreign_interface_explicit_decls_2(yes, File, Decls).

% --------------------------------------------------------------------------- %

:- pred build_foreign_interface_explicit_decls(in(File),in(Decls)) ::
	sourcename * list(term)
 # "Like @pred{build_foreign_interface/1}, but use declarations in @var{Decls}
    instead of reading the declarations from @var{File}.".
build_foreign_interface_explicit_decls(File, Decls) :-
	build_foreign_interface_explicit_decls_2(no, File, Decls).

% --------------------------------------------------------------------------- %

:- pred rebuild_foreign_interface_explicit_decls(in(File),in(Decls)) ::
	sourcename * list(term)
 # "Like @pred{build_foreign_interface_explicit_decls/1}, but it does not
    check the modification time of any file.".
rebuild_foreign_interface_explicit_decls(File, Decls) :-
	build_foreign_interface_explicit_decls_2(yes, File, Decls).

% --------------------------------------------------------------------------- %

build_foreign_interface_explicit_decls_2(Rebuild, File, Decls) :-
	( do_interface(Decls) ->
	    absolute_file_name(File, [], '.pl', '.', PrologFile, Base, Dir),
	    gluecode(Rebuild, Base, PrologFile), 
	    compile_and_link(Rebuild, Dir, Base, Decls)
	; true
	).

% -----------------------------------------------------------------------------

:- pred do_interface(in(Decls)) :: list(term) # "Given the declarations in
	@var{Decls}, this predicate succeeds if these declarations involve
        the creation of the foreign interface".

do_interface(Decls) :-
	contains1(Decls, use_foreign_library(_)),  !.
do_interface(Decls) :-
	contains1(Decls, use_foreign_library(_, _)),  !.
do_interface(Decls) :-
	contains1(Decls, use_foreign_source(_)),  !.
do_interface(Decls) :-
	contains1(Decls, use_foreign_source(_, _)).

% -----------------------------------------------------------------------------

get_decls(File, Decls) :-
	absolute_file_name(File, [], '.pl', '.', PrologFile, Base, _), 
        error_protect(ctrlc_clean(
		process_files_from(PrologFile, in, module, get_decls_2(Decls),  
                                   false, false, '='(Base)))).

get_decls_2(Base, Decls) :-
	findall(D, decl(Base, D), Decls).

% -----------------------------------------------------------------------------

gluecode(Rebuild, Base, PrologFile) :-
	get_gluecode_base(Base, CFileBase), 
	atom_concat(CFileBase, '.c', CFile), 
	( Rebuild = no ->
	    has_changed(PrologFile, CFile)
	; true
	), !, 
	gluecode_2(Rebuild, PrologFile, CFile).
gluecode(_, _, _).

gluecode_2(Rebuild, PrologFile, CFile) :-
	( Rebuild = yes ->
	    delete_files([CFile])
	; true
	), 
	cleanup_code_and_related_assertions, 
	get_code_and_related_assertions(PrologFile, Module, _, _, _), 
	read_foreign_predicates(PrologFile, Predicates), 
	( Predicates = [] ->
	    warning_message("no foreign predicate found in '~w'", [PrologFile])
	; true
	), 
	( gluecode_program(Module, Predicates, Program, []) ->
	    true
	; error_message("generation of the interface gluecode for " ||
		        "Prolog file '~w' failed",  [PrologFile]),
	  fail
	),
	open_output(CFile, Stream), 
	( write_c(Program, Module, 0, _) ->
	    close_output(Stream)
	; close_output(Stream),
	  fail
	),
	cleanup_code_and_related_assertions, 
	!.
gluecode_2(_, _, CFile) :-
	cleanup_code_and_related_assertions, 
       	delete_files([CFile]), 
	fail.

% -----------------------------------------------------------------------------

get_gluecode_base(Base, CFileBase) :-
	get_os_arch(OsArch), 
	atom_concat([Base, '_', OsArch, '_glue'], CFileBase).

% -----------------------------------------------------------------------------

has_changed(SourceFile, TargetFile) :-
	modif_time0(SourceFile, TS), 
	modif_time0(TargetFile, TT), 
	TS > TT.

% -----------------------------------------------------------------------------

get_os_arch(OsArch) :-
	get_os(Os), 
	get_arch(Arch), 
	atom_concat(Os, Arch, OsArch).

% -----------------------------------------------------------------------------

delete_files([]) :- !.
delete_files([F|Fs]) :-
	( file_exists(F) ->
	    delete_file(F)
	; true
	), 
	delete_files(Fs).

% -----------------------------------------------------------------------------

:- data foreign_predicate/1.
:- data foreign_predicate_error/0.
	
read_foreign_predicates(PrologFile, _) :-
        cleanup_foreign_predicates, 

	% Reads the assertions.
	assertion_read(_, _, Status, _, Body, VarNames, PrologFile, LB, LE), 

	% Extracts PrologName,  Arity and ForeignName.
	Body = ::(Pr, =>(DP:CP, AP+GP#_)), 
	functor(Pr, PrologName, Arity), 
	PredName = PrologName/Arity, 
	Pr =.. [_|Arguments], 
	Loc = loc(PrologFile, LB, LE), 

	% Get the foreign name. If no foreign name is found,  fails (without
        % producing any error).
	( get_foreign_name(Loc, PredName, GP, ForeignName) ->
						% Checks the correctness of the assertion.
	    check_unique(Loc, PredName), 
	    check_all_arguments(Loc, PredName, Arguments, DP), 
	    check_all_arguments(Loc, PredName, Arguments, CP), 
	    check_all_arguments(Loc, PredName, Arguments, AP), 
	    get_returns(Loc, PredName, CP, GP, Arguments, VarNames, Res), 
	    check_list_correctness(Loc, PredName, DP, GP, Arguments, VarNames),
	    check_do_not_free_correctness(Loc, PredName, GP, Arguments), 
	    check_status(Loc, PredName, Status), 
						% Fill the predicate's description structure.
	    PredDescription = foreign(PredName, CName, ForeignName, 
	                              In, Out, Res, SizeLinks, CallArgs, NoFree), 
	    atom_concat('prolog_', PrologName, CName), 
	    Arity1 is Arity - 1, 
	    numbers_between(0, Arity1, Arguments), 
	    findall(X, member(ground(X), CP), InN), 
	    difference(Arguments, InN, OutN), 
	    assign_types(Loc, PredName, VarNames, DP, OutN, Out), 
	    assign_types(Loc, PredName, VarNames, DP, InN, In), 
	    difference(Arguments, Res, CallArgsN), 
	    assign_modes(InN, CallArgsN, CallArgs), 
	    findall(X, (member(Y, GP), Y=size_of(_, A, B), X=size_of(A, B)), SizeLinks), 
	    findall(X, (member(Y, GP), Y=do_not_free(_, X)), NoFree)
        ; get_native_name(Loc, PredName, GP, NativeName) ->
	  PredDescription = native(PredName, NativeName)
        ), 
	assertz_fact(foreign_predicate(PredDescription)), 
	
        % Backtracks over the next predicate.
	fail. 
read_foreign_predicates(_, Predicates) :-
	findall(Predicate, foreign_predicate(Predicate), Predicates), 
	cleanup_foreign_predicates, 
	% Fails if error.
	\+ retract_fact(foreign_predicate_error).

cleanup_foreign_predicates :-
	retractall_fact(foreign_predicate(_)).

check_unique(Loc, PredName) :-
	foreign_predicate(foreign(PredName, _, _, _, _, _, _, _, _)),  !, 
	error_message(Loc, "duplicated assertions for predicate ~w (foreign)", 
	             [PredName]), 
        fail.
check_unique(_, _).

check_all_arguments(_, _, _, []) :- !.
check_all_arguments(Loc, PredName, Arguments, [X|_]) :-
	X =.. [_, Y], 
	nocontainsx(Arguments, Y), 
	!, 
	error_message(Loc, "invalid argument name in predicate ~w", [PredName]), 
        set_fact(foreign_predicate_error), 
	fail.
check_all_arguments(Loc, PredName, Arguments, [_|Xs]) :-
	check_all_arguments(Loc, PredName, Arguments, Xs).

get_foreign_name(Loc, PredName, GP, ForeignName) :-
	select(foreign(_), GP, GP0),  !, 
	no_more_foreign_name(Loc, PredName, GP0), 
	PredName = ForeignName/_.
get_foreign_name(Loc, PredName, GP, ForeignName) :-
	select(foreign(_, ForeignName), GP, GP0), 
	valid_foreign_name(Loc, PredName, ForeignName), 
	no_more_foreign_name(Loc, PredName, GP0).

get_native_name(Loc, PredName, GP, NativeName) :-
	select(native(_), GP, GP0),  !, 
	no_more_foreign_name(Loc, PredName, GP0), 
	PredName = PrologName/_, 
	atom_concat('prolog_', PrologName, NativeName).
get_native_name(Loc, PredName, GP, NativeName) :-
	select(native(_, NativeName0), GP, GP0), 
	atom_concat('prolog_', NativeName0, NativeName), 
	valid_foreign_name(Loc, PredName, NativeName), 
	no_more_foreign_name(Loc, PredName, GP0).

valid_foreign_name(_, _, Name) :-
	atom(Name),  !.
valid_foreign_name(Loc, PredName, _) :-
	error_message(Loc, "invalid foreign/native function name in predicate ~w", 
                      [PredName]), 
        set_fact(foreign_predicate_error), 
	fail.

no_more_foreign_name(_, _, GP) :-
	\+ member(foreign(_), GP), 
	\+ member(foreign(_, _), GP), 
	\+ member(native(_), GP), 
	\+ member(native(_, _), GP), 
	!.
no_more_foreign_name(Loc, PredName, _) :-
	error_message(Loc, "more than one foreign/1,  foreign/2,  native/1 or" ||
		      "native/2 property in predicate ~w", [PredName]), 
	set_fact(foreign_predicate_error), 
	fail.

get_returns(Loc, PredName, CP, GP, Arguments, VarNames, [Argument]) :-
	select(returns(_, Argument), GP, GP0),  !, 
	valid_returns_argument(Loc, PredName, Arguments, Argument), 
	no_more_returns(Loc, PredName, GP0), 
	returns_in_output_arg(Loc, PredName, CP, VarNames, Argument).
get_returns(_, _, _, _, _, _, []).

valid_returns_argument(Loc, PredName, Arguments, Argument) :-
	nocontainsx(Arguments, Argument), 
	error_message(Loc, "returns/2 with invalid argument in predicate ~w", 
                      [PredName]), 
	set_fact(foreign_predicate_error), 
	fail.
valid_returns_argument(_, _, _, _).

no_more_returns(_, _, GP) :-
	\+ member(returns(_, _), GP), 
	!.
no_more_returns(Loc, PredName, _) :-
	error_message(Loc, "more than one returns/2 property in predicate ~w", 
                      [PredName]), 
        set_fact(foreign_predicate_error), 
	fail.

returns_in_output_arg(_, _, CP, _, Argument) :-
	nocontainsx(CP, ground(Argument)), 
	!.
returns_in_output_arg(Loc, PredName, _, VarNames, Argument) :-
	var_name(Argument, VarNames, VarName), 
	error_message(Loc, "~w is not an output argument in predicate ~w", 
	              [VarName, PredName]), 
        set_fact(foreign_predicate_error), 
	fail.		      

one_list_for_each_size_of(Loc, PredName, DP, GP, Arguments) :-
	member(size_of(_, ListVar, SizeVar), GP), 
	\+ valid_size_of_property(Arguments, ListVar, SizeVar, DP), 
	!, 
	error_message(Loc, "invalid size_of property in predicate ~w", 
	              [PredName]), 
        set_fact(foreign_predicate_error), 
        fail.
one_list_for_each_size_of(_, _, _, _, _).

check_list_correctness(Loc, PredName, DP, GP, Arguments, VarNames) :-
	one_list_for_each_size_of(Loc, PredName, DP, GP, Arguments), 
	one_size_of_for_each_int_list(Loc, PredName, DP, GP, VarNames),
	one_size_of_for_each_byte_list(Loc, PredName, DP, GP, VarNames).

valid_size_of_property(Arguments, ListVar, SizeVar, DP) :-
	\+ nocontainsx(Arguments, ListVar), 
	\+ nocontainsx(Arguments, SizeVar), 
	( \+ nocontainsx(DP, byte_list(ListVar)) ->
	    true
	; \+ nocontainsx(DP, int_list(ListVar))
	),
	\+ nocontainsx(DP, int(SizeVar)).

one_size_of_for_each_byte_list(Loc, PredName, DP, GP, VarNames) :-
	member(byte_list(ListVar), DP), 
	findall(Y, (member(size_of(_, Y, _), GP), Y==ListVar), S), 
	nonsingle(S), 
	!, 
	var_name(ListVar, VarNames, VarName), 
	error_message(Loc, "variable ~w in predicate ~w needs a (only one) " ||
		      "size_of/3 property", [VarName, PredName]), 
        set_fact(foreign_predicate_error), 
	fail.
one_size_of_for_each_byte_list(_, _, _, _, _).

one_size_of_for_each_int_list(Loc, PredName, DP, GP, VarNames) :-
	member(int_list(ListVar), DP), 
	findall(Y, (member(size_of(_, Y, _), GP), Y==ListVar), S), 
	nonsingle(S), 
	!, 
	var_name(ListVar, VarNames, VarName), 
	error_message(Loc, "variable ~w in predicate ~w needs a (only one) " ||
		      "size_of/3 property", [VarName, PredName]), 
        set_fact(foreign_predicate_error), 
	fail.
one_size_of_for_each_int_list(_, _, _, _, _).

var_name(Var, VarNames, Name) :-
	findall(N, (member(N=X, VarNames), X==Var), [Name]).

check_do_not_free_correctness(Loc, PredName, GP, Arguments) :-
	member(do_not_free(_, Var), GP), 
	nocontainsx(Arguments, Var), 
	!, 
	error_message(Loc, "invalid do_not_free/2 property in predicate ~w", 
                      [PredName]), 
	fail.
check_do_not_free_correctness(_, _, _, _).

numbers_between(A, B, []) :-
	A > B,  !.
numbers_between(A, B, [A|Ns]) :-
	A1 is A + 1, 
	numbers_between(A1, B, Ns).

assign_types(_, _, _, _, [], []) :- !.
assign_types(Loc, PredName, VarNames, CP, [N|Ns], [arg_type(Type, N)|Types]) :-
	find_type(N, CP, Type), 
	!, 
	assign_types(Loc, PredName, VarNames, CP, Ns, Types).
assign_types(Loc, PredName, VarNames, _, [N|_], _) :-
	var_name(N, VarNames, VarName), 
	error_message(Loc, "no valid type found for variable ~w in " ||
		     "predicate ~w", [VarName, PredName]), 
        set_fact(foreign_predicate_error), 
	fail.

find_type(N, [Prop|_], Type) :-
	Prop =.. [Type, N], 
	type(Type, _, _, _, _, _),  !.
find_type(N, [_|Props], Type) :-
	find_type(N, Props, Type).

assign_modes(_, [], []) :- !.
assign_modes(InN, [N|Ns], [A|As]) :-
	( contains1(InN, N) ->
	    A = in(N)
	; A = out(N)
	), 
	assign_modes(InN, Ns, As).

check_status(_, _, true) :- !.
check_status(_, _, trust) :- !.
check_status(Loc, PredName, _) :-
	warning_message(Loc,
	"assertions of predicate ~w cannot be checked (foreign)", [PredName]).

% -----------------------------------------------------------------------------

gluecode_program(Module, Predicates) -->
	imports, 
	foreign_predicates_interface(Predicates), 
	init(Predicates, Module), 
	end(Predicates, Module).

% -----------------------------------------------------------------------------

% 
% C type + properties, Prolog type + properties, Unify op (versions of unify...) + Cost of unify + Cost of undo + Garbagge collect
%
% Properties of C types: uninitialized, refers(1) to... etc.
% 

%
% Native unification ...
%  - how to determine if it is a reference?
%  - how to dereference?
%  - how to trail?
%  - how to untrail?
%  - how to unify¿
%  - how to allocate?
%  - how to deallocate?

type(int, long, 'GET_INTEGER', '', 'MakeInteger', '').
type(num, double, 'GET_NUMBER', '', 'MakeFloat', '').
type(atm, pointer(char), '', 'GET_CSTRING_FROM_ATOM', 'MAKE_ATOM', 'FREE').
type(string, pointer(char), 'STRING_TEST', 'GET_CSTRING_FROM_LIST', 'MAKE_STRING', 
     'FREE').
type(byte_list, pointer(char), 'BYTES_TEST', 'GET_BYTES', 'MakeList', 'FREE').
type(int_list, pointer(int), 'INTS_TEST', 'GET_INTS', 'MakeIntList', 'FREE').
type(address, pointer(void), 'GET_ADDRESS', '', 'MakeAddress', '').

% -----------------------------------------------------------------------------

imports -->
	{ include_base_dir(Dir), 
	  atom_concat(Dir, '/datadefs.h', Datadefs), 
	  atom_concat(Dir, '/support.h', Support), 
	  absolute_file_name(library('foreign_interface/foreign_interface.h'), 
	                     [], '.h', Dir, _, NativeInterfaceHBase, _),
	  atom_concat(NativeInterfaceHBase, '.h', Native)
	},
	[local_include(Datadefs),
	 local_include(Support),
	 local_include(Native),
	 format(new_line)].

% -----------------------------------------------------------------------------

include_base_dir(Dir) :-
	get_os_arch(OsArch), 
	ciaolibdir(Dir0), 
	atom_concat([Dir0, '/include/', OsArch], Dir).

% -----------------------------------------------------------------------------

foreign_predicates_interface([]) --> !.
foreign_predicates_interface([P|Ps]) -->
	foreign_predicate_interface(P),
	[format(new_line)],
	foreign_predicates_interface(Ps).

% -----------------------------------------------------------------------------

foreign_predicate_interface(P) -->
	{ P = foreign(_, CName, ForeignName, InVars, OutVars, ResVar,
	              SizeLinks, CallArgs, NoFreeVars), ! },
	foreign_prototype(ForeignName, InVars, OutVars, ResVar, CallArgs), 
	{ interface_function_body(ForeignName, InVars, OutVars, SizeLinks, CallArgs, ResVar, NoFreeVars, Body, []) },
	[CName:function([w:pointer(struct(worker))], 'BOOL')#Body].
foreign_predicate_interface(P) -->
	{ P = native(_, NativeName) }, 
	native_prototype(NativeName).

% -----------------------------------------------------------------------------

foreign_prototype(ForeignName, In, Out, ResNs, CallArgs) -->
	{ ResNs = [ResN] ->
	    contains1(Out, arg_type(ResType, ResN)),
	    type(ResType, ResCType, _, _, _, _)
	; ResCType = void
	},
	{ foreign_prototype_args(CallArgs, In, Out, Args, []) },
	[ForeignName:function(Args, ResCType)].

% -----------------------------------------------------------------------------

native_prototype(NativeName) -->
	[NativeName:function([pointer(struct(worker))], 'BOOL')].

% -----------------------------------------------------------------------------

foreign_prototype_args([], _, _) --> !.
foreign_prototype_args([A|As], I, O) --> !, 
	{ ctype(I, O, A, CType) },
	[CType],  
	foreign_prototype_args(As, I, O).

% -----------------------------------------------------------------------------

interface_function_body(ForeignName, InVars, OutVars, SizeLinks, CallArgs, ResVar, NoFreeVars) -->
	c_variables_declaration(InVars), 
	c_variables_declaration(OutVars), 
	tagged_variables_declaration(OutVars), 
	pl2c_conversions(InVars, phase1), 
	pl2c_compound_conversions(InVars, SizeLinks, phase1), 
	pl2c_conversions(InVars, phase2), 
	pl2c_compound_conversions(InVars, SizeLinks, phase2), 
	do_call(ForeignName, CallArgs, ResVar), 
	c2pl_conversions(OutVars), 
	c2pl_compound_conversions(OutVars, SizeLinks), 
	freeings(InVars, NoFreeVars), 
	freeings(OutVars, NoFreeVars), 
	bindings(OutVars),
	[ return('TRUE') ].

% -----------------------------------------------------------------------------

ctype(In, _, in(N), CType) :- !, 
	contains1(In, arg_type(Type, N)), 
	type(Type, CType, _, _, _, _).
ctype(_, Out, out(N), pointer(CType)) :-
	contains1(Out, arg_type(Type, N)), 
	type(Type, CType, _, _, _, _).

% -----------------------------------------------------------------------------

c_variables_declaration([]) --> !.
c_variables_declaration([arg_type(PType, N)|Vs]) -->
	{ type(PType, CType, _, _, _, _) }, 
	[identifier("v~d", [N]):CType],
	c_variables_declaration(Vs).

% -----------------------------------------------------------------------------

tagged_variables_declaration([]) --> !.
tagged_variables_declaration([arg_type(_, N)|Vs]) -->
	[identifier("p~d", [N]):'TAGGED'],
	tagged_variables_declaration(Vs).

% -----------------------------------------------------------------------------

pl2c_conversions([], _) --> !.
pl2c_conversions([arg_type(PType, N)|Vs], Phase) -->
	{ Phase = phase1 ->
	    type(PType, _, Pl2c, _, _, _)
	; type(PType, _, _, Pl2c, _, _)
	}, 
	( { (PType = byte_list ; PType = int_list ; Pl2c = '') } ->
	    { true }
	; { XN = call('X', [0]) },
	  [call('DEREF', [XN, XN]),
	   call(Pl2c, [N, identifier("v~d", [N])])]
	), 
	pl2c_conversions(Vs, Phase).

% -----------------------------------------------------------------------------

pl2c_compound_conversions([], _, _) --> !.
pl2c_compound_conversions([arg_type(PType, N)|Vs], 
	                           SizeLinks, Phase) -->
        { Phase = phase1 ->
	    type(PType, _, Pl2c, _, _, _)
	; type(PType, _, _, Pl2c, _, _)
	}, 
	( { (PType \== byte_list, PType \== int_list ) ; Pl2c = '' } ->
	    { true }
	; { contains1(SizeLinks, size_of(N, M)) },
	  [call(Pl2c, [N, identifier("v~d", [N]), identifier("v~d", [M])])]
	), 
	pl2c_compound_conversions(Vs, SizeLinks, Phase).

% -----------------------------------------------------------------------------

c2pl_conversions([]) --> !.
c2pl_conversions([arg_type(PType, N)|Vs]) -->
	( { PType = byte_list ; PType = int_list } ->
	    { true }
	; { type(PType, _, _, _, C2pl, _) },  
	  [identifier("p~d", [N])=call(C2pl, ['Arg', identifier("v~d", [N])])]
	), 
        c2pl_conversions(Vs).

% -----------------------------------------------------------------------------

c2pl_compound_conversions([], _) --> !.
c2pl_compound_conversions([arg_type(PType, N)|Vs], SizeLinks) -->
	( { PType = byte_list ; PType = int_list } ->
	    { type(PType, _, _, _, C2pl, _), 
	      contains1(SizeLinks, size_of(N, M)) },
	    [identifier("p~d", [N])=call(C2pl, ['Arg', identifier("v~d", [N]), identifier("v~d", [M])])]
	; { true }
	), 
        c2pl_compound_conversions(Vs, SizeLinks).

% -----------------------------------------------------------------------------

do_call(ForeignName, CallArgs, Res) -->
	{ call_args(CallArgs, Args, []) },
	( { Res = [N] } ->
	    [identifier("v~d", [N]) = call(ForeignName, Args)]
	; [call(ForeignName, Args)]
	).

% -----------------------------------------------------------------------------

call_args([]) --> !.
call_args([A|As]) -->
	( { A = in(N) } ->
	    [identifier("v~d", [N])]
	; { A = out(N) },
	  [address(identifier("v~d", [N]))]
	),
	call_args(As).

% -----------------------------------------------------------------------------

freeings([], _) --> !.
freeings([arg_type(PType, N)|Vs], NoFreeNs) -->
	{ type(PType, _, _, _, _, Free) },
	( { (Free \== '',  \+ contains1(NoFreeNs, N)) } ->
	    [call(Free, [identifier("v~d", [N])])]
	; { true }
	), 
	freeings(Vs, NoFreeNs).

% -----------------------------------------------------------------------------

bindings([]) --> !.
bindings([arg_type(_, N)|Vs]) -->
	{ XN = call('X', [N]) },
	[call('DEREF', [XN, XN])],
	{ CUnify = call(cunify, ['Arg', XN, identifier("p~d", [N])]) },
	[if(logical(\ CUnify), return('FALSE'))],
	bindings(Vs).

% -----------------------------------------------------------------------------

init(Predicates, Module) -->
	{ define_c_mod_predicates(Predicates, DefineCModPredicates, []), 
	  InitName = identifier("~w_init", [Module]),
	  InitBody = ['FUNCTOR_DEFINITION_CODE'|DefineCModPredicates]
	},
	[InitName:function([module:pointer(char)], void)#InitBody,
	 format(new_line)].

% -----------------------------------------------------------------------------

define_c_mod_predicates([]) --> !.
define_c_mod_predicates([P|Ps]) -->
	{ P = foreign(PrologName/Arity, CName, _, _, _, _, _, _, _) ->
	    true
	; P = native(PrologName/Arity, CName)
	},
	{ atom_codes(PrologName, PrologNameString) },  
	[call(define_c_mod_predicate, [module, PrologNameString, CName, Arity])],
	define_c_mod_predicates(Ps).

% -----------------------------------------------------------------------------

end(Predicates, Module) -->
	{ undefine_c_mod_predicates(Predicates, Body, []) },
	[identifier("~w_end", [Module]):
	 function([module:pointer(char)], void)#Body,
	 format(new_line)].

% -----------------------------------------------------------------------------

undefine_c_mod_predicates([]) --> !.
undefine_c_mod_predicates([P|Ps]) -->
	{ P = foreign(PrologName/Arity, _, _, _, _, _, _, _, _) ->
	    true
	; P = native(PrologName/Arity, _) },
	{ atom_codes(PrologName, PrologNameString) },  
	[call(undefine_c_mod_predicate, [module, PrologNameString, Arity])],
	undefine_c_mod_predicates(Ps).

% -----------------------------------------------------------------------------

get_options(Decls, Option, Xs) :-
	get_os_arch(OsArch), 
	OsArchDependantOption =.. [Option, OsArch, X], 
	findall(X, member(OsArchDependantOption, Decls), Xs0), 
	\+ Xs0 = [],  % If empty,  try the default options.
	flatten(Xs0, Xs),  !.
get_options(Decls, Option, Xs) :-
	DefaultOption =.. [Option, X], 
	findall(X, member(DefaultOption, Decls), Xs0), 
	flatten(Xs0, Xs),  !.

% -----------------------------------------------------------------------------

compile_and_link(Rebuild, Dir, Base, Decls) :-
	get_foreign_files(Dir, Base, Decls, CFiles, OFiles, SOFile, _), 
	( Rebuild = yes ->
	    delete_files([SOFile|OFiles])
	; true
	), 
	compile_and_link_2(Decls, CFiles, OFiles, SOFile).

compile_and_link_2(Decls, CFiles, OFiles, SOFile) :-
	get_options(Decls, extra_compiler_opts, ExtraCompilerOpts), 
	compile_foreign(ExtraCompilerOpts, CFiles, OFiles), 
	get_options(Decls, extra_linker_opts, ExtraLinkerOpts), 
	get_options(Decls, use_foreign_library, Libs), 
	link_foreign(ExtraLinkerOpts, Libs, OFiles, SOFile), 
	!.
compile_and_link_2(_, _, OFiles, SOFile) :-
	delete_files([SOFile|OFiles]), 
	fail.

% -----------------------------------------------------------------------------

get_foreign_files(Dir, Base, Decls, CFiles, OFiles, SOFile, UniqueOFile) :-
	get_options(Decls, use_foreign_source, Files), 
	absolute_base_names(Dir, Files, AbsFiles), 
	get_gluecode_base(Base, CFileBase), 
	get_os_arch(OsArch), 
	atom_concat('_', OsArch, OsArchSuffix), 
	atom_concat(CFileBase, '.c', CFile), 
	append_suffix(AbsFiles, '.c', CFiles0), 
	CFiles = [CFile|CFiles0], 
	atom_concat(CFileBase, '.o', OFile), 
	atom_concat(OsArchSuffix, '.o', OSuffix), 
	append_suffix(AbsFiles, OSuffix, OFiles0), 
	OFiles = [OFile|OFiles0], 
	atom_concat(OsArchSuffix, '.so', SOSuffix), 
	atom_concat(Base, SOSuffix, SOFile), 
	atom_concat(Base, OSuffix, UniqueOFile).

% -----------------------------------------------------------------------------

absolute_base_names(_, [], []) :- !.
absolute_base_names(Dir, [File|Files], [AbsBase|AbsBases]) :-
	absolute_file_name(File, [], '.c', Dir, _, AbsBase, _), 
	absolute_base_names(Dir, Files, AbsBases).

% -----------------------------------------------------------------------------

append_suffix([], _, []) :- !.
append_suffix([A0|As0], Suffix, [A|As]) :-
	atom_concat(A0, Suffix, A), 
	append_suffix(As0, Suffix, As).

% -----------------------------------------------------------------------------

compile_foreign(ExtraOpts, CFiles, OFiles) :-
	compiler_and_opts(Compiler, Opts), 
	append(Opts, ExtraOpts, TotalOpts), 
	atom_concat_with_blanks([Compiler, '-c'|TotalOpts], CommandHead), 
	compile_foreign_2(CommandHead, CFiles, OFiles).

compile_foreign_2(_, [], []) :- !.
compile_foreign_2(CommandHead, [CFile|CFiles], [OFile|OFiles]) :-
	( has_changed(CFile, OFile) ->
	    atom_concat_with_blanks([CommandHead, '-o', OFile, CFile], Command), 
	    system(Command, 0)
	; true
	), 
	compile_foreign_2(CommandHead, CFiles, OFiles).

% -----------------------------------------------------------------------------

link_foreign(ExtraOpts, Libs0, OFiles, SOFile) :-
	( member(OFile, OFiles), 
	  has_changed(OFile, SOFile) ->
	    linker_and_opts(Linker, Opts), 
	    append_prefix('-l', Libs0, Libs), 
	    flatten([Opts, ExtraOpts, ['-o', SOFile|OFiles], Libs], Args),  !, 
	    atom_concat_with_blanks([Linker|Args], Command), 
	    system(Command, 0)
	; true
	).

% -----------------------------------------------------------------------------

append_prefix(_, [], []) :- !.
append_prefix(Prefix, [A0|As0], [A|As]) :-
	atom_concat(Prefix, A0, A), 
	append_prefix(Prefix, As0, As).

% -----------------------------------------------------------------------------

atom_concat_with_blanks(L, A) :-
	separate_with_blanks(L, L0), 
	atom_concat(L0, A).

separate_with_blanks([], []) :- !.
separate_with_blanks([A], [A]) :- !.
separate_with_blanks([A, B|Cs], [A, ' '|Ds]) :-
	separate_with_blanks([B|Cs], Ds).

% -----------------------------------------------------------------------------

:- pred build_foreign_interface_object(in(File)) ::
	sourcename
 # "Compiles the gluecode file with the foreign source files producing an
    unique object file.".

build_foreign_interface_object(File) :-
	build_foreign_interface_object_2(no,  File).

% -----------------------------------------------------------------------------

:- pred rebuild_foreign_interface_object(in(File)) ::
	sourcename
 # "Compiles (again) the gluecode file with the foreign source files
    producing an unique object file.".

rebuild_foreign_interface_object(File) :-
	build_foreign_interface_object_2(yes,  File).

% -----------------------------------------------------------------------------

build_foreign_interface_object_2(Rebuild, File) :-
	get_decls(File, Decls), 
	( do_interface(Decls) ->
	    absolute_file_name(File, [], '.pl', '.', PrologFile, Base, Dir), 
	    gluecode(Rebuild, Base, PrologFile), 
	    compile_unique_object(Rebuild, Dir, Base, Decls)
	; true
	).

compile_unique_object(Rebuild, Dir, Base, Decls) :-
	get_foreign_files(Dir, Base, Decls, CFiles, _, _, UniqueOFile), 
	( Rebuild = yes ->
	    delete_files([UniqueOFile])
	; true
	), 
	compile_unique_object_2(Decls, CFiles, UniqueOFile).

compile_unique_object_2(Decls, CFiles, UniqueOFile) :-
	( member(CFile, CFiles), 
	  has_changed(CFile, UniqueOFile) ->
	    compiler_and_opts(Compiler, Opts), 
	    get_options(Decls, extra_compiler_opts, ExtraCompilerOpts), 
	    flatten([Opts, ExtraCompilerOpts, '-c', '-o', UniqueOFile|CFiles], 
	            Args),  !, 
	    atom_concat_with_blanks([Compiler|Args], Command), 
	    system(Command, 0)
	; true
	), 
	!.
compile_unique_object_2(_, _, UniqueOFile) :-
	delete_files([UniqueOFile]), 
	fail.

% -----------------------------------------------------------------------------
:- comment(version_maintenance, dir('../../version')).

:- comment(version(1*7+75,2001/03/26,17:08*47+'CEST'), "Documentation
updated (MCL)").

:- comment(version(1*3+123, 1999/11/27, 03:03*55+'MET'), "Minor changes
   to documentation. Incorporated into reference manual.  (Manuel
   Hermenegildo)").

:- comment(version(1*3+122, 1999/11/27, 03:02*54+'MET'), "Added new
   foreign interface.  (Jose Morales, Daniel Cabeza)").

% -----------------------------------------------------------------------------




