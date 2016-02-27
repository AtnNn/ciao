:- module( convert , _ , [ dcg , assertions ] ).

:- use_module(library(file_utils)).

:- use_module(library(format)).
:- use_module(library(lists)).

:- doc( author , "David Trallero Mena" ).
:- doc( module , 
"This module converts from lpdoc-1.9 Makefile-like SETTTING format to
the current lpdoc-2.0 lpmake format" ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                             GRAMMAR                                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


spaces --> " ", spaces.
spaces --> "". 

enters --> "\n".
enters --> "".

enters_and_spaces --> " "  , enters_and_spaces.
enters_and_spaces --> [10] , enters_and_spaces.
enters_and_spaces --> [13] , enters_and_spaces.
enters_and_spaces --> [ 9] , enters_and_spaces.
enters_and_spaces --> "".


alpha( A   ) :- A >= 0'A , A =< 0'Z.
alpha( A   ) :- A >= 0'a , A =< 0'z.
alpha( 0'[ ).
alpha( 0'] ).
alpha( 0'$ ).
alpha( 0'( ).
alpha( 0') ).
alpha( 0'_ ).
alpha( 0'. ).
alpha( 0'/ ).
alpha( 0': ).
alpha( 0'- ).
alpha( 0', ).

alphanum(X) :- alpha(X) ; number_char(X).

number_char( A ) :-  A >= 0'0 , A =< 0'9.


separator --> "\\", enters_and_spaces.
separator --> ",", enters_and_spaces.



process_file( F1 , F3 ) --> comment( F1 , F2 ) , process_file( F2 , F3 ).
process_file( F1 , F3 ) --> assign(  F1 , F2 ) , process_file( F2 , F3 ).
process_file( F1 , F3 ) --> ignore(  F1 , F2 ) , process_file( F2 , F3 ).
process_file( F1 , F1 ) --> "".
process_file( [ "ERROR: unexpected end" | F ] , F ) --> "".



comment( [ 0'% | C ] , CC ) --> [ 0'# ] , till_nl( C , CC ).



till_nl( [ 10 | C ] , C  ) --> "\n", enters_and_spaces.
till_nl( [ C | C1 ] , C2 ) --> [C], till_nl( C1 , C2 ).

ignore( "%% ignored line: %% include " || L , CC ) -->
	              "include " , till_nl( L, CC ).
		      
assign( Asign , A2 ) --> 
	              variable_name( N , [] ) ,
		      equal_symbol ,
		      spaces, 
		      value( V , V2 ),
		      {
%			  write( valores( V , V2 ) ),
			  V == V2 
		      ->
	                  Asign = [ 10 | A2 ]
                      ;
			  do_transform_option( N , NT ),
%			  write( transformando( N , NT ) ) , nl ,nl,
			  append( NT , " := " || V , Asign ),
		          V2 = [ 0'. , 0'\n | A2 ]
                      }.

equal_symbol --> "=".
equal_symbol --> ":=".


variable_name( [ A | As ] , C ) --> [A] , {alphanum( A )} ,
	variable_name( As , C ).
variable_name(   C        , C ) --> spaces.



num_variable( [ A | As ] , B ) --> [A] , {number_char( A )} ,
	num_variable( As , B ).
num_variable(   A        , A ) --> spaces.



quoted_variable_name( [ 0'' | V ]  , VC ) --> 
	variable_name( V , [ 0'' | VC ] ), 
	{ 
	  !,
	  V \== [ 0'' | VC ]
	}.

quoted_variable_name( V  , V ) --> "".




value( VT , VC ) --> 
	             variable_name( V , [] ) ,
		     {V \== []},
		     !,
		     {transform_if_needed( V , VT , A )},
		     value_kleene( A , VC ).

value( VT , VC ) --> 
	             num_variable( V , [] ) ,
		     {V \== []},
		     !,
		     {transform_if_needed( V , VT , A )},
		     value_kleene( A , VC ).

value( V , V ) --> enters_and_spaces.



value_kleene( [ 0'| , 0'\n , 32 , 32 ,32 ,32| AT ] , AsC ) -->
	                                    separator , 
					    !,
	                                    variable_name( A , [] ) ,
					    {A \== []},
					    {transform_if_needed( A , AT , As )},
					    value_kleene( As , AsC ).

value_kleene(  " | " || AT , AsC ) -->       
	                                    spaces,
	                                    variable_name( A , [] ) ,
					    {A \== []},
					    !,
					    {transform_if_needed( A , AT , As )},
					    value_kleene( As , AsC ).

value_kleene( A , A ) --> enters_and_spaces.




append_list( [] , [] ) :- !.
append_list( [X] , X ) :- !.
append_list( [A,B|C] , R ) :-
	append_list( [B | C ] , R1 ),
	append( A , R1 , R ).




no_caps_lock( [] , [] ).

no_caps_lock( [ A | As ] , [ B | Bs ] ) :-
	A >= 0'A , 
	A =< 0'Z,
	!,
	B is A - 0'A + 0'a,
	no_caps_lock( As , Bs ).

no_caps_lock( [ A | As ] , [ A | Bs ] ) :-
	no_caps_lock( As , Bs ).
       



transform_if_needed( STR , AT , Tail ) :-
 	list_concat( [ BEFORE , ".pl", REST ] , STR ),
 	!,
 	transform_if_needed( REST , REST_T , Tail ),
 	append_list( [ "'" , BEFORE , "'" , REST_T ] , AT ).

transform_if_needed( STR , AT , Tail ) :-
	transform_for_concat( STR , ST , [] ),
	(
	    ST = [ST1]
	->
	    append( ST1 , Tail , AT )
	;
	    generate_atom_concat( ST , AT , Tail )
	).




transform_for_concat( "" , Tail , Tail ) :-
	!.

transform_for_concat( STR , AT , Tail ) :-
 	list_concat( [ BEFORE , "$(", VARIABLE , ")" , REST ] , STR ),
	!,
	transform_for_concat( BEFORE , AT , ATT ),
	do_transform_option( VARIABLE, Variable),
	append( ["~" || Variable], RT , ATT ),
	transform_for_concat( REST , RT , Tail ).

transform_for_concat( STR , AT , Tail ) :-
	string_subs( PATH , VAR ),
	list_concat( [ BEFORE , PATH , REST ] , STR ),
	!,
	transform_for_concat( BEFORE , AT , ATT ),
	ATT = [ VAR | RT ],
	transform_for_concat( REST , RT , Tail ).

transform_for_concat( STR , [ STR | Tail ] , Tail ) :-
	STR = [ N | _ ],
	number_char( N ),
	!.

transform_for_concat( STR , [AT | Tail] , Tail ) :-
	append( [ 0'' | STR ] , "'" , AT ).




generate_atom_concat( "" , Tail , Tail ) :-
	!.

generate_atom_concat( ST , AT , Tail ) :-
	generate_atom_concat__( ST , AtomList ),
	append_list( [ "~atom_concat( [ " , AtomList , " ] )" , Tail ] , AT ).




generate_atom_concat__( [] , [] ).

generate_atom_concat__( [ A ] , A ) :-
	!.

generate_atom_concat__( [ A | As ] , AT ) :-
	generate_atom_concat__( As , AsComma ),
	append( A , " , " || AsComma , AT ).

	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                        TRANSFORMATIONS                                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Option Header conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform_option( "FILEPATHS"         , "filepath"               ) :- !.
transform_option( "SYSTEMPATHS"       , "systempath"             ) :- !.
% (obsolete)
transform_option( "MAIN"              , "mainfile"               ) :- !.
transform_option( "COMPONENTS"        , "component"              ) :- !.
transform_option( "COMPOPTS"          , "fileoption(~component)" ) :- !.
transform_option( "MAINOPTS"          , "fileoption(~mainfile)"  ) :- !.
%
transform_option( "DOCFORMATS"        , "docformat"              ) :- !.
transform_option( "INDICES"           , "index"                  ) :- !.
transform_option( "BIBFILES"          , "bibfile"                ) :- !.
transform_option( "STARTPAGE"         , "startpage"              ) :- !.
transform_option( "PAPERTYPE"         , "papertype"              ) :- !.
transform_option( "HTMLSTYLE"         , "htmlstyle"              ) :- !.
transform_option( "LIBTEXINFO"        , "libtexinfo"             ) :- !.
transform_option( "DOCDIR"            , "docdir"                 ) :- !.
transform_option( "DATAMODE"          , "datapermissions"        ) :- !.
transform_option( "EXECMODE"          , "execpermissions"        ) :- !.
transform_option( "HTMLINDEXHEADFILE" , "htmlindex_headfile"     ) :- !.
transform_option( "HTMLINDEXTAILFILE" , "htmlindex_tailfile"     ) :- !.
transform_option( "INFODIRHEADFILE"   , "infodir_headfile"       ) :- !.
transform_option( "INFODIRTAILFILE"   , "infodir_tailfile"       ) :- !.
transform_option( "LIBDIR"            , "lpdoclib"               ) :- !.

do_transform_option(A, B) :-
	transform_option(A, B) -> true
 ;
	no_caps_lock( A, B ).

%transform_option( A                   , A                        ).


%% To subsitute strings
%%%%%%%%%%%%%%%%%%%%%%%

string_subs( "/home/clip/Systems/ciao"  , "~bundle_src(ciao)"           ).
string_subs( "/home/clip/Systems/lpdoc" , "~bundle_src(lpdoc)"          ).
string_subs( "/home/clip/public_html/Local/lpdoc_docs",
	"~(ciao_config_options:docdir)" ).
string_subs( "664"                      , "perm(rw, rw, r)"    ).
string_subs( "775"                      , "perm(rwx, rwx, rx)" ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                               MAIN                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main( Args ) :-
	process_args(Args, _).

process_args(['-h'], _) :-
	format(user_error, "Usage: convert FileName Target").

process_args(['-m',Head|Params], Head) :-
	!,
	process_args(Params, Head).

process_args([File,Target], Head) :-
	(
	    var(Head) -> HeadString = []
	;
	    file_to_string( Head , HeadString )
	),
	set_prolog_flag( write_strings , on ),
	file_to_string( File , String ),
	process_file( Output , [] , String , Rest ),
	( 
	    Rest = []
	->
	    append(HeadString, Output, OutputString),
	    string_to_file(OutputString, Target)
	;
	    format(user_error, "ERROR while converting: ~n~s", [ Output ] ),
	    format(user_error, "~nSTOPPED AT:~n~s", [ Rest ] )
	).
