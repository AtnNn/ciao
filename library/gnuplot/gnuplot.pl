:- module( gnuplot , [
	                      get_general_options/1, 
			      set_general_options/1,

			      generate_plot/2,
			      generate_plot/3
			   ], [assertions,regtypes] ).


:- use_module( library( lists ) ).
:- use_module( library( write ) ).
:- use_module( library( system ) ).

:- data general_options/1.

general_options( [
                             key([left,nobox]) , 
			     term_post([portrait,color,"Times-Roman",14])
			   ] ).


:- pred get_general_options(X)
	: var(X) => list(X)
# "Get the general options of the graphic that will be plotted".

get_general_options( X ) :- current_fact( general_options( X ) ).

:- pred set_general_options( list(X) )
# "Get the general options of the graphic that will be plotted.
Posible options are:
@item format(A) Specify the format of points
@item nokey Leyend is no represented
@item nogrid No grid
@item grid An smooth grid is shown
@item label(L , (X,Y)) Put Label L at point (X,Y)
@item xlabel(A) Label of X-Axis
@item ylabel(A) Lavel of Y-Axis
@item xrange(A,B) Define the X range representation
@item yrange(A,B) Define the Y range representation
@item title(A) Title of the plot

@item key(A) define the key (for example [left,box], left is the position,
box indicates that a box should be around)

@item term_post(A) define the postcript terminal. A is a list of atoms.
@item size(A,B) specify the size of the plot (A,B float numbers)
@item autoscale autoscale the size of the plot
@item autoscale(A) autoscale the argument (for example: autoscale(x))
".

set_general_options( NO ) :- 
	current_fact( general_options( X ) ),
	retract_fact( general_options( X ) ),
	asserta_fact( general_options( NO ) ).



:- comment(title,"Printing graph using gnuplot as auxiliary tool.").
:- comment(author, "David Trallero Mena").

:- comment(module, "This library uses @tt{gnuplot} for printing graphs.

Two predicates are provided:

@item genarate_plot( BaseName , DataList ) 
@item genarate_plot( BaseName , DataList , GlobalOptions ) 


Several files can be generated as temporary files. A BaseName is requiered
for generating the tempoaries files.Data files name will be created from
BaseName + number + .dat. The BaseName + \".plot\" will be the name used
for @tt{gnuplot} tool.

A list of pairs of list of pairs of the from (X,Y) and Local Option value
is provided to the main predicate as data. In other words DataList =
[(CurveDataList,LocalOptions), (CurveDataList1,LocalOptions1)
...]. Additionaly (function( String ) , LocalOptions) can be used for
adding a curve to the plot (imagine you want to compare your result with x=y).

LocalOptions of the DataList are options that are applied to the curve, as
for example, if we print the curve with lines, or the title in the leyend,
etc.  GlobalOptions are referred to the plot options, like title in x or y
axis, etc. ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% PRINTING GRAPHICS %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


disp_list( [] ) :- !.
disp_list( [(T,P) | R ] ) :-
	write( T ) , write( ' ' ) , write( P ) , nl,
	disp_list( R ).


:- set_prolog_flag(multi_arity_warnings,off).

:- pred generate_plot( BaseName , TimeList ) 
	: atom * list(pair) => atom * list(pair)

# "This predicates generate a '@var{BaseName} + .ps' postcript file using
	each element of @var{TimeList} as pair of list of pairs and local
	options, i.e., ( list((X,Y)), LocalOptions), in which X is the
	position in X-Axis and Y is the position in Y-Axis. Nevertheless,
	each element of @var{TimeList} can be a list of pairs instead of a
	pair for comodity. @tt{gnuplot} is used as ausiliary
	tool. Temporary files '@var{BaseName} + N.dat' are generated for
	for every list of pairs, and '@var{BaseName} + .plot' is de file
	used by gnuplot. The local options can be:

 @item with(Option) Tells how the curve will be represented. Option can b
        line, dots, boxes, impulses, linespoints. This option HAVE TO BE
        the last one

 @item title(T) Put the name of the curve in the leyend to T".


generate_plot( _ , [] ) :- !.

generate_plot( BaseName , A ) :- !,
	get_general_options( GO ),
	generate_plot( BaseName , A , GO ).

:- pred generate_plot( BaseName , TimeList , GeneralOptions ) : atom *
	list(pair) * list => atom * list(pair) * list # "It is the same as
	generate_plot/2 but @var{GeneralOptions} are used as the general
	options of the plot. Look at predicate set_general_options for
	detailed description of posible options".

generate_plot( BaseName ,  L , GeneralOptions ) :-
	length( L , Len ),
	generate_plot_data_files( BaseName , L , Len  ),
	generate_ps( BaseName , L , Len , GeneralOptions ).

:- set_prolog_flag(multi_arity_warnings,on).

generate_plot_data_files( _BaseName , [] , 0 ) :- !.

generate_plot_data_files( BaseName , [(A,_LocalOptions)|R] , Len ) :- !,
	generate_nfile( BaseName, Len , A ),
	Len1 is Len - 1 ,
	generate_plot_data_files( BaseName , R , Len1 ).

generate_plot_data_files( BaseName , [function( _ )|R] , Len ) :-
	Len1 is Len - 1 ,
	generate_plot_data_files( BaseName , R , Len1 ).


generate_nfile( BaseName , N , A ) :-
        atom_number( NN , N ),
	atom_concat_list( [BaseName , NN , '.dat'] , File ),
	open( File , write , FileStream ), 
	current_output( Out ),
	set_output( FileStream ),
	disp_list( A ),
	set_output( Out ),
	close( FileStream ).


generate_ps( BaseName , L , Len , GeneralOptions ) :-
	atom_concat( BaseName , '.plot' , PlotName ),
	atom_concat( BaseName , '.ps' , PS_FileName ),
	
	open( PlotName , write , S ),
	current_output( Out ),
	set_prolog_flag( write_strings, on),
	  set_output( S ),
	    write_options_list( GeneralOptions ),
	    write( 'plot ' ), 
	    write_plot_list( Len , L , BaseName ),
	  set_output( Out ),
	close( S ),
	atom_concat_list( ['gnuplot ' , PlotName , ' > ' , PS_FileName] , Command ),
	system( Command ).


atom_concat_list( [A ,B] , C ) :-
	atom_concat( A , B , C ).

atom_concat_list( [A,B|R] , Res ) :-
	atom_concat( A , B , C ),
	atom_concat_list( [C|R] , Res ).

%%%%% WRITE_OPTIONS_LIST

write_options_list( [] ).
write_options_list( [A|R] ) :-
	write_plot_option( A ) , nl ,
	write_options_list( R ).
	

write_plot_option( format(A) ) :- write( 'set format ' ), write_list_of_options( A ).
write_plot_option( nokey ) :- write( 'set nokey' ).
write_plot_option( nogrid ) :- write( 'set nogrid' ).
write_plot_option( grid ) :- write( 'set grid' ).
write_plot_option( label(L , (X,Y)) ) :- write( 'set label ' ), write( L ) , 
	                                 write( ' at ' ) , write( X ) , write( ',' ) , write( Y ).
write_plot_option( xlabel(A) ) :- write( 'set xlabel ' ), write( A ).
write_plot_option( ylabel(A) ) :- write( 'set ylabel ' ), write( A ).
write_plot_option( xrange(A,B) ) :- write( 'set xrange [ ' ), write( A ) , write(' : ') , write(B),write(' ]').
write_plot_option( yrange(A,B) ) :- write( 'set yrange [ ' ), write( A ) , write(' : ') , write(B),write(' ]').
write_plot_option( title(A) ) :- write( 'set title ' ), write( A ).
write_plot_option( key(A) ) :- 	write( 'set key ' ), write_list_of_options( A ).
write_plot_option( term_post(A) ) :- 	write( 'set term post ' ), write_list_of_options( A ).
write_plot_option( size(A,B) ) :- 	write( 'set size ' ), write( A ) , write( ',' ) , write( B ).
write_plot_option( autoscale ) :- 	write( 'set autoscale' ).
write_plot_option( autoscale(A) ) :- 	write( 'set autoscale ' ) , write( A).
write_plot_option( Opt ) :- message( error , ['Option ' , Opt , ' unknown'] ).

write_list_of_options( [] ).
write_list_of_options( [A|R] ) :-
	write( A ),
	write( '  ' ) ,
	write_list_of_options( R ).

%%%%% END WRITE_OPTIONS_LIST


write_plot_list( 0 , _ , _ ) :- !.
write_plot_list( N , [function( F )|R] , BaseName ) :-
	N1 is N - 1,
	write( F ),
	(
	    N1 = 0
	->
	    true 
	;
	    write( ' , ' )
	),
	write_plot_list( N1 , R , BaseName ).
	
write_plot_list( N , [(_, LocalOptions)|R] , BaseName ) :-
	N1 is N - 1,
	atom_number( N_atom , N ),
	atom_concat_list( [ '\'' , BaseName , N_atom ,  '.dat\''],Command ),
	write( Command ),
	write_local_options( LocalOptions ),
	(
	    N1 = 0
	->
	    true 
	;
	    write( ' , ' )
	),
	write_plot_list( N1 , R , BaseName ).


write_local_options( [] ) :- !.
write_local_options( [A|R] ) :-
	write_local_option( A ),
	write_local_options( R ).

write_local_option( title( X ) ) :- write( ' title ' ) , write( X ).
write_local_option( with( X ) ) :- write( ' with ' ) , write( X ).
