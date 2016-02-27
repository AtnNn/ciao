:- package(difference_constraints).

:- use_package(assertions).

:- doc(nodoc, assertions).

:- doc(filetype, package).

:- doc(author,"Pablo Chico de Guzm@'{a}n Huerta").
:- doc(author,"The CLIP Group").

:- doc(title,"Difference Constraints").

:- doc(module, "This module supports difference constraint
evaluation.").

:- doc(bug, "This library is a beta version. The constraint store is a
$N\timesN$ matrix.").

:- doc('#='/2, "Meta-constraint \"equal\".").
:- doc('#<>'/2, "Meta-constraint \"not equal\".").
:- doc('#\<'/2, "Meta-constraint \"smaller than\".").
:- doc('#=<'/2, "Meta-constraint \"smaller or equal\".").
:- doc('#>'/2, "Meta-constraint \"greater than\".").
:- doc('#>='/2, "Meta-constraint \"greater or equal\".").

:- use_package(runtime_ops).

:- include(library(difference_constraints(difference_constraints_syntax))).

:- push_prolog_flag(unused_pred_warnings, no).

:- use_module(library(difference_constraints(difference_constraints_rt))).
:- use_module(library(difference_constraints(difference_constraints_translation))).
:- include(library(difference_constraints(difference_constraints_attributes))).

:- reexport(library(difference_constraints(difference_constraints_rt)),
	[
 %%  	    '$forward_trail'/2,
 %%  	    '$incr_dc_num_vars'/0,
 %%  	    '$decr_dc_num_vars'/0,
 %%  	    '$put_dc_value'/3,
 %%  	    '$put_dc_pi'/1,
 %% 	    '$put_dc_attr'/2,
 %%    	    '$put_dc_space'/1,
	    difference_constraints_print/0,
	    difference_constraints_print_variable/1,
	    difference_constraints_var/1,
	    difference_constraints_min/2,
	    difference_constraints_max/2,
	    difference_constraints_difference/3,
	    difference_constraints_delay/1,
	    difference_constraints_reset/3,
	    difference_constraints_full_abstraction/1,
	    difference_constraints_normalize/3,
	    difference_constraints_do_canonical/0
	]).

:- reexport(library(difference_constraints(difference_constraints_translation)),
        [
	    '#='/2,
	    '#>'/2,
	    '#<'/2,
	    '#>='/2,
	    '#=<'/2,
	    '#<>'/2
	]).

 :- pop_prolog_flag(unused_pred_warnings).
