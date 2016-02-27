:- module(mattr_sicstus_trans, [lsmattr_def/3, mattr_sics_redef/3], []).

:- use_module(library(mattr_local(mattr_local_common))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DIRECTIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lsmattr_def(A, B, C) :-
	dispatch_attr_directives(A, B, C).

lsmattr_def(end_of_file,
	    [
		:-(multifile('$max_num_attr$'/2)),
		:-(multifile('$attr_hash$'/3)),
		'$max_num_attr$'(PM, Len),
		( check_attr(A, B) :-
		    set_attr(AA, A, PM),
		    (verify_attributes(AA, B, Goals) -> true),
		    AA = B,
		    call_list(Goals) ),

		( combine_attr(A, B, AA) :-
		    set_attr(AA, A, PM),
		    set_attr(BB, B, PM),
		    (verify_attributes(AA, BB, Goals) -> true),
		    AA = BB,
		    call_list(Goals) )
		|Hash
	    ],
	    M) :-
	generate_prio_module(M, PM),
	attr_convert(AttrList, Len, M),
	g_h_t(AttrList, PM, 2, Hash).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Precalculate attributes we can
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mattr_sics_redef(get_atts(X, A), get_atts(X, A, PM), M) :-
	generate_prio_module(M, PM).

mattr_sics_redef(put_atts(X, A), put_atts(X, A, PM), M) :-
	generate_prio_module(M, PM).
