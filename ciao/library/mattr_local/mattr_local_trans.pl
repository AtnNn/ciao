:- module(mattr_local_trans, [lmattr_def/3, lmattr_redef/3],
	    [assertions, dcg]).

:- use_module(library(messages)).
:- use_module(library(mattr_local(mattr_local_common))).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TRANSFORMING CLAUSES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

lmattr_def(A, B, C) :-
	dispatch_attr_directives(A, B, C).

%% FOR adding code at the end of the file :D
lmattr_def(end_of_file,
	    [
		:-(multifile('$max_num_attr$'/2)),
		:-(multifile('$attr_hash$'/3)),
		'$max_num_attr$'(PM, L)
		|Hash
	    ],
	    M) :-
	generate_prio_module(M, PM),
	generate_hash_table(M, Hash, L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Precalculate attributes we can
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lmattr_redef(0, _, M) :-
	retract_fact(attr_convert(_, _, M)),
	fail.

lmattr_redef(mattr_global_code:get_attr(X, A, PM),
	    mattr_local_code: get_iattr(X, A, I, PM), M) :-
	nonvar(A),
	!,
	( get_index(A, I, M)
	-> true
	; error_message("Could not translate ~w in get_attr(~w,~w)",
		[A, X, A])
	).

lmattr_redef(mattr_global_code:get_attr(X, A, PM),
	    mattr_local_code: get_attr(X, A, PM), _M).


lmattr_redef(mattr_global_code:set_attr(X, A, PM),
	    mattr_local_code:set_iattr(X, A, I, PM), M) :-
	nonvar(A),
	!,
	( get_index(A, I, M)
	-> true
	; error_message("Could not translate ~w in " ||
		"set_attr(~w,~w)",
		[A, X, A])
	).

lmattr_redef(mattr_global_code:set_attr(X, A, PM),
	    mattr_local_code: set_attr(X, A, PM), _M).


lmattr_redef(mattr_global_code:detach_attr(X, A, PM),
	    mattr_local_code:detach_iattr(X, I, PM), M) :-
	nonvar(A),
	!,
	get_index(A, I, M).

lmattr_redef(mattr_global_code:detach_attr(X, A, PM),
	    mattr_local_code: detach_attr(X, A, PM), _M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END of code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
