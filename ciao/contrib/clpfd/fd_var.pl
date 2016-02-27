%% ---------------------------------------------------------------------------
%% This file is part of the clpfd package for Ciao
%%
%% Copyright (C) 2012 CLIP Group
%%
%% Written by:
%%   * Remy Haemmerle
%%   * Jose F. Morales
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 2
%% of the License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
%% ---------------------------------------------------------------------------

:- module(fd_var,
	[
	    fd_var_t/1,

	    default/1,
	    new/2,
	    get_range/2,
	    set_range/2,
	    get_propags/2,
	    set_propags/2,
	    get_id/2
	],
	[assertions, regtypes, fsyntax, condcomp]).

:- use_module(library(clpfd(fd_range)),   [fd_range_t/1, default/1]).
:- use_module(library(clpfd(fd_pchains)), [fd_pchains_t/1, empty/1]).

:- doc(title, "Finite Domain Variables").

:- regtype fd_var_t/1 #
	"Internal representation of an indexical variable".

% Note: The Id field is used for debugging purposes only.  We may
% remove it to save memory.
fd_var_t(fd_var(Id, Range, Chains)) :-
	int(Id),
	fd_range_t(Range),
	fd_pchains_t(Chains).

:- pred default(-fd_var_t).

default(Var):-
	new(~(fd_range:default), Var).

:- pred new(+fd_range_t, -fd_var_t) # "Create a new FD variable from a range.".
:- pred get_range(+fd_var_t, -fd_range_t) # "Return the range of an FD variable.".
:- pred get_propags(+fd_var_t, -fd_propags_t) # "Return the propagation chain of an FD variable.".
:- pred set_propags(+fd_var_t, +fd_propags_t) # "Set the propagation chain of an FD variable. Imperative.".
:- pred get_id(+fd_var_t, -int) # "Get the ID of an FD variable.".

% TODO: the type is not right for the JS-backend.

% TODO: Once we solve the bug, we should abstract the imperative
% features to avoid multiple optim_comp defines

:- if(\+ (defined(optim_comp), backend(js_backend))).

:- use_module(library(odd), [setarg/3]).

new(Range, fd_var(~incr_id_counter, '$'(Range), '$'(~(fd_pchains:empty)))).

get_id(fd_var(Id, _, _)) := Id.

get_range(fd_var(_, '$'(Range), _)) := Range.

set_range(Store, Range) :-
	odd:setarg(2, Store, '$'(Range)).

get_propags(fd_var(_, _, '$'(Chains))) := Chains.

set_propags(Store, Chains) :-
	odd:setarg(3, Store, '$'(Chains)).

% Avoid setarg in the JS-backend.
:- else.

new(Range, Store) :-
	Store = fd_var(RangeV, ChainsV),
	put_attr_local(RangeV, Range),
	put_attr_local(ChainsV, ~fd_propags:empty).

get_val(fd_var(Var, _, _), Var).

get_range(fd_var(_, RangeV, _)) := Range :-
	get_attr_local(RangeV, Range).
set_range(fd_var(_Var, RangeV, _), Range) :-
	put_attr_local(RangeV, Range).

get_propags(fd_var(_, _, ChainsV)) := Chains :-
	get_attr_local(ChainsV, Chains).
set_propags(fd_var(_Var, _, ChainsV), Chains) :-
	put_attr_local(ChainsV, Chains).

get_id(_) := 1. % ignored (couldn't it be extracted from the variable?)

:- endif.

% ---------------------------------------------------------------------------
% A global counter for variables (debug)

:- use_module(library(global_vars), [getval/2, setval/2]).

incr_id_counter(X):-
        global_vars:getval(var_counter, X),
        (
            X = 0 ->
            Y = 1
        ;
            Y is X + 1
        ),
        global_vars:setval(var_counter, Y).



