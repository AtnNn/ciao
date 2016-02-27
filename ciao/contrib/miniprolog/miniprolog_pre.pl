:- module(miniprolog_auto, _, [fsyntax, assertions]).

can_fail := 
	arith_add
	|arith_div
	|arith_mod
	|arith_mul
	|arith_sub
	|bigger
	|bigger_eq
	|call
	|call_function
	|call_builtin
	|equal
	|fail
	|get_constant_atom
	|get_constant_int
	|get_value
	|profile
	|profile_init
	|time
	|smaller
	|smaller_eq
	|unequal
	|unify_variable.

init_new_literal :=
	arith_add
	|arith_div
	|arith_mod
	|arith_mul
	|arith_sub
	|bigger
	|bigger_eq
	|call
	|call_function
	|call_builtin
	|cut
	|equal
	|execute
	|execute_function
	|fail
	|smaller
	|smaller_eq
	|unequal.

bytecode(unknown, 0).
bytecode(allocate, 0).
bytecode(arith_add, 3).
bytecode(arith_div, 3).
bytecode(arith_mod, 3).
bytecode(arith_mul, 3).
bytecode(arith_sub, 3).
bytecode(bigger, 2).
bytecode(bigger_eq, 2).
bytecode(call, 2).
bytecode(call_function, 2).
bytecode(call_builtin, 1).
bytecode(create_q_variable, 2).
bytecode(cut, 1).
bytecode(deallocate, 0).
bytecode(equal, 2).
bytecode(execute, 2).
bytecode(execute_function, 2).
bytecode(fail, 0).
bytecode(get_constant_atom, 2).
bytecode(get_constant_int, 2).
bytecode(get_level, 1).
bytecode(get_list, 2).
bytecode(get_struct, 2).
bytecode(get_value, 2).
bytecode(get_variable, 2).
bytecode(halt, 0).
bytecode(no_op, 0).
bytecode(proceed, 0).
bytecode(profile, 0).
bytecode(profile_init, 0).
bytecode(profile_end, 0).
bytecode(time, 0).
bytecode(time_init, 0).
bytecode(time_end, 0).
bytecode(put_a_constant_atom, 2).
bytecode(put_a_constant_int, 2).
bytecode(put_constant_atom, 2).
bytecode(put_constant_int, 2).
bytecode(put_value, 2).
bytecode(retry_me_else, 2).
bytecode(set_constant_atom, 1).
bytecode(set_constant_int, 1).
bytecode(set_variable, 1).
bytecode(smaller, 2).
bytecode(smaller_eq, 2).
bytecode(trust_me, 0).
bytecode(try_me_else, 2).
bytecode(unequal, 2).
bytecode(unify_variable, 2).

bc_params(unify_variable) := 
	[
	    [[gnd,var]],
	    [[var,gnd]],
	    [[con1,con2]],
	    [[int,int]],
	    [[lst(1),lst(1)]],
	    [[atm,atm]],
	    [[str,str]],
	    [[lst(100),lst(100)]]
	].
