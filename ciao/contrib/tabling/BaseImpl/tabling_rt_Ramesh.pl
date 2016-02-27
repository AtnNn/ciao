:- module(tabling_rt_Ramesh,
        [
	    tabled_call/5,
	    consume_answer/3,
	    new_answer/2
	], 
%	[assertion,regtypes,foreign_interface]).
	[assertions,isomodes,regtypes,foreign_interface]).

:- doc(doinclude,table_call/5).
:- doc(doinclude,consume_answer/3).
:- doc(doinclude,new_answer/2).
:- doc(doinclude,print_stuct/2).

:- true pred tabled_call(?A, ?B, ?C, ?D, ?E) 
	+ foreign_low(tabled_call_c).

:- true pred consume_answer(?A, ?B, ?C) 
        + foreign_low(consume_answer_c).

:- true pred new_answer(?A, ?B) 
        + foreign_low(new_answer_c).

:- true pred initial 
        + foreign_low(initial_c).

:- use_foreign_source(cont_calls).

%:- use_foreign_library('LINUXi86', ['c']).

:- extra_compiler_opts(['-O2 -Wall -fno-strict-aliasing -fomit-frame-pointer -falign-loops=2 -falign-jumps=2 -falign-functions=2 -DATOMGC -D_REENTRANT -DTHREADS -DFOREIGN_FILES -DLINUX -Di86   -DHAS_MMAP -DANONYMOUS_MMAP']).

:- initialization(initial).

