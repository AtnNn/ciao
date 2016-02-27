:- module(tabling_rt_NOIFAZ,
        [
	    put_tabled_call/3,
	    test_complete/1,
	    is_lider/1,
	    tabled_call/6,
	    consume_answer/3,
	    new_answer/4
	], 
%	[assertion,regtypes,foreign_interface]).
	[assertions,isomodes,regtypes,foreign_interface]).

:- doc(doinclude,put_table_call/3).
:- doc(doinclude,test_complete/1).
:- doc(doinclude,is_lider/1).
:- doc(doinclude,table_call/5).
:- doc(doinclude,consume_answer/3).
:- doc(doinclude,new_answer/2).
:- doc(doinclude,print_stuct/2).

:- true pred put_tabled_call(?A, ?B, ?C) 
	+ foreign_low(put_tabled_call_c).

:- true pred test_complete(?A) 
	+ foreign_low(test_complete_c).

:- true pred is_lider(?A) 
	+ foreign_low(is_lider_c).

:- true pred tabled_call(?A, ?B, ?C, ?D, ?E, ?F) 
	+ foreign_low(tabled_call_c).

:- true pred consume_answer(?A, ?B, ?C) 
        + foreign_low(consume_answer_c).

:- true pred new_answer(?A, ?B, ?C, ?D) 
        + foreign_low(new_answer_c).

:- true pred initial 
        + foreign_low(initial_c).

:- use_foreign_source(cont_calls).

%:- use_foreign_library('LINUXi86', ['c']).

%:- extra_compiler_opts(['-g -Wall -DDEBUG -DATOMGC -DGLOBVARS -DINTERNAL_CALLING']).

:- initialization(initial).

