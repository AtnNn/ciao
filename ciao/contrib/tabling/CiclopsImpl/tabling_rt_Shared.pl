:- module(tabling_rt_Shared,
        [
	    test_complete/1,
	    tabled_call/5,
	    read_answers/6,
	    is_lider/1,
	    clean_tables/1,
	    consume_answer/3,
	    new_answer/5
	], 
%	[assertion,regtypes,foreign_interface]).
	[assertions,isomodes,regtypes,foreign_interface]).

:- doc(doinclude,is_lider/1).
:- doc(doinclude,test_complete/1).
:- doc(doinclude,table_call/5).
:- doc(doinclude,read_answers/6).
:- doc(doinclude,consume_answer/3).
:- doc(doinclude,new_answer/5).

:- true pred clean_tables(?A)
	+ foreign_low(clean_tables_c).

:- true pred is_lider(?A) 
	+ foreign_low(is_lider_c).

:- true pred test_complete(?A) 
	+ foreign_low(test_complete_c).

:- true pred tabled_call(?A, ?B, ?C, ?D, ?E) 
	+ foreign_low(tabled_call_c).

:- true pred read_answers(?A, ?B, ?C, ?D, ?E, ?F) 
        + foreign_low(read_answers_c).

:- true pred consume_answer(?A, ?B, ?C) 
        + foreign_low(consume_answer_c).

:- true pred new_answer(?A, ?B, ?C, ?D, ?E) 
        + foreign_low(new_answer_c).

:- true pred initial 
        + foreign_low(initial_c).

:- use_foreign_source(cont_callsShared).

:- initialization(initial).

