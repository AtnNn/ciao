:- module(_fib,[do_fib/0,fib/2],ciaopp).

:- use_module('/home/clip/Systems/ciao-1.5/lib/prolog_sys',[statistics/0,statistics/2,predicate_property/2,current_atom/1,current_predicate/2,garbage_collect/0,new_atom/1]).

:- use_module('/home/clip/Systems/ciao-1.5/lib/write',[write_term/3,write_term/2,write_option/1,write/2,write/1,write_list1/1,writeq/2,writeq/1,write_canonical/2,write_canonical/1,print/2,print/1,portray_clause/2,portray_clause/1,numbervars/3,prettyvars/1,printable_char/1,write_term/3,write_term/2,write_option/1,write/2,write/1,write_list1/1,writeq/2,writeq/1,write_canonical/2,write_canonical/1,print/2,print/1,portray_clause/2,portray_clause/1,numbervars/3,prettyvars/1,printable_char/1]).

:- use_module('/home/clip/Systems/ciao-1.5/lib/streams',[open_null_stream/1,open_input/2,close_input/1,open_output/2,close_output/1]).

:- use_module('/home/clip/Systems/ciao-1.5/lib/read',[read/1,read/2,read_term/2,read_term/3,read_top_level/3,second_prompt/2]).

:- use_module('/home/clip/Systems/ciao-1.5/lib/operators',[op/3,current_op/3,current_prefixop/3,current_infixop/4,current_postfixop/3]).

:- use_module('/home/clip/Systems/ciao-1.5/lib/iso_incomplete',[open/4,close/2,stream_property/2]).

:- use_module('/home/clip/Systems/ciao-1.5/lib/iso_byte_char',[char_code/2,atom_chars/2,number_chars/2,get_byte/1,get_byte/2,peek_byte/1,peek_byte/2,put_byte/1,put_byte/2,get_char/1,get_char/2,peek_char/1,peek_char/2,put_char/1,put_char/2]).

:- use_module('/home/clip/Systems/ciao-1.5/lib/iso_misc',[\= /2,once/1,compound/1,sub_atom/5,unify_with_occurs_check/2]).

:- use_module('/home/clip/Systems/ciao-1.5/lib/dynamic',[asserta/1,asserta/2,assertz/1,assertz/2,assert/1,assert/2,retract/1,retractall/1,abolish/1,clause/2,clause/3,current_predicate/1,(dynamic)/1,(data)/1,wellformed_body/3]).

:- use_module('/home/clip/Systems/ciao-1.5/lib/aggregates',[setof/3,bagof/3,findall/3,findall/4,(^)/2]).

:- use_module(engine(arithmetic),[arithexpression/1,=\= /2,=:= /2,>= /2,> /2,=< /2,< /2,is/2]).

:- use_module(engine(atomic_basic),[sub_atom/4,atom_concat/3,atom_length/2,number_codes/3,number_codes/2,atom_codes/2,name/2]).

:- use_module(engine(attributes),[detach_attribute/1,update_attribute/2,get_attribute/2,attach_attribute/2]).

:- use_module(engine(basic_props),[regtype/1,not_further_inst/2,iso/1,compat/2,atm_or_atm_list/1,predname/1,string/1,character_code/1,sequence_or_list/2,sequence/2,member/2,list/2,list/1,operator_specifier/1,callable/1,constant/1,gnd/1,struct/1,atm/1,num/1,flt/1,nnegint/1,int/1,term/1]).

:- use_module(engine(basiccontrol),[srcdbg_spy/6,call/1,repeat/0,fail/0,true/0,if/3,(\+)/1,!/0,-> /2,;/2,','/2]).

:- use_module(engine(concurrency),[atom_lock_state/2,unlock_atom/1,lock_atom/1,eng_status/0,eng_self/1,eng_killothers/0,eng_kill/1,eng_wait/1,eng_release/1,eng_cut/1,eng_backtrack/2,eng_call/3,eng_call/4]).

:- use_module(engine(data_facts),[erase/1,set_fact/1,open_predicate/1,close_predicate/1,retract_fact_nb/1,current_fact_nb/1,retractall_fact/1,retract_fact/1,current_fact/2,current_fact/1,assertz_fact/2,assertz_fact/1,asserta_fact/2,asserta_fact/1]).

:- use_module(engine(exceptions),[abort/0,halt/1,halt/0,throw/1,intercept/3,catch/3]).

:- use_module(engine(io_aux),[display_term/1,display_list/1,display_string/1,inform_user/1,debug/1,message/1,note/1,warning/1,error/1,message_lns/4,message/2]).

:- use_module(engine(io_basic),[displayq/1,displayq/2,display/1,display/2,getct1/2,getct/2,code_class/2,tab/1,tab/2,nl/0,nl/1,put_code/1,put_code/2,skip_code/1,skip_code/2,peek_code/1,peek_code/2,get1_code/1,get1_code/2,get_code/1,get_code/2]).

:- use_module(engine(prolog_flags),[nofileerrors/0,fileerrors/0,nogc/0,gc/0,prompt/2,pop_prolog_flag/1,push_prolog_flag/2,prolog_flag/3,current_prolog_flag/2,set_prolog_flag/2]).

:- use_module(engine(streams_basic),[io_mode/1,stream/1,sourcename/1,absolute_file_name/7,absolute_file_name/2,stream_code/2,current_stream/3,clearerr/1,flush_output/0,flush_output/1,line_position/2,line_count/2,character_count/2,current_output/1,set_output/1,current_input/1,set_input/1,close/1,open/3]).

:- use_module(engine(system_info),[ciaolibdir/1,current_module/1,this_module/1,get_os/1,get_arch/1]).

:- use_module(engine(term_basic),['C'/3,copy_term/2,=.. /2,functor/3,arg/3,= /2]).

:- use_module(engine(term_compare),[compare/3,@>= /2,@> /2,@=< /2,@< /2,\== /2,== /2]).

:- use_module(engine(term_typing),[type/2,ground/1,atomic/1,number/1,float/1,integer/1,atom/1,nonvar/1,var/1]).

n(0).

n(X) :-
        n(Y),
        X is Y+1.

fib(N,F) :-
        n(N),
        fibaux(0,1,N,F).

fibaux(Fact,_Fpost,0,Fact).

fibaux(Fact,Fpost,N,F) :-
        N>0,
        N1 is N-1,
        Nfib is Fact+Fpost,
        fibaux(Fpost,Nfib,N1,F).

do_fib :-
        statistics(runtime,_1),
        N=1000,
        fib(N,F),
        fib(K,F),
        statistics(runtime,[_2|T]),
        write('Answer to fibonacci is '),
        write(K),
        write(' (Should be '),
        write(N),
        write(').'),
        nl,
        write('Used '),
        write(T),
        write(' milliseconds'),
        nl.

