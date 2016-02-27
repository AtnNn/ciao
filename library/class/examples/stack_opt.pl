:- module(_stack,[],ciaopp).

:- use_module('/home/clip/lib/ciao/ciao-1.5/lib/engine/internals',[last_module_exp/5,'$meta_call'/1]).

:- use_module('/home/clip/lib/ciao/ciao-1.5/library/class/virtual',[mod_exp/5]).

:- use_module('/home/clip/lib/ciao/ciao-1.5/library/class/class_rt',[mod_exp/5,self/2,assert_attr/2,asserta_attr/2,assertz_attr/2,set_attr/2,retract_attr/2,retract_attr_nb/2,retractall_attr/2,current_attr/2,current_attr_nb/2,functor_concat/3]).

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

:- new_declaration(inherit_class/1,on).

:- new_declaration(implements/1,on).

:- new_declaration(inheritable/1,on).

:- new_declaration(public/1,on).

:- new_declaration(virtual/1,on).

:- new_declaration(persistent/1,on).

:- new_declaration(method/1,on).

:- new_declaration(attribute/1,on).

:- new_declaration(super/1,on).

:- op(1150,fx,[public,inheritable,virtual,method]).

:- op(900,fy,[inherited]).

:- load_compilation_module(library('class/class_tr')).

:- add_sentence_trans(class_sentence_translation/3).

:- add_clause_trans(class_clause_translation/3).

:- dynamic storage/1.

'obj$push'(Item,_1) :-
        nonvar(Item),
        asserta_fact(storage(Item)).

'obj$pop'(Item,_1) :-
        var(Item),
        retract_fact(storage(Item)).

'obj$top'(Top,_1) :-
        storage(Top),
        !.

'obj$is_empty'(_1) :-
        storage(_2),
        !,
        fail.

'obj$is_empty'(_1).

:- multifile '$class$'/1.

:- multifile 'class$super'/2.

:- multifile 'class$call'/3.

:- multifile 'class$initial_state'/3.

:- multifile 'class$virtual'/6.

:- multifile 'class$attr_template'/4.

:- multifile 'class$default_cons'/1.

:- multifile 'class$constructor'/4.

:- multifile 'class$destructor'/3.

:- multifile 'class$implements'/2.

:- redefining(mod_exp/5).

'$class$'(stack).

'$tachun$tachun$'(_1) :-
        call(_1).

:- public(is_empty/0).

:- public(top/1).

:- public(pop/1).

:- public(push/1).

:- inheritable(push/1).

:- inheritable(pop/1).

:- inheritable(top/1).

:- inheritable(is_empty/0).

:- attribute(storage/1).

:- method(is_empty/0).

:- method(top/1).

:- method(pop/1).

:- method(push/1).

:- redefining('obj$is_empty'/1).

:- redefining('obj$top'/2).

:- redefining('obj$pop'/2).

:- redefining('obj$push'/2).

:- inheritable(persistent_attribute/1).

:- virtual(persistent_attribute/1).

:- method(persistent_attribute/1).

'$end$$of$$expansion$'.

