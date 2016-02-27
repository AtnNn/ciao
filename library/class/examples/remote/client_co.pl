:- module(_client,[main/0],ciaopp).

:- use_module(library('actmods/actmodrt')).

:- use_module(library('objects/objects_rt')).

:- op(700,xfx,[new,instance_of,derived_from,interface]).

:- op(900,fx,[destroy]).

:- multifile 'class$used'/2.

:- new_declaration(use_class/1,on).

:- new_declaration(instance_of/2,on).

:- use_module(library('actmods/filebased_locate')).

:- use_module(library('class/examples/stack'),[]).

:- redefining(_7646/_7647).

:- use_class(library('class/examples/stack')).

main :-
        objects_rt:new(X,stack),
        display(X),
        nl,
        'class$call'(X,push(a),client),
        'class$call'(X,pop(E),client),
        display(E),
        nl.

'$static_instance_creation$' :-
        intercept(true,_1,inform_user(['Static instances declared on ',client,'could not be created due to exception: ',_1])).

:- multifile 'class$call'/3.

'$force$rt$info$'(_1) :-
        call(_1).

% added by the actmods expansion:
'class$call'(A,B,C):-
        module_address(stack_remote,_2),
        remote_call(_2,'class$call'(A,B,C)).

'class$used'(A,B):-
        module_address(stack_remote,_2),
        remote_call(_2,'class$used'(A,B)).

:- multifile 'class$remote'/3.

'class$remote'(client,stack,stack_remote).
% end-added

/* plus objects_rt:new/2 should have this clause somewhere:
new(Instance,Class,FromModule):-
	'class$remote'(FromModule,Class,RemoteClass), !,
        module_address(RemoteClass,_2),
        remote_call(_2,new(Instance,Class,FromModule)).
*/



% and if succeeds call
