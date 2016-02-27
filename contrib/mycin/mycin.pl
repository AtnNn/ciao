%%----------------------------------------------------------------------
%%
%% MYCIN PROGRAMMING LIBRARY
%%
%% AUTHOR:  Angel Fernandez Pineda
%% DATE:    FEBRUARY 2000
%%
%%----------------------------------------------------------------------

:- op(700,xfy,[(cf)]).
:- op(1150,fx,[(extern)]).

%%----------------------------------------------------------------------

:- use_module(library('mycin/mycin_rt')).

%%----------------------------------------------------------------------

:- new_declaration(extern/1,off).

%% For internal use only.
:- new_declaration(mycin_export/1,on).

%%----------------------------------------------------------------------

:- load_compilation_module(library('mycin/mycin_tr')).

:- add_sentence_trans(mycin_sentence_tr/3).
:- add_clause_trans(mycin_clause_tr/3).

%%----------------------------------------------------------------------
