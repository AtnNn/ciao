:- package(ll).
:- use_package(library(persdb(persdb_decl))).

:- use_module(library(persdb(persdbrt)), [%donotredefinebuiltins
        passerta_fact/1, 
        passertz_fact/1, 
        pretract_fact/1,
        init_persdb/0, 
        initialize_db/0,
        make_persistent/2,
        update_files/0,
        update_files/1]).

:- initialization(init_persdb).

:- load_compilation_module(library(persdb(persdbtr))).
:- add_sentence_trans(persistent_tr/2).
