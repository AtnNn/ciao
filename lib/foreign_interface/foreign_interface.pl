:- use_package([assertions,regtypes]).

:- use_module(library('foreign_interface/foreign_interface_properties')).

:- new_declaration(use_foreign_source/1,on).
:- new_declaration(use_foreign_source/2,on).
:- new_declaration(use_foreign_library/1,on).
:- new_declaration(use_foreign_library/2,on).
:- new_declaration(extra_compiler_opts/1,on).
:- new_declaration(extra_compiler_opts/2,on).
:- new_declaration(extra_linker_opts/1,on).
:- new_declaration(extra_linker_opts/2,on).


