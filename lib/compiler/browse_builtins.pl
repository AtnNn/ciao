:- module(browse_builtins, [main/0], []).

:- use_module(library(streams), 
        [open_output/2, close_output/1, open_input/2, close_input/1]).
:- use_module(engine(internals), [builtin_module/1]).
:- use_module(library(read), [read/1]).

main :-
        open_output(engine('builtin_exports.pl'), O),
        write_engine_imports(_),
        close_output(O).

write_engine_imports(M) :-
        builtin_module(M),
        absolute_file_name(engine(M), '', '.itf', '.', ItfFile, _, _),
        open_input(ItfFile, I),
        write_engine_facts(M),
        close_input(I),
        fail.
write_engine_imports(_).

write_engine_facts(M) :-
        repeat,
          read(ITFData),
        ( ITFData = end_of_file, !
        ; ITFData = e(F,A,_,Meta),
            display_term(builtin_export(M, F, A, Meta)),
          fail
        ).
