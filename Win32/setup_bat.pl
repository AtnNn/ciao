:- module(setup_bat,[main/1,make_bats/1],[]).

:- use_module(utilities).
:- use_module(library(system), [getenvstr/2]).
:- use_module(library(streams), [open_output/2, close_output/1]).

main([EngineAtm]):-
        atom_codes(EngineAtm, EngineStr),
        make_bats(EngineStr).

make_bats(Engine) :-
	setup_mess(['Building prototype .bat files pointing to engine.\n']),
        ( getenvstr('OS',"Windows_NT") ->
            AllArgs = ' %*' 
        ; AllArgs = ' %1 %2 %3 %4 %5 %6 %7 %8 %9'
        ),
        bat_file(BatFile, Head, Tail),
          open_output(BatFile, Out),
          display(Head),
          display_string("@"||Engine),
          display(AllArgs),
          display(Tail),
          close_output(Out),
        fail.
make_bats(_).


 %% bat_file('Win32/bat_skel', Head, Tail):-
 %%         bat_file('lib/compiler', Head, Tail).
bat_file('lib/compiler/bat_skel',
         '@REM Change the path below to the absolute path \c
          of the application\n',
         ' -C -b "/path/to/ciao/application"').
bat_file('shell/ciaosh.bat','',' -C -i -b "$/shell/ciaosh"').
bat_file('ciaoc/ciaoc.bat','',' -C -b "$/ciaoc/ciaoc"').
