
:- use_package([assertions]).

:- comment(title,"Using Ciao inside GNU emacs").

:- comment(subtitle,"@em{An interactive program development environment for Ciao}").
:- comment(subtitle,"@bf{The Ciao System Documentation Series}").
:- comment(subtitle,"Technical Report CLIP X/YY.1").
:- comment(subtitle,"@em{Draft printed on:} @today{}").

:- comment(author,"Manuel Hermenegildo and the CLIP Group").

% :- include(library('ClipAddress')).

:- include(library('Copyright')).

:- comment(summary,"This documents the Ciao emacs interface (or
    @em{mode} in @apl{emacs} terms), which provides a rich, integrated
    user interface to the Ciao program development environment
    components, including the @apl{ciaosh} interactive top level and
    the @apl{ciaopp} preprocessor. While most features of the Ciao
    development environment are available from the command line of the
    preprocessor and the top-level shell, using Ciao inside
    @apl{emacs} is highly recommended, since it greatly facilitates
    the process of editing, compiling, running, and debugging Ciao
    programs.").

:- comment(module,"@include{CiaoMode.lpdoc}").

main.
