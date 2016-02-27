:- package(chr).
:- include(library(chr(ciao(chr_rt)))).

:- load_compilation_module(library(chr(ciao(chr_tr)))).
:- add_sentence_trans(chr_compile_module/3, 2010).

% TODO: Priorities are not enough to make it work with other
%       translations, such as fsyntax. See "Modular Extensions for
%       Modular (Logic) Languages (LOPSTR'11)" paper for details.
