:- use_package(assertions).


:- doc(nodoc, assertions).

:- doc(filetype, package).

:- doc(title,  "And-parallel execution").

:- doc(author, "Amadeo Casas"||
                   " (@tt{http://www.cs.unm.edu/~amadeo},"||
                   " University of New Mexico)").

:- doc(module, "This library allows and-parallel execution of goals in
                    (Herbrand-)independent fashion. It resembles the
                    execution rules of &-Prolog.").

:- doc(usage, "The AND_PARALLEL_EXECUTION flag must be set to
                   \"yes\" in order to compile the engine with support
                   for the and-parallel execution of
                   goals. Concurrency primitives are defined in the
                   @tt{apll} library. That flag may be set to
                   \"visandor\" to, in addition, add support for
                   VisAndOr's events for deterministic parallel
                   goals.").


:- include(library(andprolog(andprolog_ops))).

:- use_module(library(andprolog(andprolog_rt))).


