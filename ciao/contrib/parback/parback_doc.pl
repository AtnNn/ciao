:- use_package(assertions).


:- doc(nodoc, assertions).

:- doc(filetype, package).

:- doc(title,  "And-parallel execution").

:- doc(author, "Pablo Chico de Guzman").
:- doc(author, "Amadeo Casas"||
                   " (@tt{http://www.ece.unm.edu/~amadeo},"||
                   " University of New Mexico").

:- doc(module, "This library allows and-parallel execution of
                    goals in (Herbrand-)independent fashion. It
                    resembles the execution rules of &-Prolog. It
                    performs memoization and parallel backtracking
                    execution").

:- doc(usage, "The PARALLEL_EXECUTION flag must be set to \"yes\"
                   in order to compile the engine with support for the
                   and-parallel execution of goals. Concurrency
                   primitives are defined in the @tt{apll_parback}
                   library. ").

:- use_module(library(parback(parback_rt))).


