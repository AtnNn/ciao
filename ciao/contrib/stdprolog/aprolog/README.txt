README for aprolog
by Péter Szabó <pts@fazekas.hu> at Fri Jan 13 16:01:28 CET 2006

Authors:

-- aprolog was originally written by Miklós Szeredi
-- the aprolog runtime (builtin/*.pl) has been written by several people,
   including R.A. O'Keeefe.
-- this version of aprolog is maintained by Péter Szabó <pts@fazekas.hu>

Features:

++ aprolog is an almost-ISO-standard compliant Prolog implementation
++ aprolog is very small
++ aprolog is for educational purposes (how to implement Prolog)
-- aprolog doesn't have a real module system
++ aprolog has a simplified module system (with `:- module' declarations and
   `:- use_module'), but it works only in the bootstrapped system, i.e. it
   doesn't work in the code consult()ed in the toplevel. The module names
   are injected into the clause bodies by modularize/3 in conv.pl.
-- it is not possible to use `:' in the code to call a predicate defined in
   the specific module
-- aprolog is interpreted: it doesn't have an abstract machine (such as WAM)
-- aprolog is very slow
-- aprolog doesn't have a garbage collector (GC). In fact, it doesn't free()
   unused memory.
-- aprolog programs cannot accept command-line arguments
++ aprolog can do file I/O (using open/3 etc.)
-- aprolog currently runs on UNIX only, but it shouldn't be hard to port it
   to other platforms
-- aprolog cannot report line numbers of { compilation and runtime }
   { errors and warnings }
-- aprolog may fall to an inifinite loop if unification would create cyclic
   terms
-- aprolog integers are 31-bit (30 bit + sign)

The aprolog runtime is written in C. The compiled binary is aprolog.bin.

A main program is a Prolog program directly run by the aprolog runtime. An
example main program is toplevel.pl.

The .pl source of a main program has to be converted to .apl (using e.g.
conv.apl), and the .apl source file can be interpreted directly by the
aprolog runtime.

Restrictions on main programs:

-- They must start with

     :- module(toplevel, []).

-- They mustn't contain any more `:- ...' declarations.

-- They must define the main/0 predicate.

-- The first call in main/0 must be `execution/init_buitins'.

Bootstrapping:

-- Some built-in predicates are defined in builtins.c. Others are written in
   Prolog, and they are defined in bootsrc/*.pl (except for bootsrc/conv.pl).
-- aprolog doesn't have a Prolog parser (read_term + term->clause converter)
   implemented in C. The aprolog.bin binary can read Prolog code defined in
   .apl files. .apl files are text files which define Prolog clauses in a
   tree structure directly suitable for execution by aprolog.bin.
-- The conv.pl Prolog program can be used to transform .pl files to .apl
   files.
-- Bootstrapping aprolog consists of creating the initial bootlib/conv.apl
   (and bootlib/builtin.apl, containing the built-in predicate definitions).
-- To have a working aprolog, you have to compile the C sources to the
   aprolog.bin binary, and you have to bootstrap the bootsrc/*.pl sources
   to bootlib/*.apl.
-- Bootstrapping is not required, since the bootlib/* files are part of the
   source distibution.
-- To rebootstrap aprolog, run `make rebootstrap'. This will regenerated
   the bootlib/*.apl files from the bootsrc/*.pl files.
-- To rebootstrap aprolog from scratch (i.e. not using the distributed
   bootlib/* files), you need another working Prolog implementation.
   If you have SWI-Prolog 5.4.7 or newer installed, just run `make
   rebootstrap-swipl'.

Changes by pts:

-- added the aprolog-run shell script
-- new, improved Makefile
-- Makefile for bootstrapping (aprolog in aprolog)
-- added bootstrapping with SWI-Prolog (for educational purposes)
-- added bootstrapping with SICStus (for educational purposes)
-- compilation triggers less C warnings
-- less compilation warnings (e.g. Singleton variables)
-- small bugfixes, look for %%%% pts %%%%, **** pts **** and #### pts ####
-- introduced meta_predicate()s so now modularize/3 in conv.pl doesn't
   descend into data structures
-- toplevel.pl improvements, better variable printing etc.
-- exception reporting improvements
-- current_prolog_flag(version, Version)
-- stream_property/2 fixes (respect stream aliases and var(Stream))

__EOF__
