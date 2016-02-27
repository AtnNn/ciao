:- module(bundle_registry_base, [], [assertions]).

:- doc(title, "Registry of Ciao Bundles (common definitions)").
:- doc(author, "Edison Mera (original author)").
:- doc(author, "Jose F. Morales").

:- use_module(library(terms)).

:- export(bundle_db_dir/2).
:- pred bundle_db_dir(LibDir, DbDir)
   # "Given the Ciao library directory @var{LibDir}, obtain the
      directory for the bundle database @var{DbDir}".

bundle_db_dir(LibDir, DbDir) :-
	atom_concat(LibDir, '/lib/bundle_registry/bundles/', DbDir).

