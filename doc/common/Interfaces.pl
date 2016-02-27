:- use_package(assertions).

:- comment(filetype, part).

:- comment(title,"PART VIII - Interfaces to other languages and systems").

:- comment(module,"The following interfaces to/from Ciao Prolog are
	documented in this part:
	@begin{itemize}
	@item external  interface (e.g., to C);
	@item socket interface;
	@item tcl/tk interface;
	@item Web http-like interface;
	@item persistent predicate databases (interface between the Prolog
		internal database and the external file system);
	@item SQL-like database interface;
	@item Java interface.
	@end{itemize}
").

%% Note: the preprocessor will eventually be able to put the use_module 
%% decls in! (or we can write a browser).

main.

%% --------------------------------------------------------------------------- 
:- comment(version_maintenance,dir('../../version')).
%% --------------------------------------------------------------------------- 

