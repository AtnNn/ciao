:- use_package(assertions).

:- comment(filetype, part).

:- comment(title,"PART VII - Ciao Prolog extensions").

:- comment(module,"The libraries documented in this part extend the
	Ciao language in several different ways. The extensions include:
	@begin{itemize}
	@item pure Prolog programming (although someone might view this
		as a restriction rather than an extension);
	@item feature terms or @index{records} (i.e., structures with
		names for each field);
	@item parallel (&-Prolog) programming;
	@item functional syntax;
	@item global variables;
	@item @tt{setarg} and @tt{undo};
	@item active modules;
	@item breadth-first execution;
	@item constraint logic programming;
	@item object oriented programming.
	@end{itemize}
").

%% Note: the preprocessor will eventually be able to put the use_module 
%% decls in! (or we can write a browser).

main.

%% --------------------------------------------------------------------------- 
:- comment(version_maintenance,dir('../../version')).
%% --------------------------------------------------------------------------- 

