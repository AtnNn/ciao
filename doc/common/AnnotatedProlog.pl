:- use_package(assertions).

:- comment(filetype, part).

:- comment(title,"PART V - Annotated Prolog library (assertions)").

:- comment(module,"Ciao allows to annotate the program code with
	@em{assertions},
	much in the style of the @em{literate programming}. Assertions
	can be used to document predicates (and modules and whole
        applications) and will be used by the Ciao preprocessor to
	debug and optimize the program, and by the Ciao documenter to
	build the program manual.").

%% Note: the preprocessor will eventually be able to put the use_module 
%% decls in! (or we can write a browser).

main.

%% --------------------------------------------------------------------------- 
:- comment(version_maintenance,dir('../../version')).
%% --------------------------------------------------------------------------- 

