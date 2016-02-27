:- module(header, [buildHeader/4]).

:- use_module(library(write)).

buildHeader(S, ModuleName, _, AbsAspFileName) :-

	write(S, ':- module('), write(S, ''''),
	write(S, ModuleName), write(S, ''''),
	write(S,', [compute/2]).'), nl(S), nl(S),

	write(S,':- export(assert/1).\n'),
	write(S,':- export(retract/1).\n'),
	write(S,':- export(model/1).\n'),
	write(S,':- export(change_parm/1).\n'),
	write(S,':- export(assert_nb/1).\n'),
	write(S,':- export(retract_nb/1).\n'),
	write(S,':- export(reset_asp/0).\n'), 
%	write(S,':- export(justify_atoms/2).\n'), 
%	write(S,':- export(justify_all/1).\n'), 
%	write(S,':- export(asp_trace/1).\n'), 
%	write(S,':- export(gen_graph/3).\n'),
	write(S,':- export(release/1).\n'), 
%	write(S,':- export(quit/1).\n'), nl(S),

	write(S,':- use_package([objects]).'), nl(S),
	write(S,':- use_module(library(lists)).'), nl(S),
	write(S,':- use_module(library(strings)).'), nl(S),
	write(S,':- use_module(library(system)).'), nl(S),
	write(S,':- use_module(library(atom2term)).'), nl(S),
	write(S,':- use_module(library(operators)).'), nl(S),
	write(S,':- use_module(library(streams)).'), nl(S),
	write(S,':- use_module(engine(internals)).'), nl(S),
	write(S,':- use_module(library(write)).'), nl(S),
	write(S,':- use_module(library(davinci)).'), nl(S),
	write(S,':- use_module(library(terms_check)).'), nl(S),
	write(S,':- use_module(library(vndict)).'), nl(S),
	write(S,':- use_module(library(iso_byte_char)).'), nl(S),
	write(S,':- use_module(library(terms)).'), nl(S),
%	write(S,':- use_module(library(term_to_string)).'), nl(S),
  write(S,':- use_module(library(filenames)).\n\n'),

%	atom_concat(INameNoExt, 'j', JustClass),
%	atom_concat(INameNoExt, 'atom', AtomClass),
%	atom_concat(INameNoExt, 'rule', RuleClass),
%	atom_concat(INameNoExt, 'skep', SkepClass),
%	atom_concat(INameNoExt, 'trc', TrcClass),

	write(S, ':- use_class(justclass).\n'),
	write(S, ':- use_class(wellfound).\n'),
%	format(S, "~k",JustClass),
%	write(S, ''').'), nl(S),

	write(S, ':- use_class(atomclass).\n'),
%	write(S, AtomClass),
%	write(S, ''').'), nl(S),

%	write(S, ':- use_class(ruleclass).\n'),
%	write(S, RuleClass),
%	write(S, ''').'), nl(S),

	write(S, ':- use_class(skepclass).\n'),
%	write(S, SkepClass),
%	write(S, ''').'), nl(S),

%	write(S, ':- use_class('''),
%	write(S, TrcClass),
%	write(S, ''').'), nl(S),

	write(S,'self('''),write(S,ModuleName),
	write(S,''').\n\n'),

%	name(AbsAspFileName, AbsAspFileString),
%	cyg2win(AbsAspFileString, WinAspFileString, swap),
%	name(WinAspFileName, WinAspFileString),
	write(S,'aspFileName('),
	format(S,"~k",AbsAspFileName),
%	atom_concat(WinAspFileName,'1',TmpWinFile1),
%	format(S,",~k",TmpWinFile1),
%	atom_concat(WinAspFileName,'2',TmpWinFile2),
%	format(S,",~k",TmpWinFile2),
	write(S,').\n\n'),

	write(S,' :- export(getAspName/1).\n'),
	write(S,'getAspName(F2) :- aspFileName(F),\n'),
	write(S,'	absolute_file_name(F,F1),\n'),
	write(S,'	no_path_file_name(F1,F2).\n\n').
