%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMMENTS                                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newline('*** --------------------------------------------------------').

:- discontiguous my_target_comment/2.

% some common comments
my_target_comment(clean,          'delete compilation auxiliary files').
my_target_comment(realclean,      'delete all unnecesary files').
my_target_comment(distclean,      'delete all files which can be easily generated automatically').
my_target_comment(braveclean,     'delete absolutely all files that can be generated automatically').
display_comment(TextList) :-
	list(TextList),!,
	display_comment(~atom_concat(TextList)).

display_comment(Text) :-
	newline(NL),
	display(NL),nl,
	display('*** '), display(Text), nl,
	display(NL),nl.

target_comment(X) :- 
	my_target_comment(X,Text),
	display_comment(Text).

lpmake_subdirs(Dirs, Action) :-
	make_subdirs(~lpmake, Dirs, Action).

command_subdir(Dir, Command) :-
	display(~atom_concat([Dir, ': Executing `', Command, '`'])),nl,
	do(['cd ', Dir, '&& ', Command], nofail).

make_subdir(Make, Dir, Action) :-
	atom_codes(PWD, ~getenvstr('PWD')),
	display(~atom_concat([~lpmake, '->', Make,': Entering `', PWD, '/', Dir, '`'])),nl,
	atom_concat([Make, ' ', Action], Command),
	command_subdir(Dir, Command),
	display(~atom_concat([~lpmake, '<-', Make,': Leaving  `', PWD, '/', Dir, '`'])),nl.

make_subdirs(_Make, [], _Action).
make_subdirs(Make, [Dir|Dirs], Action) :-
	make_subdir(Make, Dir, Action),
        make_subdirs(Make, Dirs, Action).

finally(G, F) :-
	(G -> Ok = 1; Ok = 0),
	F,
	Ok = 1.
