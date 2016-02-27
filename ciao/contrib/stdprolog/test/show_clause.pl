%
% show_clause.pl -- showing Prolog clauses, like SICStus portray_clause.pl
% by pts@fazekas.hu at Mon May  8 14:06:05 CEST 2006
%
% Imp: take operator precedence into account when displaying ',' and ';'
% Imp: print the argument of call/1, findall/3 etc. nicely
%
% Dat: prints with one simple call per line
% Dat: example: show_clause((foo(X) :- ((bar(X), true), X=5, !, Y=6, ! ;
%      fail, !, foo ; (bar ; rab), barr ; X=1 -> (Y=3 -> Y=3 -> Y=3), 3=Y),
%      baz(X,Y))).   --> (with `show_clause_indent_spaces('   ').')
%      foo(A) :-
%         (  (  bar(A),
%               true
%            ),
%            A=5, !,
%            B=6, !
%         ;  fail, !,
%            foo
%         ;  (  bar
%            ;  rab
%            ),
%            barr
%         ;  A=1 ->
%            (  B=3 ->
%               B=3 ->
%               B=3
%            ),
%            3=B
%         ),
%         baz(A,B).

% Imp: why? :- set_prolog_flag(language, iso).

%** show_clause(+Clause), where Clause is returned by clause/2.
show_clause(Clause) :-
  % vvv Imp: better printing of '$VAR'(...)
  ( numbervars(Clause, 0, _), % Dat: makes nonvar(Clause) etc.
    ( Clause=(Head:-Body) ->
      ( Body == true ->
        show_clause_head(Head, '.')
      ; show_clause_head(Head, ' :-'),
        show_clause_call(Body, 1, '', 1, '.', yes, no, no)
      )
    ; show_clause_head(Clause, '.')
    )
  ; fail % Dat: because of numbervars
  ).

show_clause_head(Head, Trailer) :-
  show_clause_simple_call(Head),
  write(Trailer), nl.

show_clause_simple_call(Call) :-
  write_term(Call, [quoted(true),numbervars(true),ignore_ops(false)]).

%** show_clause_call(+Call, +IndentStep1, +AfterIndent1, +IdentStep2,
%** +Trailer, +CommaOK, +SemicolonOK, +ArrowOK).
%** Dat: AI1 is an atom to print
%** Dat: *OK might be 'yes' or 'no'.
%** Dat: IndentStep1 is for the first line, IndentStep2 is for the following
%**      lines
show_clause_call((Call,'!'), IS1, AI1, IS2, Trailer, yes, _SemicolonOK, _ArrowOK) :- !,
  atom_concat(', !', Trailer, Trailer2),
  show_clause_call(Call, IS1, AI1, IS2, Trailer2, no, no, no).
show_clause_call((C1,C2), IS1, AI1, IS2, Trailer, CommaOK, _SemicolonOK, _ArrowOK) :- !,
  ( C2 = ('!', C2B) -> Trailer1=', !,'
  ; C2B=C2, Trailer1=','
  ),
  ( CommaOK=yes ->
    show_clause_call(C1,  IS1, AI1, IS2, Trailer1, no, no, no),
    show_clause_call(C2B, IS2, '',  IS2, Trailer, yes, no, no)
  ; show_clause_indent(IS1), write(AI1),
    show_clause_add_rest('(', WritePre), write(WritePre),
    IS2P is IS2+1,
    show_clause_call(C1,  0   , AI1, IS2P, Trailer1, no, no, no), % Dat: value of SemicolonOK doesn't matter here
    show_clause_call(C2B, IS2P, '',  IS2P, '', yes, no, no),
    show_clause_indent(IS2), write(')'), write(Trailer), nl
  ).
show_clause_call((C1->C2), IS1, AI1, IS2, Trailer, _CommaOK, _SemicolonOK, ArrowOK) :- !,
  ( ArrowOK=yes ->
    show_clause_call(C1,  IS1, AI1, IS2, ' ->', no, no, no),
    show_clause_call(C2,  IS2, '',  IS2, Trailer, yes, no, yes)
  ; show_clause_indent(IS1), write(AI1),
    show_clause_add_rest('(', WritePre), write(WritePre),
    IS2P is IS2+1,
    show_clause_call(C1,  0   , AI1, IS2P, ' ->', no, no, no), % Dat: value of SemicolonOK doesn't matter here
    show_clause_call(C2,  IS2P, '',  IS2P, '', yes, no, yes),
    show_clause_indent(IS2), write(')'), write(Trailer), nl
  ).
show_clause_call((C1;C2), IS1, AI1, IS2, Trailer, _CommaOK, SemicolonOK, _ArrowOK) :- !,
  show_clause_add_rest(';', WritePreS),
  ( SemicolonOK=yes ->
    show_clause_call(C1, IS1, AI1, IS2, '', yes, no, yes),
    IS2M is IS2-1,
    show_clause_call(C2, IS2M, WritePreS, IS2, Trailer, yes, yes, yes)
  ; show_clause_indent(IS1), write(AI1),
    show_clause_add_rest('(', WritePre), write(WritePre),
    IS2P is IS2+1,
    show_clause_call(C1,  0   , '', IS2P, '', yes, no, yes),
    show_clause_call(C2,  IS2 , WritePreS, IS2P, '', yes, yes, yes),
    show_clause_indent(IS2), write(')'), write(Trailer), nl
  ).
show_clause_call(Call, IS1, AI1, _IS2, Trailer, _CommaOK, _SemicolonOK, _ArrowOK) :-
  show_clause_indent(IS1),  write(AI1),
  show_clause_simple_call(Call),
  write(Trailer), nl.

%** Configuration option
show_clause_indent_spaces('   ').

show_clause_indent(I) :-
  show_clause_indent_spaces(Spaces),
  show_clause_indent(I, Spaces).

show_clause_indent(I, Spaces) :-
  ( I > 0 -> write(Spaces), I1 is I-1, show_clause_indent(I1, Spaces)
  ; true
  ).

%** show_clause_add_rest(?Atom1, +Atom2): Atom2 is Atom1 with the appropriate
%** amount of spaces added (appended).
%** @assert atom_length(Atom1, 1)
show_clause_add_rest(Atom1, Atom2) :-
  show_clause_indent_spaces(Spaces),
  atom_length(Spaces, LSpaces),
  ( LSpaces < 2 -> Atom2=Atom1
  ; LSpaces = 2 -> atom_concat(Atom1, ' ', Atom2)
  ; atom_codes(Spaces, [_|CSpaces]),
    atom_codes(Atom1, [Code1]), % Imp: release this restriction
    atom_codes(Atom2, [Code1|CSpaces])
  ).
