% op definition to allow system to read ASP rules.
% allows programmer to write: not, ..

% for conditional or qualification:
% for set of integers or characters
:- op(550, xfy, '..').
:- op(550, xfy, :).
% for !=
:- op(700, xf, !).
% allows programmer to write: not, ...
:- op(990, fx, not).
% for '{'
:- op(1100, xfy, ^).
:- op(1100, fy, ^).
% for '}'
:- op(1102, xfy, #).
:- op(1102, xf, #).
%:- op(120, fx, #).
% for '['
:- op(1100, xfy, &).
:- op(1100, fy, &).
% for ']'
:- op(1102, xfy, @).
:- op(1102, xf, @).
% for set of integers or characters
% for !=
% ----------------------------------------------------------------

% ASCII code for some characters used in the system.
% defining some char ascii codes: -------------------------------
:- push_prolog_flag(unused_pred_warnings,no).
tabb(9).
space(32).
double_quote(34).
hash(35).
percent(37).
andc(38).
single_quote(39).
lpar(40).
left_par(40).
right_par(41).
rpar(41).
comma(44).
dot(46).
period(46).
slash(47).

ats(64).

left_bracket(91).
backSlash(92).
right_bracket(93).
power(94).
under_score(95).

left_brace(123).
right_brace(125).
:- pop_prolog_flag(unused_pred_warnings).

% ---------------------------------------------
