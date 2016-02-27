:- module(intdefine,[buildDefine/1]).

buildDefine(S) :-


write(S,'comma(44).'), nl(S),
write(S,'period(46).'), nl(S),
write(S,''), nl(S),
write(S,'lpar(40).'), nl(S),
write(S,'rpar(41).'), nl(S),
write(S,'under_score(95).'), nl(S),
write(S,''), nl(S),



%write(S,':- initialization(start).'), nl(S),
write(S,''), nl(S),
write(S,'% atom: store atom index, atom(index,name)'), nl(S),
write(S,'% rule : store rule index, rule(Index,head,body)'),nl(S),
write(S,'% model: store models, model(no,atom_name, value, just'), nl(S),
write(S,'% skepAtoms/2: store skeptical atoms\n'),
write(S,'% ans/2:stores answer models list for skeptical coomputation\n'),

write(S,':- dynamic atom/2, rule/3, model/4.'), nl(S),
write(S,':- dynamic asp_model/3, uf/2, rr/1, comp/2.'), nl(S),
write(S,':- dynamic lparse_set/2.'), nl(S),
write(S,':- dynamic computed/1, rule_computed/1.'), nl(S),
write(S,':- dynamic graph_list/1.\n'),

write(S,'% smodels_term_status: flag of end_of_state.\n'),
write(S,':- dynamic smodels_term_status/1.\n'),


%write(S,':- export(lparse_set/2).\n'),
%write(S,':- export(lparse_parm/1).\n'),
%write(S,':- export(trace_parm/1).\n'),
%write(S,':- export(fn/3).\n'),

write(S,'% mftime: used to store time of last change for ASP program\n'),
write(S,'% state: record state history\n'),
write(S,':- data mftime/1.\n'),
write(S,'% ans: used in skeptical computation\n'),
write(S,':- data ans/2.\n'),
write(S,'% end_model: store if all models is stored in classes.\n'),
write(S,':- data end_models/1.\n'),

%write(S,'% inp/1, out/1, normal/1: access uDrawGraph\n'),
%write(S,':- data inp/1, out/1, normal/1.\n\n'),

% for set of integers or characters
write(S,':- op(550, xfy, ''..'').'), nl(S),
write(S,':- op(550, xfy, :).'), nl(S),
% for !=
write(S,':- op(700, xf, !).'), nl(S),
% allows programmer to write: not, ...
write(S,':- op(990, fx, not).'), nl(S),
write(S,':- op(1100, xfy, ^).'), nl(S),
write(S,':- op(1100, fy, ^).'), nl(S),
write(S,':- op(1102, xfy, #).'), nl(S),
write(S,':- op(1102, yf, #).'), nl(S),
write(S,':- op(1100, xfy, &).'), nl(S),
write(S,':- op(1100, fy, &).'), nl(S),
write(S,':- op(1102, xfy, @).'), nl(S),
write(S,':- op(1102, xf, @).'), nl(S),
write(S,'%:- op(99, fy, ''!'').    % this one might not be needed.'), nl(S),
write(S,'% ----------------------------------------------------------------'), nl(S),
write(S,''), nl(S),
write(S,'% defining some char ascii codes: -------------------------------'), nl(S),
write(S,'percent(37).'), nl(S),
write(S,'space(32).'), nl(S),
write(S,'tabb(9).'), nl(S),
write(S,'double_quote(34).'), nl(S),
write(S,'single_quote(39).'), nl(S),
write(S,'left_par(40).'), nl(S),
write(S,'right_par(41).'), nl(S),
write(S,'dot(46).'), nl(S),
write(S,'left_bracket(91).'), nl(S),
write(S,'right_bracket(93).'), nl(S),
write(S,'left_brace(123).'), nl(S),
write(S,'right_brace(125).'), nl(S),
write(S,'power(94).'), nl(S),
write(S,'hash(35).'), nl(S),
write(S,'andc(38).'), nl(S),
write(S,'ats(64).'), nl(S),
write(S,'% ---------------------------------------------'), nl(S),
write(S,''), nl(S),

write(S,'% fn: stores the filename and other stuff.'), nl(S),

write(S,'% compute_number: a field to store the number should be in the compute'), nl(S),
write(S,'%                 statment.'), nl(S),
write(S,'% old_compute: save old compute values Number and string.'), nl(S),
write(S,'% retracted_list: a list of rules that is retracted from the asp file.'), nl(S),
write(S,':- data compute_number/2.'), nl(S),
write(S,':- data old_compute/2.'), nl(S),
write(S,''), nl(S),

write(S,''), nl(S),

write(S,'% -------------------------------------------'), nl(S),
write(S,'\n'),

write(S,'smodel_parameter(nolookahead).'), nl(S),
write(S,'smodel_parameter(backjump).'), nl(S),
write(S,'% -----------------------------------------------------'), nl(S),

write(S,'\n\n').
