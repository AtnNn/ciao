:- module(intrunsolver, [buildRunSolver/1]).


buildRunSolver(S) :- 

%write(S,':- export(lparse_parm/1).\n'),
%write(S,':- export(skep_parm/1).\n'),
%write(S,':- export(asp_prolog_parm/1).\n'),
%write(S,':- export(trace_parm/1).\n'),

write(S,'lparse_parm([''-d '',all,'' -W '',none, '' '']).'), nl(S),
write(S,'skep_parm([''0'',''skeptical'',''-asp_prolog'']).'), nl(S),
write(S,'asp_prolog_parm([''-asp_prolog'']).\n'),
write(S,'trace_parm([''0'',''-trace'',''-asp_prolog'']).\n'),
write(S,'lparse_exe(''/home/grad3/okhatib/lparse-1.0.17/src/lparse '').\n'),
write(S,'smodel_exe(''/home/grad3/okhatib/smodels-2.28/smodels '').\n'),

write(S,'%------------------------------------------------'), nl(S),
write(S,'%running Smodels System:'), nl(S),
write(S,'%------------------------'), nl(S),
write(S,'runSolver(No,OSM,Type) :- '), nl(S),
write(S,'   aspFileName(AspF),\n'),
write(S,'	children(L), (L=[]->F=AspF;atom_concat(AspF,''1'',F)),\n'),
%write(S,'display(''runSolver on F=''),display(F),nl,\n'),
write(S,'   lparse_set(LPARM2,_),\n'),
write(S,'   lparse_parm(LPARM1),\n'),
write(S,'   append(LPARM1,LPARM2, LPARM3),\n'),
write(S,'   append(LPARM3,[F], LPARM),\n'),
write(S,'   atoms_concat(LPARM, LP),\n'),
%write(S,'	display(''lparse=''), display(LP), nl,\n'),
write(S,'	this_module(M),\n'),
write(S,'	ERR=''running lparse: lparse does not exist'',\n'),
write(S,'   number_atom(No,Num),\n'),
%write(S,'   get_parm(Type,SPARM1),\n'),
write(S,'   lparse_set(_,SPARM2),\n'),
%write(S,'   append(SPARM1, SPARM2, SPARM),\n'),
write(S,'	atoms_concat(SPARM2,SP),\n'),
%write(S,'  display(''SP=''), display(SP), nl,\n'),
write(S,'	smodel_exe(SMEXE),\n'),
%write(S,'      display(''Type=''), display(Type),nl,\n'),
write(S,'	(Type=4->atoms_concat([SMEXE,'' -w '', SP],X1);\n'),
write(S,'	atoms_concat([SMEXE,'' '',Num,'' '',SP],X1)),\n'),
%write(S,'      display(SMEXE),nl,\n'),
write(S,'	lparse_exe(LPEXE),\n'),
write(S,'	atoms_concat([LPEXE,LP,'' | '',X1],RUNSMODELS),\n'),
%write(S,'   atoms_concat([''/home/grad3/okhatib/lparse-1.0.17/src/lparse '',LP, '' | /home/grad3/okhatib/smodels-2.28/smodels '',Num,'' '',SP],RUNSMODELS),\n'),
%write(S,'	display(RUNSMODELS),nl,\n'),
write(S,'   catch(popen(RUNSMODELS,read,OSM),_,err(0,M,ERR,0)).\n'),

write(S,'stop_solver :-\n'),
write(S,'	this_module(M), \n'),
write(S,'	aspFileName(AspF),\n'),
%write(S,'	aspFileName(AspF,_,_),\n'),
write(S,'	atom_concat(''Smodels when reading asp file: '',AspF, ERR),\n'),
write(S,'	err(0,M,ERR,0).\n'),

write(S,'get_parm(asp_prolog,P) :- asp_prolog_parm(P).\n'),
write(S,'get_parm(skep,P) :- skep_parm(P).\n'),
write(S,'get_parm(trace,P) :- trace_parm(P).\n\n'),

write(S,'get_one_line(O,String) :-\n'),
write(S,'	this_module(M),\n'),
write(S,'   catch(get_line(O,String), E, err(E,M,'' in read_lparse_send_smodels'',0)).\n\n').

