:- module('devel-grep-expansions', [main/0], [dcg, assertions]).

:- doc(title, "Expansion Grep Tool").
:- doc(author, "Jose F. Morales").

:- doc(module, "A simple tool to locate expansions recursively in the source
   tree").

:- doc(bug, "Search is based on external @tt{grep} command").

:- doc(bug, "Include the output in Ciao manuals (integrate it with
   LPdoc?)").

:- doc(bug, "Identify not only the expansion hooks, but also the
   syntactic flags and definitions (such as changes in the reader
   flags and the operator table)").

:- doc(bug, "Combine with an operator and package index in LPdoc").

:- use_module(library(strings), [get_line/2, write_string/1]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(apply), [maplist/2]).
:- use_module(library(system), [exec/8]).

decls(add_sentence_trans).
decls(add_term_trans).
decls(add_clause_trans).
decls(add_goal_trans).

:- data pkg/3.

clean_db :-
	retractall_fact(pkg(_,_,_)).

:- pred show_decls # "Show a summary of the data in @pred{pkg/3}".
show_decls :-
	findall(t(Prior, Decl, File), pkg(File, Decl, Prior), Ps0),
	sort(Ps0, Ps),
	display('List of packages ordered by increasing priority (lower numbers are applied before)'), nl,
	nl,
	maplist(show_decl, Ps).

show_decl(t(Prior, Decl, File)) :-
	display('  '),
	display(Prior),
	display(' ('),
	display(Decl),
	display(') - '),
	display(File),
	nl.

:- pred search_decls # "Search declarations (specified in
   @pred{decls/1}) in the current directory. The result is collected
   in the @pred{pkg/3} predicate.".

search_decls :-
	( decls(Decl),
	    Cmd = 'grep',
	    atom_concat(['^[^%]*:- *', Decl], Str),
	    Args = ['-r', '-e', Str, '.', '--include=*.pl'],
	    % TODO: exec/3 may not work; it incorrectly breaks the argument at ' '
	    %       Fix system:split_atom/3.
	    exec(Cmd, Args, [], Out, [], false, _, _),
	    process_packages(Out, Decl),
	    fail
	; true
	).

process_packages(S, Decl) :-
	( repeat,
	    get_line(S, L),
	    ( L = end_of_file ->
	        !
	    ; %write_string(">> "),
	      %write_string(L), nl,
	      parse_line(File, Prior, L, _),
	      %display(f(Prior, File)), nl,
	      assertz_fact(pkg(File, Decl, Prior)),
	      fail
	    )
	; true
	).

:- doc(subsection, "Parsing the grep output").

parse_line(File, Prior) -->
	parse_filename(File),
	( find_comma,
	  skip_blanks ->
	    parse_num(Prior)
	; { Prior = none }
	).

parse_filename(File) -->
	parse_filename_(File0), { atom_codes(File, File0) }.

parse_filename_([]) --> ":", !.
parse_filename_([C|Cs]) --> [C], parse_filename_(Cs).

find_comma --> ",", !.
find_comma --> [_], find_comma.

skip_blanks --> " ", !, skip_blanks.
skip_blanks --> [].

parse_num(Num) -->
	parse_num_(Num0), { number_codes(Num, Num0) }.

parse_num_([C|Cs]) --> [C], { C >= 0'0, C =< 0'9 }, !, parse_num_(Cs).
parse_num_([]) --> [].

:- doc(section, "The main predicate").

main :-
	display('Searching language extension declarations...'), nl,
	display('(please wait, this process can take several minutes)'), nl,
	search_decls,
	show_decls,
	clean_db.

%% 
%% Current output of (cd <CIAOSRC>; ciao/etc/devel-grep-expansion)
%% Thu Aug 18 15:06:43 CEST 2011
%% Rev: r13694
%%
% List of packages ordered by increasing priority (lower numbers are applied before)
%
%   110 (add_sentence_trans) - ./ciao/lib/condcomp/condcomp.pl
%   150 (add_sentence_trans) - ./ciao/contrib/doccomments/doccomments.pl
%   208 (add_sentence_trans) - ./ciao/lib/metatypes/metatypes.pl
%   210 (add_sentence_trans) - ./ciao/lib/regtypes/regtypes.pl
%   210 (add_sentence_trans) - ./ciao/lib/runtime_ops/runtime_ops.pl
%   210 (add_sentence_trans) - ./ciaopp/tests/benchs/modular/wms2/mwsm2_out.pl
%   310 (add_sentence_trans) - ./ciao/lib/dcg/dcg.pl
%   310 (add_sentence_trans) - ./ciaopp/tests/benchs/modular/wms2/mwsm2_out.pl
%   320 (add_sentence_trans) - ./ciao/library/make/make.pl
%   330 (add_sentence_trans) - ./ciao/library/menu/menu.pl
%   340 (add_sentence_trans) - ./ciao/contrib/clpfd/indexicals.pl
%   350 (add_sentence_trans) - ./ciao/lib/optparse/optparse.pl
%   500 (add_term_trans) - ./ciao/bugs/Pending/compmod_deps/a.pl
%   550 (add_sentence_trans) - ./ciao/contrib/contextcore/contextcore.pl
%   605 (add_sentence_trans) - ./ciao/contrib/monad/monad.pl
%   610 (add_goal_trans) - ./ciao/library/fsyntax/fsyntax.pl
%   610 (add_sentence_trans) - ./ciao/library/fsyntax/fsyntax.pl
%   620 (add_sentence_trans) - ./ciao/contrib/lazy/lazy.pl
%   630 (add_goal_trans) - ./ciao/library/argnames/argnames.pl
%   630 (add_sentence_trans) - ./ciao/library.development/argnamesvv/argnamesvv.pl
%   630 (add_sentence_trans) - ./ciao/library/argnames/argnames.pl
%   630 (add_term_trans) - ./ciao/library.development/argnamesvv/argnamesvv.pl
%   630 (add_term_trans) - ./ciao/library/argnames/argnames.pl
%   640 (add_sentence_trans) - ./ciao/contrib/regexp/regexp.pl
%   710 (add_sentence_trans) - ./ciao/contrib/transactions/transactions.pl
%   710 (add_term_trans) - ./ciao/contrib/transactions/transactions.pl
%   750 (add_clause_trans) - ./ciao/contrib/block/block.pl
%   750 (add_clause_trans) - ./ciao/contrib/mycin_rulebase/mycin_rulebase.pl
%   750 (add_clause_trans) - ./ciao/library/andorra/andorra.pl
%   750 (add_clause_trans) - ./ciao/library/id/id.pl
%   750 (add_goal_trans) - ./ciao/contrib/fd/fd.pl
%   750 (add_goal_trans) - ./ciao/contrib/hmtypes_check/hmtypes_check.pl
%   750 (add_goal_trans) - ./ciao/contrib/xml_path/xml_path.pl
%   750 (add_goal_trans) - ./ciao/library/clpq/clpq.pl
%   750 (add_goal_trans) - ./ciao/library/clpq/clpq_src.pl
%   750 (add_goal_trans) - ./ciao/library/clpr/clpr.pl
%   750 (add_goal_trans) - ./ciao/library/clpr/clpr_src.pl
%   750 (add_sentence_trans) - ./ciao/contrib/ams/ams.pl
%   750 (add_sentence_trans) - ./ciao/contrib/and_sim_plan/and_sim_plan.pl
%   750 (add_sentence_trans) - ./ciao/contrib/asp/asp.pl
%   750 (add_sentence_trans) - ./ciao/contrib/block/block.pl
%   750 (add_sentence_trans) - ./ciao/contrib/catcher/catcher.pl
%   750 (add_sentence_trans) - ./ciao/contrib/chip/chip.pl
%   750 (add_sentence_trans) - ./ciao/contrib/clpfd/clpfd.pl
%   750 (add_sentence_trans) - ./ciao/contrib/colp/colp.pl
%   750 (add_sentence_trans) - ./ciao/contrib/contextual/contextual.pl
%   750 (add_sentence_trans) - ./ciao/contrib/costmodel/costmodel.pl
%   750 (add_sentence_trans) - ./ciao/contrib/hmtypes_check/hmtypes_check.pl
%   750 (add_sentence_trans) - ./ciao/contrib/lprolog/lprolog.pl
%   750 (add_sentence_trans) - ./ciao/contrib/gsl_imports/gsl_imports_dummy.pl
%   750 (add_sentence_trans) - ./ciao/contrib/mycin_rulebase/mycin_rulebase.pl
%   750 (add_sentence_trans) - ./ciao/contrib/pcpe_rtquery/pcpe_rtquery.pl
%   750 (add_sentence_trans) - ./ciao/contrib/tabling/CCAT/CCAT.pl
%   750 (add_sentence_trans) - ./ciao/contrib/tabling/CCall/CCall.pl
%   750 (add_sentence_trans) - ./ciao/contrib/tabling/MVV/MVV.pl
%   750 (add_sentence_trans) - ./ciao/contrib/tabling/SimulatedCHAT/SimulatedCHAT.pl
%   750 (add_sentence_trans) - ./ciao/contrib/tabling/memo/memo.pl
%   750 (add_sentence_trans) - ./ciao/contrib/tabling/tabling.pl
%   750 (add_sentence_trans) - ./ciao/contrib/types.old/types_basic.pl
%   750 (add_sentence_trans) - ./ciao/library.development/decl_io/expansion/dec_io.pl
%   750 (add_sentence_trans) - ./ciao/library/actmods/actmods.pl
%   750 (add_sentence_trans) - ./ciao/library/agent/agent.pl
%   750 (add_sentence_trans) - ./ciao/library/agents/agents.pl
%   750 (add_sentence_trans) - ./ciao/library/andorra/andorra.pl
%   750 (add_sentence_trans) - ./ciao/library/bf/af.pl
%   750 (add_sentence_trans) - ./ciao/library/bf/afall.pl
%   750 (add_sentence_trans) - ./ciao/library/bf/bf.pl
%   750 (add_sentence_trans) - ./ciao/library/bf/bfall.pl
%   750 (add_sentence_trans) - ./ciao/library/chr/chr.pl
%   750 (add_sentence_trans) - ./ciao/library/factsdb/factsdb.pl
%   750 (add_sentence_trans) - ./ciao/library/factsdb/static/cache.pl
%   750 (add_sentence_trans) - ./ciao/library/factsdb/window/cache.pl
%   750 (add_sentence_trans) - ./ciao/library/id/id.pl
%   750 (add_sentence_trans) - ./ciao/library/indexer/indexer.pl
%   750 (add_term_trans) - ./ciao/contrib/asp/asp.pl
%   750 (add_term_trans) - ./ciao/library/agents/agents.pl
%   760 (add_clause_trans) - ./ciao/library/fuzzy/fuzzy.pl
%   760 (add_goal_trans) - ./ciao/contrib/dclp/and/and.pl
%   760 (add_goal_trans) - ./ciao/contrib/gecode/gecode.pl
%   760 (add_sentence_trans) - ./ciao/library/fuzzy/development/fuzzy/paco/fuzzy.pl
%   760 (add_sentence_trans) - ./ciao/library/fuzzy/development/fuzzy/work/fuzzy.pl
%   760 (add_sentence_trans) - ./ciao/library/fuzzy/fuzzy.pl
%   805 (add_sentence_trans) - ./ciao/lib/resprof/resprof.pl
%   810 (add_goal_trans) - ./ciao/lib/resdefs/predefprf/prf_costcenter/prf_costcenter.pl
%   810 (add_sentence_trans) - ./ciao/lib/resdefs/predefprf/prf_costcenter/prf_costcenter.pl
%   820 (add_sentence_trans) - ./ciao/lib/resdefs/resources_decl.pl
%   820 (add_sentence_trans) - ./ciaopp/resources/tests/examples/polytrust_pa.pl
%   830 (add_goal_trans) - ./ciao/lib/resdefs/resdefs.pl
%   830 (add_sentence_trans) - ./ciao/lib/resdefs/resdefs.pl
%   910 (add_sentence_trans) - ./ciao/lib/attr/attr.pl
%   1010 (add_sentence_trans) - ./ciao/lib/foreign_interface/foreign_interface.pl
%   1020 (add_sentence_trans) - ./ciao/lib/foreign_interface/foreign_interface.pl
%   1110 (add_goal_trans) - ./ciao/library/persdb_mysql/persdb_mysql.pl
%   1110 (add_sentence_trans) - ./ciao/library/persdb/persdb.pl
%   1110 (add_sentence_trans) - ./ciao/library/persdb/persdb_ll.pl
%   1110 (add_sentence_trans) - ./ciao/library/persdb_mysql/persdb_mysql.pl
%   1110 (add_sentence_trans) - ./ciao/library/persdb_mysql_op/persdb_mysql_op.pl
%   1110 (add_sentence_trans) - ./ciao/library/persdb_odbc/persdb_sql.pl
%   1150 (add_sentence_trans) - ./ciao/library.development/transactions/transactions.pl
%   1150 (add_sentence_trans) - ./ciao/library/det_hook/det_hook.pl
%   1150 (add_term_trans) - ./ciao/library.development/transactions/transactions.pl
%   8010 (add_sentence_trans) - ./ciao/library/dialect/yap_compat.pl
%   8110 (add_clause_trans) - ./ciao/library/class/class.pl
%   8110 (add_clause_trans) - ./ciao/library/interface/interface.pl
%   8110 (add_clause_trans) - ./ciao/library/objects/objects.pl
%   8110 (add_sentence_trans) - ./ciao/library/class/class.pl
%   8110 (add_sentence_trans) - ./ciao/library/interface/interface.pl
%   8110 (add_sentence_trans) - ./ciao/library/objects/objects.pl
%   8150 (add_clause_trans) - ./ciao/contrib/codecoverage/codecoverage.pl
%   8150 (add_sentence_trans) - ./ciao/contrib/codecoverage/codecoverage.pl
%   8210 (add_goal_trans) - ./ciao/contrib/profiler/profiler.pl
%   8210 (add_sentence_trans) - ./ciao/contrib/profiler/profiler.pl
%   8310 (add_goal_trans) - ./ciao/lib/rtchecks/rtchecks.pl
%   8310 (add_sentence_trans) - ./ciao/lib/rtchecks/rtchecks.pl
%   8410 (add_goal_trans) - ./ciao/lib/inliner/inliner.pl
%   8410 (add_sentence_trans) - ./ciao/lib/inliner/inliner.pl
%   8510 (add_clause_trans) - ./ciao/contrib/srcbyrd/srcbyrd.pl
%   8510 (add_clause_trans) - ./ciao/lib/debug.pl
%   8510 (add_clause_trans) - ./ciao/lib/nodebug.pl
%   8510 (add_clause_trans) - ./ciao/lib/trace.pl
%   8510 (add_sentence_trans) - ./ciao/lib/debug.pl
%   8510 (add_sentence_trans) - ./ciao/lib/nodebug.pl
%   8510 (add_sentence_trans) - ./ciao/lib/trace.pl
%   9010 (add_goal_trans) - ./ciao/contrib/debugpred/debugpred.pl
%   9010 (add_sentence_trans) - ./ciao/contrib/debugpred/debugpred.pl
%   9010 (add_sentence_trans) - ./ciaopp/plai/notrace.pl
%   9010 (add_sentence_trans) - ./ciaopp/poly_spec/notime_stats.pl
%   9010 (add_sentence_trans) - ./ciaopp/spec/no_debug.pl
%   9010 (add_sentence_trans) - ./ciaopp/spec/nomem_usage.pl
%   9010 (add_sentence_trans) - ./ciaopp/spec/nounfold_stats.pl
%   9010 (add_sentence_trans) - ./ciaopp/tests/benchs/modular/ciaopp-pe/spec/nomem_usage_.pl
%   9050 (add_sentence_trans) - ./ciao/library/tracing/tracing.pl
%   9060 (add_sentence_trans) - ./ciao/library/byrdbox/byrdbox.pl
%   9110 (add_sentence_trans) - ./ciaopp/resources/predefres/res_exectime_hlm/profiler/calibuil/calibuil_bench.pl
%   9110 (add_sentence_trans) - ./ciaopp/resources/tests/tests.pl
%   9910 (add_clause_trans) - ./ciao/lib/attr/examples/myfreeze_co.pl
%   9910 (add_clause_trans) - ./ciao/library/expander/expander.pl
%   9910 (add_clause_trans) - ./ciao/library/show_trans/show_trans.pl
%   9910 (add_sentence_trans) - ./ciao/library/expander/expander.pl
%   9910 (add_sentence_trans) - ./ciao/shell/tests/display_clauses.pl
