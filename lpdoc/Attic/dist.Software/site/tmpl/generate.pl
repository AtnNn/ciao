% generate( [fileurl='~dtm/public_html/CiaoDE/',dirImage='/~dtm/CiaoDE/images/',dirIndex='/~dtm/CiaoDE/',weburl='/~dtm/CiaoDE/'] , '~dtm/CiaoDE/dist/tmpl/' ).

% cp -R --reply=yes site/* ~dtm/public_html/CiaoDE/

% lpmake -m \'makedir.pl\' install_website

:- module(_, _, [assertions]).

:- use_module(library(wp_gen(process_tmpl))).
:- use_module(library(distutils(packages_access))).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(aggregates)).
:- use_module(library(lists)).

:- comment(title,  "One-download page website style").
:- comment(author, "David Trallero Mena").

:- comment(module, "This website template generates Clip Software
website.").

generate(Vars, WD) :-
	goto_dir(WD),
        %
	get_current_packages(Vars, CurrentDesc),
        % Package download index
	process_for([file = 'index_index.html', project_vars(file(CurrentDesc),
		    '~'([package_file_desc,
			    package_file,
			    package_doc_file_desc,
			    package_doc_file])
		), target = html('<v>weburl</v>packages/<v>package_name</v>/<v>package_name_version</v>')
		|Vars], INDEX_TERMS, _),
        %
	process_tmpl([file ='index.html', index = INDEX_TERMS
%	              , manuals = MANUALS_TERMS
		|Vars],
	    Output, _Dict),
        %
	generate_per_package_download(Vars),
	get_var(wr_permisions, Vars, Perms),
	get_var(wr_owner,      Vars, Owner),
	write_html('../index.html', Perms, Owner, Output).

generate_per_package_download(Vars) :-
	get_current_packages(Vars, CCurrentDesc),
	get_file_from_wildcards(CCurrentDesc, CurrentDesc),
        %
	read_vars_from_tmpl_file(CurrentDesc, TMPLVars),
	unify_vars(Vars, TMPLVars, TotalVars),
        %
	get_package_sources(Vars, CurrentDesc, Src_Terms, _),
	get_package_docs(Vars, CurrentDesc, Doc_Terms, _),
        %
	( process_tmpl([file = 'download.html', src_formats = Src_Terms,
		    doc_formats = Doc_Terms |TotalVars], Pkg_Terms, _) -> true
	),
	get_var(wr_permisions, Vars, Perms),
	get_var(wr_owner,      Vars, Owner),
	no_path_file_name(CurrentDesc, Path, _),
	atom_concat([Path, 'index.html'], Output),
	write_html(Output, Perms, Owner, Pkg_Terms),
	fail.
generate_per_package_download(_).

get_package_sources(Vars, CurrentDesc, TERMS, DICT) :-
	Each_doc_vars = [package_doc_file_desc, package_doc_file],
	Each_vars = [package_file_desc, package_file|
	    Each_doc_vars],
        % Check if there are source files
	project_vars(file(CurrentDesc), '~'(Each_doc_vars), PrjVars),
        %
	( get_var(package_file_desc, PrjVars, _)
	-> % Generate list of downloable file (TGZ, GZ...)
	    process_for([file = 'download_src.html', project_vars(file(
			    CurrentDesc), '~'(Each_doc_vars))
		    |Vars], SRC_TERMS, _),
            % Generate the table
	    process_tmpl([file = 'download_doc_table.html'
% 				 , thead = html( "<span class=""applname"">" || 
% 						 "<v>package_name</v></span> " ||
%                                                "Sources (all platforms)"),
		    icon = 'source.gif', tbody = SRC_TERMS, project_vars(file(
			    CurrentDesc), '~'(Each_vars)) |Vars], TERMS, DICT)
	;
	    TERMS = '',
	    DICT = []
	).


get_package_docs(Vars, CurrentDesc, TERMS, DICT) :-
	Each_src_vars = [package_file_desc, package_file],
	Each_vars = [package_doc_file_desc, package_doc_file|
	    Each_src_vars],
        % Check if there are source files
	project_vars(file(CurrentDesc), '~'(Each_src_vars), PrjVars),
        %
	( get_var(package_doc_file_desc, PrjVars, _)
	-> % Generate list of docs (PS, PDF...)
	    process_for([file = 'download_doc_src.html', project_vars(file(
			    CurrentDesc), '~'(Each_src_vars))
		    |Vars], DOC_TERMS, _),
            % Generate the table
	    process_tmpl([file = 'download_table.html'
% 				 , thead = html( "<span class=""applname"">" || 
% 						 "<v>package_name</v></span> " ||
%                                                "Sources (all platforms)"),
		    icon = 'source.gif', tbody = DOC_TERMS, project_vars(file(
			    CurrentDesc), '~'(Each_vars)) |Vars], TERMS, DICT)
	;
	    TERMS = '',
	    DICT = []
	).
