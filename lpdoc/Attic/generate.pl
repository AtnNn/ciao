% generate( [fileurl='~dtm/public_html/lpdoc_docs/',weburl='/~dtm/lpdoc_docs/'] , '~dtm/public_html/lpdoc_docs/tmpl/' ).

:- module(generate, [], [assertions]).

:- use_module(library(wp_gen(process_tmpl))).
:- use_module(library(distutils(packages_access))).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(messages)).
:- use_module(library(aggregates)).

:- comment(title,  "Lpdoc locally installed manuals webpage.").
:- comment(author, "David Trallero Mena").

:- comment(module, "This website template generates Lpdoc locally
installed manuals website.").

:- export(generate/2).
generate(Vars, WD) :-
	goto_dir(WD),
        % Manuals, downloads, old_download
	get_current_packages(Vars, CurrentDesc),
        % Package download index
	process_for([file = 'index_index.html', target = html(
		    '#<v>package_name</v>'), project_vars(file(CurrentDesc),
		    '~'([package_file_desc,
			    package_file,
			    package_doc_file_desc,
			    package_doc_file])
		)
		|Vars], INDEX_TERMS, _),
        %
	findall((DT, DD),
	    generate_package_doc_index(CurrentDesc, Vars, DT, DD),
	    MANUALS),
        %
	process_for_join_pages(MANUALS, MANUALS_TERMS, _),
        %
	process_tmpl([file ='index.html', index = INDEX_TERMS, manuals =
		MANUALS_TERMS |Vars],
	    Output, _Dict),
	get_var(wr_permisions, Vars, Perms),
	get_var(wr_owner,      Vars, Owner),
	write_html('../index.html', Perms, Owner, Output).

generate_package_doc_index(CurrentDesc, Vars, DOC_TERMS, DOC_DICTS) :-
	get_file_from_wildcards(CurrentDesc, CurrentPackage),
	process_for([file = 'doc_page_index.html', project_vars(file(
			CurrentPackage),
		    '~'([package_file_desc, package_file])
		)
		|Vars], MANUALS, _),
        %
	( process_tmpl([file = 'doc_page.html', manuals = MANUALS, project_vars(
			file(CurrentPackage), '~'([package_file_desc,
				package_file,
				package_doc_file_desc,
				package_doc_file])
		    )
		    |Vars], DOC_TERMS, DOC_DICTS) -> 
          true
	; fail
	).
