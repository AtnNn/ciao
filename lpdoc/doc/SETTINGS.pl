:- module(_, _, [ciaopaths, assertions, regtypes, fsyntax]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- doc(title, "LPdoc Manual Settings").

:- doc(module, "This file contains the definitions and configuration
   settings for the @apl{lpdoc} manual.").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").

:- doc(filetype, user).

% ----------------------------------------------------------------------------

:- use_module(library(lpdist(makedir_aux)), [fsR/2]).
:- reexport(library(lpdist(ciao_config_options)), 
    [bibfile/1, htmldir/1, docdir/1, infodir/1, mandir/1, lpdoclib/1]).
datamode(_) :- fail.
execmode(_) :- fail.

% the bundle that contains this manual
% TODO: This could be inferred (looking for a Manifest.pl in a parent dir)
parent_bundle := 'lpdoc'.

filepath := ~fsR(bundle_src(lpdoc)/'src').
filepath := ~fsR(bundle_src(lpdoc)/'readmes').
filepath := ~fsR(bundle_src(lpdoc)/'examples').
filepath := ~fsR(bundle_src(ciao)/'doc'/'common').

systempath := ~fsR(bundle_src(ciao)/'lib').
systempath := ~fsR(bundle_src(ciao)/'lib'/'assertions').
systempath := ~fsR(bundle_src(ciao)/'lib'/'metaprops').
systempath := ~fsR(bundle_src(ciao)/'lib'/'regtypes').
systempath := ~fsR(bundle_src(ciao)/'lib'/'engine').
systempath := ~fsR(bundle_src(ciao)/'lib'/'rtchecks').
systempath := ~fsR(bundle_src(ciao)/'lib'/'unittest').
systempath := ~fsR(bundle_src(ciao)/'library').
systempath := ~fsR(bundle_src(ciao)/'contrib').
systempath := ~fsR(bundle_src(ciao)/'doc'/'common').

pathsfile(_) :- fail. 

output_name := 'lpdoc'.

doc_structure := 
        'lpdoc'-[
	  'Reference'-[
	    'Generating',
	    'comments',
	    'assertions_doc',
	    'assertions_props',
	    'regtypes_doc',
	    'basic_props',
	    'native_props',
	    'meta_props',
	    'lpdoc_examples',
	    'example_module',
	    'rtchecks_doc',
	    'unittest_doc',
	    'lpdoc_install'
          ],
	  'Internals'-[
	    'autodoc',
	    'autodoc_state',
	    'autodoc_doctree',
	    'autodoc_structure',
	    'autodoc_settings',
	    % Backends
	    'Backends'-[
	      'autodoc_texinfo',
	      'autodoc_html'-[
	        'autodoc_html_resources',
	        'autodoc_html_template'
              ],
	      'autodoc_man'
            ],
	    % Miscellanea and other support code
	    'autodoc_filesystem',
	    'autodoc_index',
	    'autodoc_refsdb',
	    'autodoc_errors',
	    'autodoc_bibrefs',
	    'autodoc_aux',
	    'autodoc_images'
% TODO: Compute local modules that are not included in the internal documentation? Emit warning?
%	    'pbundle_download'
          ]
        ].

commonopts := no_bugs|no_patches.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

index := concept|lib|pred|prop|regtype|decl|author|global.

startpage := 1.

papertype := afourpaper.

libtexinfo := 'yes'.

docformat := texi|ps|pdf|manl|info|html.

