:- module(_, _, [ciaopaths, assertions, regtypes, fsyntax]).

:- include(lpdocsrc(lib('SETTINGS_schema'))).
% ***************************************************************************
% This is a LPdoc configuration file. See SETTINGS_schema for documentation *
% ***************************************************************************

:- doc(title, "LPdoc Manual Settings").

:- doc(module, "This file contains the definitions and configuration
   settings for the @apl{lpdoc} manual.").

:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Edison Mera").
:- doc(author, "Jose F. Morales").

:- doc(filetype, user).

% ----------------------------------------------------------------------------

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(component_registry), [component_src/2]).
:- reexport(ciaodesrc(makedir('DOCCOMMON'))).
datamode(_) :- fail.
execmode(_) :- fail.

% the component that contains this manual
% TODO: This could be inferred (looking for a makedir/CONFIG.pl in a parent dir)
parent_component := 'lpdoc'.

filepath := ~atom_concat([~component_src(lpdoc), '/src']).
filepath := ~atom_concat([~component_src(lpdoc), '/readmes']).
filepath := ~atom_concat([~component_src(lpdoc), '/examples']).
filepath := ~atom_concat([~component_src(ciao),  '/doc/common']).

systempath := ~atom_concat([~component_src(ciao), ~ciao_path]).

ciao_path := '/lib'|
	'/lib/assertions'|
	'/lib/metaprops'|
	'/lib/regtypes'|
	'/lib/engine'|
	'/lib/rtchecks'|
	'/lib/unittest'|
	'/library'|
	'/contrib'|
        '/doc/common'.

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
%	    'distpkg_download'
%	    'fastformat'
          ]
        ].

commonopts := no_bugs|no_patches.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

index := concept|lib|pred|prop|regtype|decl|author|global.

startpage := 1.

papertype := afourpaper.

libtexinfo := 'yes'.

infodir_headfile := ~atom_concat([~component_src(lpdoc),
		'/lib/Head_clip.info']).
infodir_tailfile := ~atom_concat([~component_src(lpdoc),
		'/lib/Tail_clip.info']).

docformat := texi|ps|pdf|manl|info|html.

