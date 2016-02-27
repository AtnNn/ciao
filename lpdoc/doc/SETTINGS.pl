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

% ---------------------------------------------------------------------------
% Distpkg (distribution package) definitions

% the component that contains this manual
parent_component := 'lpdoc'.

% TODO: move to lpdoc, put version numbers automatically, and symlinks
parent_component_version_nice := ~distpkg_obtain_version_nice(~atom_concat(~component_src(~parent_component), '/')).
:- use_module(library(distutils(distpkg_versions))).

% ----------------------------------------------------------------------------

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

manual_name := 'lpdoc'.
output_name := ~atom_concat([~manual_name, '-', ~parent_component_version_nice]).

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

commonopts := '-nobugs'|'-nopatches'.
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

