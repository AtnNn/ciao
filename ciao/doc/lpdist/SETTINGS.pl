:- module(_, _, [ciaopaths, regtypes, fsyntax, assertions]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- use_module(library(lpdist(makedir_aux)), [fsR/2]).
:- use_module(library(lpdist(ciao_config_options))). % TODO: used?

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).
libtexinfo(_) :- fail.
datamode(_) :- fail.
execmode(_) :- fail.

output_name := 'lpdist'.

% TODO: use parent_bundle to share those defs
filepath := ~fsR(bundle_src(ciao)/'doc'/'lpdist'). % TODO: really?
filepath := ~ciaofilepath_common.

doc_structure := 
        'lpdist_reference'-[
          'lpdist/bundle_versions',
          'lpdist/ciao_config_db',
          'lpdist/ciao_config_options',
          'lpdist/ciao_configure',
          'lpdist/collect_modules',
          'lpdist/detcheader',
          'lpdist/distutils',
          'lpdist/info_installer',
          'lpdist/makedir_aux',
          'lpdist/ciao_bundle_db',
          'lpdist/makedir_distclean',
          'lpdist/pbundle_generator'-[
            'lpdist/pbundle_meta',
            'lpdist/pbundle_gen_common',
            'lpdist/pbundle_gen_bin',
            'lpdist/pbundle_gen_mac',
            'lpdist/pbundle_gen_rpm',
            'lpdist/pbundle_gen_src',
            'lpdist/pbundle_gen_win32'
          ],
          'lpdist/readme_generator',
          'lpdist/skip_settings',
          'lpdist/svn_tools'
        ].

%doc_mainopts := no_patches.
doc_mainopts := _ :- fail. % Allow patches in main changelog (those are the release notes)

% TODO: Added no_propuses because texindex breaks with very large
%       indices (due to internal, maybe arbitrary, limitations) --JF.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog|no_propuses.


