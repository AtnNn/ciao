:- module(pbundle_gen_rpm, _, [ciaopaths, make, fsyntax, assertions]).

:- doc(title, "RPM-based Installer Generation").
:- doc(subtitle, "An automated RPM binary package generator for CiaoDE").

:- doc(author, "Jos@'{e} Luis Gonz@'{a}lez").
:- doc(author, "Edison Mera").
:- doc(author, "Jos@'{e} F. Morales").
:- doc(author, "The CLIP Group").

:- doc(ack, "This work builds on the work of Manuel Carro, Emilio
   Gallego and Edison Mera. Thank you also to Manuel Hermenegildo and
   Germ@'{a}n Puebla for their invaluable support.").

:- doc(copyright,"Copyright @copyright{} 2006--2007 Jos@'{e} Luis
   Gonz@'{a}lez/The CLIP Group.").

% ===========================================================================

:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(file_utils)).
:- use_module(library(make(make_rt))).
:- use_module(library(system_extra)).

:- use_module(library(lpdist(ciao_config_options))).
:- use_module(library(lpdist(ciao_bundle_db))).
:- use_module(library(lpdist(pbundle_gen_src))).
:- use_module(library(lpdist(pbundle_gen_common))).
:- use_module(library(lpdist(makedir_aux))).

:- doc(summary, "
RPM generator is a system that builds binary RPM packages
for @apl{Ciao} automatically (with no user interaction),
for as many Linux distributions as possible, and requiring minimal
packager maintenance. The system is designed to meet the following
requirements:

@begin{itemize}
@item Keep packager maintenance effort as close as possible to zero
  while @apl{Ciao} bundles (software packages) evolve from version to
  version. In most cases the system is able to build packages for a
  new @apl{Ciao} version fully automatically.
@item It allows building each @apl{Ciao} bundle separately. It is
  also possible to produce a single package with the whole
  @apl{Ciao} bundle.
@item In order to simplify @apl{Ciao} distribution, only one RPM
  package is produced for each @apl{Ciao} bundle and
  architecture. This single package should work in all Linux
  distributions. (This also saves the time and resources that would
  require building a set of packages for each Linux distribution.)
@item The system is meant to be portable. It should work (build packages)
  in any RPM Linux system with equivalent behaviour in all of them
  (except for differences in the local RPM version and system libraries).
@item @index{Versioned packages} can be produced for specific versions of
  the @apl{Ciao} bundles so that users can install as many versions
  of the bundles as they wish at the same time (each version from its own
  versioned package). The regular (non-versioned) package is meant for
  the main system version (usually the latest) to be upgraded.
@end{itemize}
").

:- doc(module, "
@section{Building RPM packages}

@subsection{Requirements}

These are the main prerequisites for the build process to succeed:

@begin{itemize}
@item A user account with enough free space (at least twice that 
	necessary for a compiled local repository).
@item A local @apl{Ciao} repository with documentation already generated.
@item A working installation of @apl{Ciao}. (This is needed to generate
	the @concept{RPM specification} and handle the build process.)
@item @apl{RPM} v3 or higher.
@itm @apl{rpmbuild} installed in your system. (@tt{yum install rpm-build})
@end{itemize}

The usual @apl{Ciao} software requirements also apply, but these are
declared in the @concept{RPM specification} and the build process should
complain if anything is missing.

@subsection{Instructions}

By default, a source @apl{Ciao} distribution for your local repository
will be packaged. Building is requested with the following command:

@begin{itemize}
@item @tt{ciaosetup gen_pbundle rpm [--option=value...]}
@end{itemize}

@subsection{Options summary}

One or more options may be added to the building command.

These control the behaviour of the building and generation processes: 

@begin{description}

@item{@tt{subpackages=yes}} Enables building each bundle on its own
subpackage. Without subpackages (option set to @tt{'no'}) a single
@file{Ciao} package will be produced with the whole bundle.

@item{@tt{versionp=yes}} Produces versioned packages instead of regular
ones.  These are packages that allow installing several versions of
Ciao or its bundles at the same time (see @ref{Installing
packages}). The contents for both kinds of packages are identical
except that the versioned ones lack the simple names for binaries
(e.g., they include an executable called @tt{ciao-1.13} but not one
called @tt{ciao}) and add a version suffix to the names of manual
pages.  This option @em{must} be used along with subpackages.

@item{@tt{svn_sources=yes}} Switches the starting point used for building
the rpm file from a distribution tarball to building straight from the
@apl{subversion} sources. The rpm will then include the whole contents
of the @apl{subversion} sources, including directories and bundles
that are marked to not be included in distributions.  This option is
not recommended in general since it will include elements not ready or
intended for release. The repository in the RPM build directory (as
determined by the @tt{_builddir} macro) will be automatically updated,
to guarantee that the latest @apl{Ciao} release is included in the
rpm package, or just checked out if it is missing (a @file{Ciao}
symbolic link can be placed there instead, pointing to your own
repository). Beware that the @apl{ciaosetup} configure settings in the
repository will be overwritten by the RPM build.

@item{@tt{vendor_independent=yes}} Produces packages that should work in all
RPM-based Linux distributions. If disabled the packages are only
guaranteed to work in the same distribution (vendor) they were built on.

@end{description}

Setting any of the following compiles @apl{Ciao} with the feature of
the same name enabled (as defined in @apl{ciaosetup} @tt{configure}):

@begin{itemize}
@item @tt{with_gsl=yes}
@item @tt{use_posix_locks=yes}
@item @tt{with_mysql=yes}
@item @tt{with_java_interface=yes}
@end{itemize}

Note that none of these gets compiled in by default (if the option is
not explicitly set.)

Vendor-dependent paths and filenames can also be modified as options.
This is only recommended for building vendor dependent packages
(@code{vendor_independent=no}) since default values are sensible and
changing @tt{emacs_sitestart_dir} or @tt{emacs_sitestart_filename}
messes up with vendor-independent installation/removal scripts. These
would set the default values:

@begin{itemize}
@item @tt{install_info_cmd=/sbin/install-info}
@item @tt{emacs_sitestart_dir=@{_datadir@}/emacs/site-lisp/site-start.d}
@item @tt{emacs_sitestart_filename=ciao-mode-init.el}
@item @tt{xemacs_sitestart_dir=@{_datadir@}/xemacs/site-packages/lisp/site-start.d}
@end{itemize}

There is also support for changing some of Ciao's project details:

@begin{description}
@item{@tt{repo_dirname=CiaoDE}}
	Sets @apl{Ciao}'s directory name for its @apl{subversion} repository.
	Relative to the RPM build directory (absolute paths not allowed).
@item{@tt{repo_uri=file:///home/clip/SvnReps/Systems/CiaoDE/trunk}}
	Sets @apl{Ciao}'s checkout URI for its @apl{subversion} repository.
@item{@tt{bibrepo_dirname=CiaoDE/bibtex_clip}}
	Sets the Ciao bibliography directory name for its @apl{subversion}
	repository. Relative to the RPM build directory
	(absolute paths not allowed.)
@item{@tt{bibrepo_uri=file:///home/clip/SvnReps/bibtex/clip}}
	Sets the Ciao bibliography checkout URI for its
	@apl{subversion} repository.
@end{description}

@section{Installing RPM packages}

A RPM package can be installed with:

@begin{verbatim}
# rpm -U package(s).rpm
@end{verbatim}

In order to upgrade your currently installed version of a package or
packages to a newer one, the same command is used with the new package(s).
This replaces the old (installed) version with the new one.

If you want to install more than one version of a @apl{Ciao} bundle
at the same time, just install a regular package for the latest version
(the one that you want to be upgraded as the system's main version) and
also install a versioned package for each of the additional (older)
versions. For instance, if you want to install both ciao 1.18, 1.16 and
1.14, do:

@begin{verbatim}
# rpm -U ciao-1.18.0-XXX.rpm
# rpm -U ciao-v1.16-1.16.0-XXX.rpm
# rpm -U ciao-v1.14-1.14.1-XXX.rpm
@end{verbatim}

The main (regular) installation will be available with plain commands
(@apl{ciao}, @apl{ciaoc}) as well as versioned commands. The older
(versioned) installations will only be available through versioned
commands (e.g. @tt{ciao-1.16}, @tt{ciaoc-1.14}.)

@section{Packager maintenance guide}

The system comprises the following elements:

@begin{enumerate}
@item A stub (@file{pbundle_gen_rpm.pl}) for @apl{Ciao}'s installer that
	handles the whole process.
@item A shell script (@file{RPM-CiaoDE.sh}) that ensures that an adequate
	RPM building environment exists (and sets it up if it doesn't)
	before running the actual build.
@item A skeleton for the @index{RPM specification} (@file{CiaoDE.spec.skel}).
@end{enumerate}

When the build process is run, an @concept{RPM specification} file is
generated on-the-fly from the skeleton. Then the @apl{RPM-CiaoDE.sh}
script builds the packages from this specification, the resulting
packages get moved to the @file{build/pbundle/} directory, and the
RPM building environment is cleaned up.

@subsection{Changes demanding maintenance}

Any of the following changes to @apl{Ciao} requires that the @concept{RPM
specification} skeleton be updated accordingly:

@begin{itemize}
@item Major changes to the top path structure (@file{bin/}, @file{lib/ciao/},
	@file{lib/ciaopp/}, @file{lib/lpdoc/}, etc.)
@item Added, removed, or renamed binaries.
@item Renamed or relocated documentation.
@item Added, removed, or renamed (Ciao) bundles. This also affects the
      installer stub.
@item Changes to requirements for building the bundles.
@item Changes to requirements for running the bundles.
@item Changes in the configuration or build system.
@item For SVN compilation/building: changes to repository names or locations.
@end{itemize}

Special care must be taken to guarantee that @concept{versioned
packages} remain installable along with other versioned packages of the
same bundle (as well as the regular upgradeable package), provided
they are all different versions. Special care must also be taken to
replicate changes in the separate subpackages to the main package.

Finally, there is a minor risk to be considered: that SuSE changes
its distribution-specific peculiarities or some of the RPM-based
Linux distributions change the path or name for the install-info
command, the path or name for (x)emacs site-start scripts (all of which
conveniently defined on top of the specification), or the names for
packages listed as @tt{BuildRequires}.

@subsection{Troubleshooting}

The following command is available for testing purposes:

@begin{verbatim}
ciaosetup gen_pbundle rpm_spec
@end{verbatim}

This produces the @concept{RPM specification} for your current
@apl{Ciao} version, but does not build any package with it. The
specification is left in the top directory of your repository (named
@file{CiaoDE.spec}). You can then review and modify it at will and build
the packages manually (by running @apl{rpmbuild} by yourself). All
generator options are also available through rpmbuild's @tt{--define}
facility (see the specification for details). This is useful to create
custom packages and facilitates package generation for development
snapshots of @apl{Ciao} (where some binaries or documentation may be
temporarily unavailable or located in unusual places).

@subsection{Further reading}

The reference documentation for RPM is available at:

@begin{itemize}
@item @href{http://fedora.redhat.com/docs/drafts/rpm-guide-en/}{RPM Guide}
@item @href{http://www.rpm.org/max-rpm/}{Maximum RPM}
@end{itemize}

The former resource is extensive and current, the latter includes a
convenient global index.

Many specific details not covered by those documents are scattered in
RPM's own documentation (a handful of note files).
").

% ===========================================================================

:- pred rpm_options(Opts) # "@var{Opts} are the options to be used in 
	the RPM generation. All these get defined as RPM macros of the
	same name.".

rpm_options := ~append([
	% These are compulsory (makedir_rpm/2 needs them to know how to behave).
	% We set a default value if the option wasn't defined by user:
	~rpm_option_or_default('subpackages', 'no'),
	~rpm_option_or_default('svn_sources', 'no'),
	~rpm_option_or_default('versionp',    'no'),
	~rpm_option_or_default('vendor_independent', 'yes')|
        % All other options are optional. We only set them if defined by user:
	~findall(option(OptName, OptVal), name_value(OptName, OptVal))
	],
	% All available subpackages are produced by default:
	~findall(option(Bundle, 'yes'), registered_bundle(Bundle))).

% ===========================================================================
:- doc(section, "pbundle Generation as a 'RPM package'").

% Options (each one option(Name,Value)) that will be used for RPM
% generation. Defaults are provided on top of CiaoDE.spec.skel, so we only
% need to list here the ones that the user explicitly sets. If the same option
% appears more than once, the first occurence will take precedence.

gen_pbundle__rpm <- [] # % [~pbundle_name(tgz)] #
	"Generate RPM packages. A source distribution is generated if missing."
	:-
	gen_pbundle__rpm(~rpm_options).

%% -

svn_rpm <- [] #
	"Generate straight repository RPM packages with NODISTRIBUTE elements."
	:-
	gen_pbundle__rpm([option('svn_sources', 'yes') |~rpm_options]).

% ---------------------------------------------------------------------------

% TODO: necessary target?
gen_pbundle__rpm_spec <- [] #
	"Generate a RPM specification for regular packages." :-
	create_ciaode_spec.

:- pred versioned_packagename(BundleName, PackageName) #
   "@var{PackageName}, is the versioned RPM package name for
   @apl{Ciao}'s bundle @var{BundleName}.".

versioned_packagename(BundleName, PName) :-
	atom_concat([BundleName, '-v', ~bundle_version(BundleName)], PName).

:- doc(bug, "To speed up the process, we create the rpm from a
	precompiled bin distribution.").

:- doc(bug, "Some @apl{rpmbuild} versions are said to no longer
	support --defining a macro's value as an argument. This would
	break generation options.").

:- pred gen_pbundle__rpm(GenerationOptions) # "
	Handle generation of RPM packages according to
	@var{GenerationOptions} (see @ref{Options summary}.)".
%	option(@var{Macro},@var{Value})).

gen_pbundle__rpm(GenerationOptions) :-
	create_ciaode_spec,
	%
	SpecFileName = 'CiaoDE.spec',
	%
	PackageNameVersion = ~bundle_packname_version_patch_rev(~bundle_wholesystem),
	bold_message("Creating RPM package ~w, please be patient ...", [PackageNameVersion]),
	pbundle_output_dir(OutputDirName),
	rpm_prevailingoptions(GenerationOptions, RpmbuildOptions),
	rpmbuild_setoptions(RpmbuildOptions, RpmbuildSuffix),
	do([~fsR(~lpdist_dir/'rpm'/'RPM-CiaoDE.sh'), ' ', OutputDirName, '/ ',
		~bundle_packname_version_patch_rev(~bundle_wholesystem), '-bin-', ~get_platform, ' ',
		SpecFileName, RpmbuildSuffix], [nofail]),
	create_pbundle_output_dir,
	rpm_macrovalue('_arch',   Arch),
	rpm_macrovalue('_rpmdir', RpmDir),
 	% TODO: generalize, use fsR
	atom_concat([RpmDir, '/', Arch, '/', 'CiaoDE', '-',
		~bundle_version_patch(ciaode), '-', ~bundle_svn_revatm,
		'.', Arch, '.rpm'], CiaoDERpmFileName),
	bundle_svn_revatm(SvnRevisionAtom),
	%
	findall(BundleRpmFileName,
	    (
		registered_bundle(Bundle),
		( member(option('versionp', 'yes'), RpmbuildOptions) ->
		    versioned_packagename(Bundle, Ciaoname)
		; Ciaoname = Bundle
		),
		bundle_version_patch(Bundle, BundleVersion),
 		% TODO: refactor
		atom_concat([RpmDir, '/', Arch, '/', Ciaoname, '-',
			BundleVersion, '-', SvnRevisionAtom,
			'.', Arch, '.rpm'], BundleRpmFileName)
	    ), RpmFileNames),
	%
	( member(option('subpackages', 'no'), RpmbuildOptions) ->
	    copy_file(CiaoDERpmFileName, OutputDirName, [overwrite])
	;
	    true
	),
	( member(option('subpackages', 'yes'), RpmbuildOptions) ->
	    copy_files(RpmFileNames, OutputDirName, [overwrite])
	;
	    true
	),
 	del_file_nofail(~fsR(concat_k(ext('.tar.gz'), OutputDirName/PackageNameVersion))),
	del_file_nofail(CiaoDERpmFileName),
	( member(option('subpackages', 'yes'), RpmbuildOptions) ->
	    del_files_nofail(RpmFileNames)
	; true
	).

:- doc(bug, "For now, release is equal to the svn revision number,
	that is done only to allow generating the rpm now, but when 
	version numbers are clarified, this should be reconsidered.").

bundle_version_names :=
	~flatten(~findall(S, bundle_version_name(S))).

bundle_version_name(BundleVersionName) :-
	registered_bundle(Bundle),
	atom_codes(Bundle,        BundleS),
	atom_codes(~bundle_version(Bundle), BundleVersion),
	BundleVersionName = [
          "%define "||BundleS,"name "||
          BundleS, "-v"||BundleVersion,"\n"
        ].

bundle_names := ~flatten(~findall(S, bundle_name(S))).

bundle_name(BundleName) :-
	registered_bundle(Bundle),
	atom_codes(Bundle, BundleS),
	BundleName = [
          "%define "||BundleS,"name "||
          BundleS,"\n"
        ].

bundle_move_man_versions :=
	~flatten(~findall(S, bundle_move_man_version(S))).

bundle_move_man_version(BundleMoveManVersion) :-
	registered_bundle(Bundle),
	atom_codes(Bundle,                    BundleS),
	atom_codes(~bundle_version_patch(Bundle), BundleVersion),
	BundleMoveManVersion = [
          "mv %{buildroot}%{_mandir}/"||BundleS, "-"||BundleVersion, ".manl ",
	     "%{buildroot}%{_mandir}/man1/"||BundleS, "-"||BundleVersion, ".1\n"].

bundle_move_mans :=
	~flatten(~findall(S, bundle_move_man(S))).

bundle_move_man(BundleMoveMan) :-
	registered_bundle(Bundle),
	atom_codes(Bundle,                    BundleS),
	atom_codes(~bundle_version_patch(Bundle), BundleVersion),
	BundleMoveMan = [
          "mv %{buildroot}%{_mandir}/"||BundleS,"-"||BundleVersion,".manl",
	    " %{buildroot}%{_mandir}/man1/"||BundleS,".1\n"].

bundle_install_info_cmds(Command) :=
	~flatten(~findall(S, bundle_install_info_cmd(Command, S))).

bundle_install_info_cmd(Command, BundleInstallInfoCmd) :-
	registered_bundle(Bundle),
	atom_codes(Bundle,                    BundleS),
	atom_codes(~bundle_version_patch(Bundle), BundleVersion),
	BundleInstallInfoCmd = [
	    "    install-info "||Command,
	    " --dir-file=%{_infodir}/dir",
            " %{_infodir}/"||BundleS,"-"||BundleVersion,".info\n"].

bundle_files := ~flatten(~findall(S, bundle_file(S))).

bundle_file(BundleFile) :-
	registered_bundle(Bundle),
	atom_codes(Bundle,                    BundleS),
	atom_codes(~bundle_version_patch(Bundle), BundleVersion),
	atom_codes(~bundle_version(Bundle), BundlePathVersion),
	BundleFile = [
	  "%{_libdir}/"||BundleS,"/"||BundleS,"-"||BundlePathVersion,"\n",
          "%{ciaodocdir}/"||BundleS,"-"||BundleVersion,".pdf\n"].

:- pred create_ciaode_spec # "Generate RPM specification file.".
create_ciaode_spec :-
	wr_template(cwd, ~lpdist_dir/'rpm', 'CiaoDE.spec', [
	    'Version' = ~bundle_version_patch(ciaode),
	    'Release' = ~bundle_svn_revatm,
	    'PackageNameVersion' = ~bundle_packname_version_patch_rev(~bundle_wholesystem),
	    'OsArch' = ~get_platform,
	    'BundleMoveManVersions' = ~bundle_move_man_versions,
	    'BundleMoveMans' = ~bundle_move_mans,
	    'BundleIntegrateInfoindexes' = "",
	    'BundleInstallInfoCmds' = ~bundle_install_info_cmds(""),
	    'BundleInstallInfoCmdsRemove' = ~bundle_install_info_cmds("--remove"),
	    'BundleVersionNames' = ~bundle_version_names,
	    'BundleFiles' = ~bundle_files,
	    'BundleNames' = ~bundle_names
	|~bundle_vars]).

% Other keys to be filled in CiaoDE.spec
bundle_vars :=
	~findall(C,
	    (
		bundle_version_var(C)
	    ;
		bundle_path_version_var(C)
	    )).

bundle_version_var(Key = BundleVersion) :-
	registered_bundle(Bundle),
	bundle_packname(Bundle, PackName),
	Key = ~atom_concat(PackName, 'Version'),
	BundleVersion = ~bundle_version_patch(Bundle).

bundle_path_version_var(Key = BundlePathVersion) :-
	registered_bundle(Bundle),
	bundle_packname(Bundle, PackName),
	Key = ~atom_concat(PackName, 'PathVersion'),
	BundlePathVersion = ~bundle_version(Bundle).

:- pred rpm_macrovalue(Macro, Value) # "RPM @var{Macro} is
   system-defined as @var{Value}.".

% Utilities to communicate Ciao installer with RPM macro system:
rpm_macrovalue(Macro, Value) :-
	atom_concat(['rpm --eval %', Macro], Cmd),
	do_str_without_nl__popen(Cmd, String),
	atom_codes(Value, String),
	% (returned) Value = (requested) %Macro would mean Value not defined
	Value \= ~atom_concat(['%', Macro]). % So fail if macro not defined

:- pred rpmbuild_setoptions(RpmOptions, RpmbuildSuffix) # "
	@var{RpmbuildSuffix} is a command-line suffix for invoking
	@apl{rpmbuild} that will set the RPM generation options. Each
	option is set in the specification by defining the macro of
	the same name to its appropriate value (see @pred{map_rpmoptval/2}).
	".

rpmbuild_setoptions([],                       '').
rpmbuild_setoptions([option(Macro, Value)|L], OptStrs) :-
	map_rpmoptval(option(Macro, Value), RpmValue),
	atom_concat([' --define "', Macro, ' ', RpmValue, '"'], OptStr),
	rpmbuild_setoptions(L, MoreOptStrs),
	atom_concat([OptStr, MoreOptStrs], OptStrs).

:- pred rpm_prevailingoptions(Options, PrevailingOptions) # "
	@var{Options} is a list of options for RPM generation, with
	possible repetitions (same option with different values).
	@var{PrevailingOptions} is the same list without repetitions,
	the first occurrence taking precedence over the following ones.".
% First occurence is chosen (instead of last) since this eases doing
% [ overwriting_option1, overwriting_option2 | Defaults ]

rpm_prevailingoptions([], []).
rpm_prevailingoptions([option(OptName, Val)|L],
	    [option(OptName, Val)|PL]) :-
	delete_non_ground(L, option(OptName, _), DL),
	rpm_prevailingoptions(DL, PL).

% If last occurence were to take precedence instead of first:
%
%rpm_prevailingoptions( [], [] ).
%rpm_prevailingoptions( [ option( OptName, _ ) | L ], [ PL ] ) :-
%	member( option(OptName, _ ), L ),
%	!
%	rpm_prevailingoptions( L, PL ).
%rpm_prevailingoptions( [ H | L ], [ H | PL ] ) :-
%	rpm_prevailingoptions( L, PL ).


% Utilities to keep common options between Ciao installer and RPM spec:

:- pred map_rpmoptval(Option, RpmValue) 
# "@var{RpmValue} is the value to use in the RPM specification for
   @var{Option}. It may just be @var{Option}'s straight value or a
   mapped representation if the RPM specification needs so.".

map_rpmoptval(option(Opt, Val), MappedVal) :-
	ciaorpm_opttype(Opt, Type),
	ciaorpm_mapvalue(Type, Val, MappedVal),
	!.
map_rpmoptval(option(_, Val), Val).

:- pred ciaorpm_opttype(OptName, OptType)
# "This predicate declares the kind of some options (it is not meant
   to be exhaustive, only the ones whose value gets mapped need to be
   declared). @var{OptType} is the option type for an option named
   @var{OptName}.".

ciaorpm_opttype('subpackages',        'rpm_expression').
ciaorpm_opttype('versionp',           'rpm_expression').
ciaorpm_opttype('svn_sources',        'rpm_expression').
ciaorpm_opttype('vendor_independent', 'rpm_expression').
ciaorpm_opttype(BundleName,           'rpm_expression') :-
	registered_bundle(BundleName). % Subpackage creation guards (see spec)
% Since we only need to map rpm_expressions there's no need to declare
% (and keep track of) other options.

:- pred ciaorpm_mapvalue(OptionType, OptionValue, RpmValue) 
# "Maps values between RPM generator (Ciao) options and RPM macros.
   @var{RpmValue} is the value to use in the RPM specification for an
   option of type @var{OptionType} whose value is set to
   @var{OptionValue} in Ciao. Anything not declared here is assumed to
   be immediately usable (does not need mapping).".

ciaorpm_mapvalue('rpm_expression', 'yes', '1').
ciaorpm_mapvalue('rpm_expression', 'no',  '0').

:- pred rpm_option_or_default(OptionName, DefaultValue, Option) 
# "@var{Option} is the RPM @tt{option(OptionName,Value)} defined by
   the user with @apl{ciaosetup}. @var{DefaultValue} is used instead as 
   the default fallback when @var{OptionName} was not defined by 
   the user.".

rpm_option_or_default(OptName, _, option(OptName, OptValue)) :-
	name_value(OptName, OptValue), !.
rpm_option_or_default(OptName, DefValue, option(OptName, DefValue)).

% ===========================================================================

:- doc(section, "Future Work").

:- doc(bug, "Check and warn for unknown options or incorrect values
	(at this time if the user sets an invalid option it gets silently 
	ignored).").
:- doc(bug, "Merge svn_rpm/gen_pbundle (rpm) targets (how on earth?)").
:- doc(bug, "Mark svn_sources packages so that users know whether they
	got a distribution or repository installation").
:- doc(bug, "(subpackages=no ^ versionp=yes) should warn(incompatible)
and exit error.").
:- doc(bug, "Better compliance with @apl{rpmlint}").
:- doc(bug, "Check whether enabling mysql / java is feasible").
:- doc(bug, "Bugs from emilio's email (debian package)").
:- doc(bug, "Eliminate PATHS warning issued by certain Ciao binaries").
:- doc(bug, "RPM-CiaoDE.sh should be replaced by Prolog code in this module").
:- doc(bug, "Review package descriptions").
:- doc(bug, "Improvements to ciaosetup configure so it better suits rpmbuild").
