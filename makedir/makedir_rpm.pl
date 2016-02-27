% ===========================================================================
:- module(makedir_rpm, _, [make, fsyntax, assertions]).
% ===========================================================================
:- doc(title, "RPM package generator").

:- doc(subtitle,
	    "An automated RPM binary packages generator for CiaoDE").

:- doc(author, "Jos@'{e} Luis Gonz@'{a}lez").
:- doc(author, "Edison Mera").
:- doc(author, "The CLIP Group").

:- doc(ack, "This work builds on the work of Manuel Carro, Emilio
Gallego and Edison Mera. Thank you also to Manuel Hermenegildo and Germ@'{a}n
Puebla for their invaluable support.").

:- doc(copyright,
"
Copyright @copyright{} 2006--2007 Jos@'{e} Luis Gonz@'{a}lez/The CLIP Group.
").
% ===========================================================================

:- use_module(library(aggregates)).
:- use_module(library(distutils)).
:- use_module(library(autoconfig)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(file_utils)).
:- use_module(library(make(make_rt))).
:- use_module(library(make(system_extra))).
:- use_module(library(distutils(setperms))).
:- use_module(ciaodesrc(makedir('MenuOptions'))).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).
:- use_module(ciaodesrc(makedir(makedir_src))).
:- use_module(ciaodesrc(makedir(makedir_common))).

:- doc(summary, "
RPM generator is a system that builds binary RPM packages
for @apl{CiaoDE} automatically (with no user interaction),
for as many Linux distributions as possible and requiring minimal
packager maintenance. The system is designed to meet the following
requirements:

@begin{itemize}
@item Packager maintenance effort as close to zero as possible
  as @apl{CiaoDE} components evolve from version to version (in
  most cases the system is able to build packages for a new
  @apl{CiaoDE} version fully-automatically.)
@item Each @apl{CiaoDE} component gets packaged separately. (It's also
  possible to manually produce a single package with the whole
  @apl{CiaoDE} bundle.)
@item In order to ease @apl{CiaoDE} distribution, only one RPM package is
  produced for each @apl{CiaoDE} component and architecture. This
  single package should work in all Linux distributions. (This also saves
  the time and resources that would require building a set of packages
  for each Linux distribution.)
@item The system is meant to be portable. It should work (build packages)
  in any RPM Linux system with equivalent behaviour in all of them
  (except for differences in the local RPM version and system libraries.)
@item @index{Versioned packages} can be produced for specific versions of
  the @apl{CiaoDE} components so that users can install as many versions
  of the components as they wish at the same time (each version from its own
  versioned package.) The regular (non-versioned) package is meant for
  the main system version (usually the latest) to be upgraded.
@end{itemize}
").

:- doc(module, "
@section{Building packages}

@subsection{Requirements}

These are the main prerequisites for the build process to succeed:

@begin{itemize}
@item A user account with enough free space (at least twice the
	necessary for a compiled local repository.)
@item A local @apl{CiaoDE} repository with documentation already generated.
@item A working installation of @apl{CiaoDE}. (This is needed to generate
	the @concept{RPM specification} and handle the build process.)
@item @apl{RPM} v3 or higher.
@itm @apl{rpmbuild} installed in your system.
	yum install rpm-build
@end{itemize}

The usual @apl{CiaoDE} software requirements also apply, but these are
declared in the @concept{RPM specification} and the build process should
complain if any is missing.

@subsection{Instructions}

By default, a source @apl{CiaoDE} distribution for your local repository
will be packaged. Building is requested with the following command:

@begin{itemize}
@item @tt{ciaosetup installer_rpm [--option=value...]}
@end{itemize}

@subsection{Options summary}

One or more options may be added to the building command.

These control building and generation behaviour:

@begin{description}

@item{subpackages=yes} Enables building each component on its own
subpackage. Without subpackages (option set to @tt{'no'}) a single
@file{CiaoDE} package would be produced with the whole bundle.

@item{versionp=yes} Produces versioned packages instead of regular ones.
These are packages that permit installing several versions of Ciao
components at the same time (see @ref{Installing packages}.) Contents
for both kind of packages are identical except that the versioned ones
lack the simple names for binaries (e.g. includes @tt{ciao-1.13} but not
just @tt{ciao}) and add a version suffix to the names of manual pages.
This option @em{must} be used along with subpackages.

@item{svn_sources=yes} Switches building from distribution tarball to
straight @apl{subversion} sources. This option is not recommended,
though, and would include elements not intended for release. The
repository in RPM's build directory (as determined by @tt{_builddir}
macro) will be automatically updated, to guarantee the latest
@apl{CiaoDE} release is packaged, or just checked out if it's missing (a
@file{CiaoDE} symbolic link can be placed there instead, pointing to
your own repository.) Beware that @apl{ciaosetup} configure settings in
the repository will be overwritten by the RPM build.

@item{vendor_independent=yes} Produces packages that should work in all
RPM-based Linux distributions. If disabled the packages are only
guaranteed to work in the same distribution (vendor) they were built on.

@end{description}

Setting any of these compiles @apl{CiaoDE} with the feature of the same name
enabled (as defined in @apl{ciaosetup} configure):

@begin{itemize}
@item with_gsl=yes
@item use_posix_locks=yes
@item with_mysql=yes
@item with_java_interface=yes
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
@item install_info_cmd=/sbin/install-info
@item emacs_sitestart_dir=%@{_datadir@}/emacs/site-lisp/site-start.d
@item emacs_sitestart_filename=ciao-mode-init.el
@item xemacs_sitestart_dir=%@{_datadir@}/xemacs/site-packages/lisp/site-start.d
@end{itemize}

There's also support for changing some of Ciao's project details:

@begin{description}
@item{repo_dirname=CiaoDE}
	Sets @apl{CiaoDE}'s directory name for its @apl{subversion} repository.
	Relative to RPM's build directory (absolute paths not allowed.)
@item{repo_uri=file:///home/clip/SvnReps/Systems/CiaoDE/trunk}
	Sets @apl{CiaoDE}'s checkout URI for its @apl{subversion} repository.
@item{bibrepo_dirname=CiaoDE/bibtex_clip}
	Sets Ciao bibliography's directory name for its @apl{subversion}
	repository. Relative to RPM's build directory
	(absolute paths not allowed.)
@item{bibrepo_uri=file:///home/clip/SvnReps/bibtex/clip}
	Sets Ciao bibliography's checkout URI for its
	@apl{subversion} repository.
@end{description}

@section{Installing packages}

A package can be installed with:

@begin{verbatim}
# rpm -U package(s).rpm
@end{verbatim}

In order to upgrade your currently installed version of a package or
packages to a newer one the same command is used with the new package(s).
This replaces the old (installed) version with the new one.

If you want to install more than one version of a @apl{CiaoDE} component
at the same time just install a regular package for the latest version
(the one that you want to be upgraded as the system's main version) and
also install a versioned package for each of the additional (older)
versions. For instance, if you want to install both ciao 1.18, 1.16 and
1.14:

@begin{verbatim}
# rpm -U ciao-1.18.0-XXX.rpm
# rpm -U ciao-v1.16-1.16.0-XXX.rpm
# rpm -U ciao-v1.14-1.14.1-XXX.rpm
@end{verbatim}

The main (regular) installation will be available with plain commands
(@apl{ciao}, @apl{ciaoc}) as well as versioned commands. The older
(versioned) installations will only be available through versioned
commands (e.g. @tt{ciao-1.16}, @tt{ciaoc-1.14}.)

@section{Packager's maintenance guide}

The system comprises the following components:

@begin{enumerate}
@item A stub (@file{installer_rpm.pl}) for @apl{CiaoDE}'s installer that
	handles the whole process.
@item A shell script (@file{RPM-CiaoDE.sh}) that ensures an adequate
	RPM building environment exists (and sets it up if it doesn't)
	before running the actual build.
@item A skeleton for the @index{RPM specification} (@file{CiaoDE.spec.skel}.)
@end{enumerate}

When the build process is run, an @concept{RPM specification} file is
generated on-the-fly from the skeleton. Then the @apl{RPM-CiaoDE.sh}
script builds the packages from this specification, the resulting
packages get moved to the @file{package/} directory and the RPM building
environment becomes cleaned up.

@subsection{Changes demanding maintenance}

Any of the following changes to @apl{CiaoDE} requires that the @concept{RPM
specification} skeleton be updated accordingly:

@begin{itemize}
@item Major changes to the top path structure (@file{bin/}, @file{lib/ciao/},
	@file{lib/ciaopp/}, @file{lib/lpdoc/}, etc.)
@item Binaries added, removed or renamed.
@item Documentation renamed or relocated.
@item Components added, removed or renamed. This also affects the installer
	stub.
@item Changes to requirements for building @apl{CiaoDE}.
@item Changes to requirements for running @apl{CiaoDE} components.
@item Changes in the configuration or build system.
@item For SVN compilation/building: changes to repository names or locations.
@end{itemize}

Special care must be taken to guarantee that @concept{versioned
packages} remain installable along with other versioned packages of the
same component (as well as the regular upgradeable package), provided
they are all different versions. Special care must also be taken to
replicate changes in the separate subpackages to the main package.

Finally, there's a minor risk to be considered that SuSE changes
its distribution-specific peculiarities or some of the RPM-based
Linux distributions change the path or name for the install-info
command, the path or name for (x)emacs site-start scripts (all of which
conveniently defined on top of the specification) or the names for
packages listed as @tt{BuildRequires}.

@subsection{Troubleshooting}

The following command is available for testing purposes:

@begin{verbatim}
ciaosetup installer_rpm_spec
@end{verbatim}

This produces the @concept{RPM specification} for your current
@apl{CiaoDE} version, but doesn't build any package with it. The
specification is left in your repository's top directory (named
@file{CiaoDE.spec}). You can then review and modify it at will and build
the packages manually (by running @apl{rpmbuild} by yourself). All
generator options are also available through rpmbuild's @tt{--define}
facility (see the specification for details.) This is useful to create
custom packages and facilitates package generation for development
snapshots of @apl{CiaoDE} (where some binaries or documentation may be
temporarily down or unusually located.)

@subsection{Further reading}

Reference documentation for the RPM is available on:

@begin{itemize}
@item	@uref{RPM Guide}{http://fedora.redhat.com/docs/drafts/rpm-guide-en/}
@item	@uref{Maximum RPM}{http://www.rpm.org/max-rpm/}
@end{itemize}

The former resource is extensive and current, the latter includes a
convenient global index.

Many specific details not covered by those documents are scattered in
RPM's own documentation (a handful of note files.)
").


%% INSTALLER TARGETS:

% Options (each one option(Name,Value)) that will be used for RPM
% generation. Defaults are provided on top of CiaoDE.spec.skel, so we only
% need to list here the ones that the user explicitly set. If the same option
% appears more than once, the first occurence will take precedence.

rpm_options := ~append([

% These are compulsory (makedir_rpm/2 needs them to know how to behave).
% We set a default value if the option wasn't defined by user:
		~rpm_option_or_default('subpackages', 'no'),
		~rpm_option_or_default('svn_sources', 'no'),
		~rpm_option_or_default('versionp',    'no'),
		~rpm_option_or_default('vendor_independent', 'yes')|

% All other options are optional. We only set them if defined by user:
		~findall(option(OptName, OptVal), name_value(OptName,
			OptVal))

	    ],

% All available subpackages are produced by default:
	    ~findall(option(CompName, 'yes'), component(CompName))).

:- doc(rpm_options/1, "The options to be used in the RPM
	generation. All these get defined as RPM macros of the
	same name.").

installer_rpm <- [] # % [~packagefilename(tgz)] #
	"Generate RPM packages. A source distribution is generated if missing."
	:-
	ciaode_spec('CiaoDE.spec'),
	installer_rpm('CiaoDE.spec', ~rpm_options).

%% -

svn_rpm <- [] #
	"Generate straight repository RPM packages with NODISTRIBUTE elements"
	:-
	ciaode_spec('CiaoDE.spec'),
	installer_rpm('CiaoDE.spec',
	    [option('svn_sources', 'yes') |~rpm_options]).

%% Support code:

'CiaoDE.spec' <- ['makedir/CiaoDE.spec.skel'] :: FileName #
	"Generate a RPM specification for regular packages." :-
	ciaode_spec(FileName).

versioned_packagename(ComponentName, PackgName) :-
	atom_concat([ComponentName, '-v', ~vers(ComponentName)],
	    PackgName).

:- doc(versioned_packagename/2, "
	versioned_filename( ComponentName, PackageName )

	@var{PackageName}, is the versioned RPM package name for @apl{CiaoDE}'s
	component @var{ComponentName}.
").


:- doc(bug, "To speed up the process, we create the rpm from a
	precompiled bin distribution.").

:- doc(bug, "Some @apl{rpmbuild} versions are said to no longer
	support --defining a macro's value as an argument. This would
	break generation options.").

installer_rpm(SpecFileName, GenerationOptions) :-
	version(~wholesystem, PackageNameVersion),
	output_dir_name(OutputDirName),
	bold_message("Creating RPM package ~w, please be patient ...",
	    [PackageNameVersion]),
	rpm_prevailingoptions(GenerationOptions, RpmbuildOptions),
	rpmbuild_setoptions(RpmbuildOptions, RpmbuildSuffix),
	do(['makedir/RPM-CiaoDE.sh ', OutputDirName, '/ ',
		~version(~wholesystem), '-bin-', ~get_platform, ' ',
		SpecFileName, RpmbuildSuffix], [nofail]),
	mkdir_perm(OutputDirName, ~perms),
	rpm_macrovalue('_arch',   Arch),
	rpm_macrovalue('_rpmdir', RpmDir),
	atom_concat([RpmDir, '/', Arch, '/', 'CiaoDE', '-',
		~componentversion(ciaode), '-', ~svn_revision_atom,
		'.', Arch, '.rpm'], CiaoDERpmFileName),
	svn_revision_atom(SvnRevisionAtom),

	findall(ComponentRpmFileName,
	    (
		component(Component),
		( member(option('versionp', 'yes'), RpmbuildOptions) ->
		    versioned_packagename(Component, Ciaoname)
		; Ciaoname = Component ),
		componentversion(Component, ComponentVersion),
		atom_concat([RpmDir, '/', Arch, '/', Ciaoname, '-',
			ComponentVersion, '-', SvnRevisionAtom,
			'.', Arch, '.rpm'], ComponentRpmFileName)
	    ), RpmFileNames),

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
	string_to_file("",
	    ~atom_concat([OutputDirName, '/NODISTRIBUTE'])),
	del_file_nofail(~atom_concat([OutputDirName, '/',
		    PackageNameVersion, '.tar.gz'])),
	del_file_nofail(CiaoDERpmFileName),
	( member(option('subpackages', 'yes'), RpmbuildOptions) ->
	    del_files_nofail(RpmFileNames)
	; true ).

:- doc(installer_rpm/2, "
	installer_rpm(SpecFileName, GenerationOptions).

	Handle generation of RPM packages from @var{SpecFileName}
	(generated specification file name) according to
	@var{GenerationOptions} (see @ref{Options summary}.)
").
%	option(@var{Macro},@var{Value})).").

:- doc(bug, "By now, release is equal to the svn revision number,
	that is done only to let to generate the rpm now, but when the
	version numbers chaos be solved, this should be considered.").

component_version_names :=
	~flatten(~findall(S, component_version_name(S))).

component_version_name(ComponentVersionName) :-
	component(Component),
	atom_codes(Component,        ComponentS),
	atom_codes(~vers(Component), ComponentVersion),
	ComponentVersionName = ["%define " || ComponentS, "name " ||
	    ComponentS, "-v" || ComponentVersion, "\n"].

component_names := ~flatten(~findall(S, component_name(S))).

component_name(ComponentName) :-
	component(Component),
	atom_codes(Component, ComponentS),
	ComponentName = ["%define " || ComponentS, "name " || ComponentS,
	    "\n"].

component_move_man_versions :=
	~flatten(~findall(S, component_move_man_version(S))).

component_move_man_version(ComponentMoveManVersion) :-
	component(Component),
	atom_codes(Component,                    ComponentS),
	atom_codes(~componentversion(Component), ComponentVersion),
	ComponentMoveManVersion = ["mv %{buildroot}%{_mandir}/" || ComponentS,
	    "-" || ComponentVersion, ".manl ",
	    "%{buildroot}%{_mandir}/man1/" || ComponentS, "-" ||
	    ComponentVersion, ".1\n"].

component_move_mans :=
	~flatten(~findall(S, component_move_man(S))).

component_move_man(ComponentMoveMan) :-
	component(Component),
	atom_codes(Component,                    ComponentS),
	atom_codes(~componentversion(Component), ComponentVersion),
	ComponentMoveMan = ["mv %{buildroot}%{_mandir}/" || ComponentS,
	    "-" || ComponentVersion, ".manl ",
	    "%{buildroot}%{_mandir}/man1/" || ComponentS, ".1\n"].

component_install_info_cmds(Command) :=
	~flatten(~findall(S, component_install_info_cmd(Command, S))).

component_install_info_cmd(Command, ComponentInstallInfoCmd) :-
	component(Component),
	atom_codes(Component,                    ComponentS),
	atom_codes(~componentversion(Component), ComponentVersion),
	ComponentInstallInfoCmd = [
	    "    install-info " || Command,
	    " --dir-file=%{_infodir}/dir %{_infodir}/" || ComponentS, "-" ||
	    ComponentVersion, ".info\n"].

component_files := ~flatten(~findall(S, component_file(S))).

component_file(ComponentFile) :-
	component(Component),
	atom_codes(Component,                    ComponentS),
	atom_codes(~componentversion(Component), ComponentVersion),
	atom_codes(~vers(Component),             ComponentPathVersion),
	ComponentFile = [
	    "%{_libdir}/" || ComponentS, "/" || ComponentS, "-" ||
	    ComponentPathVersion, "\n%{ciaodocdir}/" || ComponentS, "-" ||
	    ComponentVersion, ".pdf\n"].


component_version_var([Name, ComponentVersion]) :-
	component(Component),
	packname(Component, PackName),
	atom_codes(PackName,                     PackNameS),
	atom_codes(~componentversion(Component), ComponentVersion),
	flatten(["<v>" || PackNameS, "Version</v>"], Name).

component_path_version_var([Name, ComponentPathVersion]) :-
	component(Component),
	packname(Component, PackName),
	atom_codes(PackName,         PackNameS),
	atom_codes(~vers(Component), ComponentPathVersion),
	flatten(["<v>" || PackNameS, "PathVersion</v>"], Name).

component_vars :=
	~findall(C,
	    (
		component_version_var(C)
	    ;
		component_path_version_var(C)
	    )).

ciaode_spec(FileName) :-
	replace_strings_in_file([
		["<v>Version</v>",
		    ~atom_codes(~componentversion(ciaode))],
		["<v>Release</v>", ~atom_codes(~svn_revision_atom)],
		["<v>PackageNameVersion</v>",
		    ~atom_codes(~version(~wholesystem))],
		["<v>OsArch</v>",
		    ~atom_codes(~get_platform)],
		["<v>ComponentMoveManVersions</v>",
		    ~component_move_man_versions],
		["<v>ComponentMoveMans</v>",             ~component_move_mans],
		["<v>ComponentIntegrateInfoindexes</v>", []],
		["<v>ComponentInstallInfoCmds</v>",
		    ~component_install_info_cmds("")],
		["<v>ComponentInstallInfoCmdsRemove</v>",
		    ~component_install_info_cmds("--remove")],
		["<v>ComponentVersionNames</v>", ~component_version_names],
		["<v>ComponentFiles</v>",        ~component_files],
		["<v>ComponentNames</v>", ~component_names]
		|~component_vars],
	    'makedir/CiaoDE.spec.skel', FileName).

:- doc(ciaode_spec/1, "
	ciaode_spec(FileName).

	Generate RPM specification file named @var{FileName}, for
	current @apl{CiaoDE} repository.
").

% Utilities to communicate Ciao installer with RPM macro system:

rpm_macrovalue(Macro, Value) :-
	atom_concat(['rpm --eval %', Macro], Cmd),
	shell_atom(Cmd, Value),
% (returned) Value = (requested) %Macro would mean Value not defined
	Value \= ~atom_concat(['%', Macro]). % So fail if macro not defined

:- doc(rpm_macrovalue/2, "
	rpm_macrovalue(Macro, Value).

	RPM @var{Macro} is system-defined as @var{Value}.
").

rpmbuild_setoptions([],                       '').
rpmbuild_setoptions([option(Macro, Value)|L], OptStrs) :-
	map_rpmoptval(option(Macro, Value), RpmValue),
	atom_concat([' --define "', Macro, ' ', RpmValue, '"'], OptStr),
	rpmbuild_setoptions(L, MoreOptStrs),
	atom_concat([OptStr, MoreOptStrs], OptStrs).

:- doc(rpmbuild_setoptions/2, "
	rpmbuild_setoptions(RpmOptions, RpmbuildSuffix).

	@var{RpmbuildSuffix} is a command-line suffix for invoking
	@apl{rpmbuild} that will set the RPM generation options. Each
	option is set in the specification by defining the macro
	of the same name to its appropriate value (see map_rpmoptval/2.)
").

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

:- doc(rpm_prevailingoptions/2, "
	rpm_prevailingoptions(Options, PrevailingOptions).

	@var{Options} is a list of options for RPM generation, with
	possible repetitions (same option with different values.)
	@var{PrevailingOptions} is the same list without repetitions,
	the first occurrence taking precedence over following ones.
").
% First occurence is chosen (instead of last) since this eases doing
% [ overwriting_option1, overwriting_option2 | Defaults ]


% Utilities to keep common options between Ciao installer and RPM spec:

map_rpmoptval(option(Opt, Val), MappedVal) :-
	ciaorpm_opttype(Opt, Type),
	ciaorpm_mapvalue(Type, Val, MappedVal),
	!.
map_rpmoptval(option(_, Val), Val).

:- doc(map_rpmoptval/2, "
	map_rpmoptval(Option, RpmValue).

@var{RpmValue} is the value to use in the RPM specification for
@var{Option}. It may just be @var{Option}'s straight value or a mapped
representation if the RPM specification needs so.
").


ciaorpm_opttype('subpackages',        'rpm_expression').
ciaorpm_opttype('versionp',           'rpm_expression').
ciaorpm_opttype('svn_sources',        'rpm_expression').
ciaorpm_opttype('vendor_independent', 'rpm_expression').
ciaorpm_opttype(ComponentName,        'rpm_expression') :-
	component(ComponentName). % Subpackage creation guards (see spec)
% Since we only need to map rpm_expressions there's no need to declare
% (and keep track of) other options.

:- doc(ciaorpm_opttype/2, "
ciaorpm_opttype(OptName, OptType).

This predicate declares which kind some options are (it's not meant to
be exhaustive, only the ones whose value gets mapped need to be
declared.) @var{OptType} is the option type for an option named
@var{OptName}. ").


ciaorpm_mapvalue('rpm_expression', 'yes', '1').
ciaorpm_mapvalue('rpm_expression', 'no',  '0').

:- doc(ciaorpm_mapvalue/3, "
ciaorpm_mapvalue(OptionType, OptionValue, RpmValue).

Maps values between RPM generator (Ciao) options and RPM macros.
@var{RpmValue} is the value to use in the RPM specification for an
option of type @var{OptionType} whose value is set to @var{OptionValue}
in Ciao. Anything not declared here is assumed to be immediately usable
(doesn't need mapping.)
").


rpm_option_or_default(OptName, _, option(OptName, OptValue)) :-
	name_value(OptName, OptValue), !.
rpm_option_or_default(OptName, DefValue, option(OptName, DefValue)).

:- doc(rpm_option_or_default/3, "
	rpm_option_or_default(OptionName, DefaultValue, Option).

	@var{Option} is the RPM @tt{option(OptionName,Value)} defined by
	user with @apl{ciaosetup}. @var{DefaultValue} is used instead
	as the default fallback when @var{OptionName} wasn't defined by user.
").

%% ---


:- doc(bug, "TODO:

@begin{itemize}
@item Check and warn for unknown options or incorrect values (at this time
	if user sets an invalid option it gets silently ignored).
@item Merge svn_rpm/installer_rpm targets (how on earth?)
@item Mark svn_sources packages so that users know whether they
	got a distribution or repository installation
@item (subpackages=no ^ versionp=yes) should warn(incompatible) and exit error.
@item Better compliance with @apl{rpmlint}
@item Check whether enabling mysql / java is feasible
@item Bugs from emilio's email (debian package)
@item Eliminate PATHS warning issued by certain Ciao binaries
@item RPM-CiaoDE.sh should be replaced by Prolog code in this module
@item Review package descriptions
@item Improvements to ciaosetup configure so it better suits rpmbuild
@end{itemize}
").
