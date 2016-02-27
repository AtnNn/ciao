:- module(pbundle_gen_mac, _, [ciaopaths, make, fsyntax, assertions]).

:- doc(title, "Mac OS X Installer Generation").

:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "The CLIP Group").

:- doc(ack, "This builds the Ciao Macport Portfile and pkg installers.
             Thanks to Edison Mera for his support.").

:- doc(copyright, "
Copyright @copyright{} 2008--2012 R@'{e}my Heammerl@'{e}/The CLIP Group.
").

% ===========================================================================

:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(llists), [append/2]).
:- use_module(library(system)).
:- use_module(library(streams)).
:- use_module(library(format)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(file_utils)).
:- use_module(library(make(make_rt))).
:- use_module(library(system_extra)).
:- use_module(library(lpdist(pbundle_generator))).
:- use_module(library(lpdist(distutils)), [bundle_invoke_lpmake/2]).
:- use_module(library(lpdist(ciao_config_options))).
:- use_module(library(lpdist(ciao_config_db)), [restore_ciao_config/0]).
:- use_module(library(lpdist(ciao_bundle_db))).
:- use_module(library(lpdist(makedir_aux))).
:- use_module(library(lpdist(pbundle_gen_src))).
:- use_module(library(lpdist(pbundle_gen_common))).
:- use_module(library(md5sum)).

:- use_module(library(compiler(exemaker)), [make_exec/2]).

:- doc(summary, "This module provides predicates to build
@href{http://www.macports.com}{macports} @tt{Portfile} and @tt{pkg} packages for
@apl{CiaoDE}. The system is designed to work on the MacOS (>= 10.4)
platform and requires Apple\'s Xcode 3.0 Developer Tools to be
installed in the machine where the packages are generated.").

:- doc(module, "
@section{Building packages}

@subsection{Requirements}

These are the main prerequisites for the build process to succeed:

@begin{itemize}
@item A user account with enough free space (at least twice that 
      necessary for a compiled local repository for package generation).
@item A local @apl{CiaoDE} repository with binaries and documentation 
      already generated.
@item A working installation of @apl{CiaoDE}.
@item Apple\'s Xcode 3.0 Developer Tools (for package generation only).
@end{itemize}

The usual @apl{CiaoDE} software requirements also apply, the build
process should complain if anything is missing.

@subsection{Instructions}

@begin{itemize}
@item @tt{ciaosetup gen_pbundle macport}
@item @tt{ciaosetup gen_pbundle pkg}
@end{itemize}

@apl{ciaosetup gen_pbundle macport} produces a MacPorts @tt{Portfile}
which depends on a source @tt{tgz} distribution. If the tarball is
available in the package directory the command will automatically
produce it.  Notice that because the portfile includes a checksum of
the tarball source distribution it is dependent on this archive. Since
macports are used online the tarball used to produce the portfile
should be the same as the one available online on the Ciao website.


@apl{ciaosetup gen_pbundle pkg} produces a @tt{pkg} (i.e., a standard
\"Managed Install\" MacOs Package) wrapped into a dmg image disk. This
action assumes that binaries and documentation have been locally built
using correct configure options.

 
@section{Packager's maintenance guide}

The system comprises a skeleton for the MacPorts @tt{Portfile}
(@file{Portfile.skel}) that should be updated in case of changes in
@apl{ciaosetup}'s actions and options but also in case of changes in
the Ciao website.

Since the MacPorts @tt{Portfile} includes the electronic address where
the tarball source distribution resides (on the Ciao website) then any
change in the architecture of the website should be reflected here.
").

% ===========================================================================
:- doc(section, "pbundle Generation as a 'MacOS Binary Package' (.pkg)").


packageMaker := '/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker'.
hdiutil := hdiutil.
osacompile := osacompile.


gen_pbundle__pkg <- [gen_pbundle__descfile] :-
	restore_ciao_config, % TODO: Why is that necessary?
	gen_pbundle__pkg.

:- pred gen_pbundle__pkg/0
   # "Create a temporary directory @apl{__tmp_for_packagemaker} in the
      current directory, install Ciao into this directory as root
      build and generate the package using @apl{package_pkg}, called
      with default arguments".

make_temp_dir :=
	~codes_atom(~do_str_without_nl([mktemp, ' -d',
		    ' /tmp/CiaoDE_package_XXXXX'], fail)).

gen_pbundle__pkg :-
	bolder_message("creating MacOS package"),
        WorkSpace =  ~fsR(bundle_src(ciaode)),
	PackDir = ~pbundle_output_dir,
	TmpDir = ~make_temp_dir,
	DestDir = ~fsR(TmpDir/'root'),
	mkdir_perm(DestDir, ~perms),
	install_to_destroot(DestDir),
	generate_uninstaller(DestDir, UninstallerPath),
	package_pkg(DestDir, TmpDir, PackDir, WorkSpace, 'Ciao',
	    ~bundle_version_patch(ciaode), PName),
	generate_uninstaller_wrapper(TmpDir, UninstallerPath,
	    UninstallWrapperPath),
	package_dmg(PackDir, [PName, UninstallWrapperPath]),
	do(['rm -rf ', TmpDir], []).

install_to_destroot(DestDir) :-
	do(['./ciaosetup install_to_destroot --destdir=', DestDir], []).

:- pred package_pkg(DestPath, TmpDir, PPath, WorkPath, Name, Version, PName)
   # "Create a MacOS pkg package assuming that @var{DestPath} is the
      root build where Ciao has been installed, @var{Tmpdir} is a
      temporary directory where the predicate can store temporary
      files, @var{PPath} is the directory to store the package once
      it has been constructed, and @var{WorkPath} is the work path for
      compilation. @var{Name} is the name of the distribution and
      @var{Version} the Ciao version".

package_pkg(DestPath, TmpDir, PPath, WorkPath, Name, Version, PName) :-
	InfoFile = ~fsR(WorkPath/'Info.plist'),
%	DescriptionFile = ~atom_concat(WorkPath, '/Description.plist'),
	ResourcesPath = ~fsR(TmpDir/'pkg_resources'/'English.lproj'),
	ScriptsDir = ~fsR(TmpDir/'Scripts'),
	Bundle = ~bundle_wholesystem,
 	PName = ~fsR(concat_k(ext('.pkg'), PPath/(~bundle_versioned_packname(Bundle)))),
	%
	mkdir_perm(ResourcesPath, ~perms),
	write_welcome_html(ResourcesPath, Name, Version),
	write_conclusion_html(ResourcesPath),
%	copy_file('GPL', ~fsR(ResourcesPath/'Licence'), [overwrite]),
%	copy_file('makedir/clip.png', ~fsR(ResourcesPath/'background'), [overwrite]),   
	generate_installation_scripts(DestPath, ScriptsDir),

	write_info_plist(InfoFile, Name, Version),

	bold_message("Packaging binary distribution for MacOS"),
	do(['PMResourceLocale=English ',
		~packageMaker,
		' --root-volume-only ', '--verbose',
		' --root ', DestPath,
		' --out ', PName,
		' --resources ', ResourcesPath,
                ' --scripts ', ScriptsDir,
		' --title ', ~atom_concat([Name, '-', Version]),
		' --info ', InfoFile,
		' --target ', ~target_os_version,
		' --domain ', system,
		' --id ', ~atom_concat('es.upm.fi.dia.clip.', Name)
	    ], []).

package_dmg(PPath, List) :-
	Bundle = ~bundle_wholesystem,
	bold_message("Generating the dmg image"),
 	DmgName = ~fsR(concat_k(ext('.dmg'), PPath/(~bundle_versioned_packname(Bundle)))),
	bold_message("Wrapping the package into a dmg image"),
	process_list_for_src(List, Tail),
	do([~hdiutil, ' create ', DmgName, ' -volname ', ~bundle_versioned_packname(Bundle)|Tail], []).

process_list_for_src([],     []).
process_list_for_src([H|T1], [' -srcfolder "', H, '"'|T2]) :-
	process_list_for_src(T1, T2).

generate_uninstaller(DestDir, Path) :-
	bold_message("Generating bash script for uninstallation"),
	reallib_dir(RealLibDir),
	lib_dir(LibDir),
	%
 	Path = ~fsR(LibDir/'uninstall_ciao'),
	%
	do_str(['(cd ', DestDir, ' ; ', 'find */* | sort -r)'], fail, StrFiles),
	add_prefix_suffix(StrFiles, "rm -fd ", " || true", StrFiles_),
	%
	do_str(['(cd ', DestDir, ' ; ', 'find ', ~infodir_local, ' -name \'*.info\')'], StrInfo ="", StrInfo),
	add_prefix_suffix(StrInfo, "my_uninstall_info ", "", StrInfo_),
	%
	generate_bash_script(DestDir, Path, "
my_uninstall_info () {
	 install-info --delete --info-dir=`dirname $1` $1 || true		   
}

if [ x\"$1\" != xNO_ASK_FOR_CONFIRMATION ]; then
    echo \"Are you sure you want to uninstall Ciao?(yes/NO)\"
    read answer 
    if [ x\"$answer\" != xyes ]; then 
	echo Uninstallation canceled!
	exit 1
    fi
fi

~s

~s

rm -fd ~a ~a ~a || true\n
", [StrInfo_, StrFiles_, Path, RealLibDir, LibDir]).


generate_uninstaller_wrapper(TmpDir, Path, AppPath) :-
	bold_message("Generating uninstaller wrapper"),
 	TxtPath = ~fsR(TmpDir/'uninstaller.applescript'),
 	AppPath = ~fsR(TmpDir/'Uninstall Ciao.app'),
	%
	open_output(TxtPath, Stream), Stream = o(_, Stream_),
	format(Stream_, 
"set PosixPath to \"~a\"

try
	(PosixPath as POSIX file) as alias
	beep
	display dialog \"Are you sure you want to uninstall Ciao?\"
	do shell script PosixPath & \" NO_ASK_FOR_CONFIRMATION\" with administrator privileges
	display dialog \"Ciao uninstalled successfully.\" buttons \"OK\" default button \"OK\"
on error
	display dialog \"Ciao does not seem to be installed in your system.\" buttons \"OK\" default button \"OK\"
end try
", [Path]),
	close_output(Stream),
	do([~osacompile, ' -ai386 -o "', AppPath, '"  "', TxtPath, '"'], []).

infodir_local(LocalInfoDir):-
	(
	    atom_codes(~infodir, "/"||LocalPathStr) ->
	    atom_codes(LocalInfoDir, LocalPathStr)
	;
	    throw(error('infodir should be global (i.e. starting with /)', generate_uninstaller/2))
	).


add_prefix_suffix(L1, Prefix, Suffix, L) :-
	append(Prefix, "/"||L2, L),
	add_prefix_suffix_(L1, Prefix, Suffix, L2).
add_prefix_suffix([], _, _, []).
add_prefix_suffix_("\n"||L1, Prefix, Suffix, L) :- !,
	append(Suffix, "\n"||L2, L),
	add_prefix_suffix(L1, Prefix, Suffix, L2).
add_prefix_suffix_(" "||L1, Prefix, Suffix, "\\ "||L2) :- !,
	add_prefix_suffix_(L1, Prefix, Suffix, L2).
add_prefix_suffix_([H|L1], Prefix, Suffix, [H|L2]) :-
	add_prefix_suffix_(L1, Prefix, Suffix, L2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MacPorts Portfile                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

target_os_version('10.4').
os_version(A) :-
	append([_, "System Version: Mac OS X ", P, ".", M, ".", _],
	    ~do_str([system_profiler, ' SPSoftwareDataType'], fail)),
	!,
	append([P, ".", M], A).


applescript_editor(X) :-
	(
	    ~os_version @>= "10.6" ->
	    X = "AppleScript Editor"
	;
	    X = "Script Editor"
	).

gen_pbundle__macport <- [~pbundle_packname_absfile(tgz)] #
  "Generate MacPorts @tt{Portfile}. The source distribution is generated if missing."
	:-
	generate_portfile.

:- pred generate_portfile # "Generates MacPorts @tt{Portfile}".
generate_portfile :-
	Bundle = ~bundle_wholesystem,
	wr_template(at(~pbundle_output_dir), ~lpdist_dir/'mac', 'Portfile', [
            'Version' = ~bundle_version_patch(ciaode),
            'VersionedPackName' = ~atom_codes(~bundle_versioned_packname(Bundle)),
            'HomeURL' = ~home_url_str,
            'MasterURL' = ~master_url_str,
	    'MD5CheckSum' = ~md5sum(~pbundle_packname_absfile(tgz))
        ]).

% TODO: See macports documentation for explanation (check that this is correct)
% TODO: Define a predicate pbundle_url for this, connect with ciaobot code
master_url_str := MasterURL :-
	Bundle = ~bundle_wholesystem,
	VersionedPackName = ~atom_codes(~bundle_versioned_packname(Bundle)),
	Rev = ~atom_codes(~bundle_commit_info(Bundle, id)),
 	% TODO: Define a predicate pbundle_url for this, connect with ciaobot code
	MasterURL = ~append([~home_url_str, ~packages_dir_str, Rev, "/", VersionedPackName]).

write_xml_header(Str) :-
	format(Str, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
\n", []).


write_info_plist(File, Name, Version) :-
	open_output(File, OStr), OStr = o(_, Str),
	write_xml_header(Str),
	format(Str, "<dict>
	<key>CFBundleGetInfoString</key>
	<string>~w ~w</string>
	<key>CFBundleIdentifier</key>
	<string>es.upm.fi.dia.clip.~w</string>
	<key>CFBundleName</key>
	<string>~w-~w</string>
	<key>CFBundleShortVersionString</key>
	<string>~w</string>
	<key>IFMajorVersion</key>
	<integer>0</integer>
	<key>IFPkgFlagAllowBackRev</key>
	<true/>
	<key>IFPkgFlagAuthorizationAction</key>
	<string>RootAuthorization</string>
	<key>IFPkgFlagDefaultLocation</key>
	<string>/</string>
	<key>IFPkgFlagInstallFat</key>
	<false/>
	<key>IFPkgFlagIsRequired</key>
	<false/>
	<key>IFPkgFlagRelocatable</key>
	<false/>
	<key>IFPkgFlagRestartAction</key>
	<string>NoRestart</string>
	<key>IFPkgFlagRootVolumeOnly</key>
	<false/>
	<key>IFPkgFlagUpdateInstalledLanguages</key>
	<false/>
	<key>IFPkgFormatVersion</key>
	<real>0.10000000</real>
</dict>
</plist>\n", [Name, Version, Name, Version, Name, Version]),
	close_output(OStr).


write_description_plist(File, Name, Version, Description) :-
	open_output(File, OStr), OStr = o(_, Str),
	write_xml_header(Str),
	format(Str, "<dict>
	<key>IFPkgDescriptionDeleteWarning</key>
	<string></string>
	<key>IFPkgDescriptionDescription</key>
	<string>~w</string>
	<key>IFPkgDescriptionTitle</key>
	<string>~w</string>
	<key>IFPkgDescriptionVersion</key>
	<string>~w</string>
</dict>
</plist>n", [Description, Name, Version]),
	close_output(OStr).


write_package_info(File) :-
	open_output(File, OStr), OStr = o(_, Str),
	format(Str,
"<pkg-info install-location=\"/\" relocatable=\"false\" auth=\"root\"></pkg-info>\n",
	    []),
	close_output(OStr).

write_welcome_html(ResourcesPath, Name, Version) :-
 	open_output(~fsR(ResourcesPath/'Welcome.html'), OStr), OStr = o(_, Str),
	format(Str, "<html lang=\"en\">
<head>
	<meta http-equiv=\"content-type\" content=\"text/html; charset=iso-8859-1\">
	<title>Install ~w</title>
</head>
<body>
<font face=\"Helvetica\">
<b>Welcome to the ~w for Mac OS X Installer</b>
<p>
Ciao is a public domain, next generation multi-paradigm 
programming environment with a unique set of features:
</p>
<p><a href=\"~s\">~s</a></p>
<p>
Note, this installer will not install any application in the /Applications/
directory, but will instead install the system in ~w
(similarly to other compilers or standard UNIX programs).  
</p>
<p>
This installer guides you through the steps necessary to 
install ~w ~w for Mac OS X. To get started, click Continue.
</p>
</font>
</body>
</html>\n", [Name, Name, ~home_url_str, ~home_url_str, ~prefix_dir, Name, Version]),
	close_output(OStr).

write_conclusion_html(ResourcesPath) :-
	open_output(~atom_concat(ResourcesPath, '/Conclusion.html'), OStr), OStr = o(_, Str),
	format(Str, 
"<html lang=\"en\">
<head>
	<meta http-equiv=\"content-type\" content=\"text/html; charset=iso-8859-1\">
	<title>Important Information</title>
</head>
<body>
<font face=\"Helvetica\">
<p>
In order to enable the Ciao development environment for Emacs
(necessary for the source-level debugger and many other
functionalities), you need to <b>add manually at the end of your .emacs</b>
the following lines:
<pre>
(if (file-exists-p \"~w/ciao-mode-init.el\")
 (load-file \"~w/ciao-mode-init.el\")
)
</pre><br/>
It is strongly encouraged that you <b>install a recent graphical version
of emacs</b> (rather than the text-only version that comes with Mac OS), such as
<a href=\"http://emacsformacosx.com/\">Cocoa Emacs</a>
or <a href=\"http://aquamacs.org/\">Aquamacs</a>.
</p>
<p>
For uninstallation please use the \"Uninstall Ciao\" script that comes
with the installer.
</p></font>
</body>
</html>\n", [~lib_dir, ~lib_dir]),
	close_output(OStr).


generate_installation_scripts(DestDir, Dir):-
	do(['mkdir -p ', Dir], []), 
	generate_bash_script(Dir, 'preupgrade', "\n$RECEIPT_PATH/preinstall\n", []),
	generate_bash_script(Dir, 'postupgrade', "\n$RECEIPT_PATH/postinstall\n", []), 
	
	generate_preinstall_script(Dir, 'preinstall'),
	generate_postinstall_script(Dir, 'postinstall', DestDir).


% preinstall script 
%   - uninstallation of previously installed version of ciao

% NOTE: the uninstaller for version 1.1[45] (revision < 14672) is not
% in the same place as newer version. Variables
% OLD_UNINSTALL_CIAO_SCRIPTS lists all possible places of "old" installers.

generate_preinstall_script(Dir, Name) :-
	generate_bash_script(Dir, Name, "
OLD_UNINSTALL_CIAO_SCRIPTS=~w/delete_ciao
UNINSTALL_CIAO_SCRIPT=~w/uninstall_ciao

for file in $UNINSTALL_CIAO_SCRIPT $OLD_UNINSTALL_CIAO_SCRIPTS
do
    if [ -f $file ] 
    then 
	$file NO_ASK_FOR_CONFIRMATION || true
    fi
done


", ['/usr/local/lib/ciao/ciao-1.1[45]', ~lib_dir]).

% postinstall script performs:
%   - installation of info file (should be done by calling ciaosetup)
generate_postinstall_script(Dir, Name, DestDir):-
	
	do_str(['(cd ', DestDir, ' ; ', 'find ', ~infodir_local, ' -name \'*.info\')'], Str = "", Str),
	add_prefix_suffix(Str, "my_install_info ", "", Str_),
	generate_bash_script(Dir, Name, "
my_install_info () {
	 install-info --info-dir=`dirname $1` $1 || true		   
}\n
~s", [Str_]).
	



generate_bash_script(Dir, Name, Str, Args):-
	open_output(~fsR(Dir/Name), OStream), OStream = o(_, Stream),
	format(Stream, "#!/bin/bash\n", []), 
	format(Stream, Str, Args), 
	close_output(OStream).



% ===========================================================================
:- doc(section, "pbundle Generation as a 'MacOS bundle' (.app)").

gen_pbundle__app <- [gen_pbundle__descfile] :-
	restore_ciao_config, % TODO: Why is that necessary?
	gen_pbundle__app.

gen_pbundle__app :-
	Bundle = ~bundle_wholesystem,
	% Note: MacOS bundle IS NOT a Ciao bundle
	bolder_message("creating MacOS bundle"),
	%
	Domain = "es.upm.fi.dia.clip.ciaode",
	reallib_dir(RealLibDir),
	ciaobin_dir(BinDir),
	CiaoEngine = ~get_installed_ciaoengine, % e.g., ENGINEDIR/ciaoengine.DARWINi86
	PackDir = ~pbundle_output_dir,
	TmpDir = ~make_temp_dir,
	%
	AppBundlePath = ~fsR(TmpDir/'CiaoDE.app'),
	ResourcesDir = ~fsR(AppBundlePath/'Contents'/'Resources'),
	%
	set_name_value(emacs_type, 'MacOSBundle'),
 	% TODO: the emacs mode could be a bundle on its own in the future
	bundle_invoke_lpmake(ciao, build_emacs_mode), % invoke 'build_emacs_mode' on bundle 'ciao'
	del_name_value(emacs_type),
	%
	wr_template(origin, ~lpdist_dir/'mac', 'CiaoDE.applescript', [
	    'VERSION' = ~bundle_versioned_packname(Bundle),
	    'REALLIBDIR' = RealLibDir,
	    'CIAOENGINE' = CiaoEngine,
	    'BINDIR' = BinDir,
	    'DOMAIN' = Domain
        ]),
	%
	do([~osacompile, ' -ai386',
	    ' -o "', AppBundlePath, '"',
	    ' "', ~fsR(~lpdist_dir/'mac'/'CiaoDE.applescript'), '"'
	   ], []),
	%
	wr_template(at(TmpDir/'CiaoDE.app'/'Contents'), ~lpdist_dir/'mac', 'Info.plist', [
	    'VERSION' = ~bundle_versioned_packname(Bundle),
	    'DOMAIN' = Domain
	]),
	%
	do(['rm -f "', ResourcesDir, '/applet.icns"'], []),
	%
	install_to_destroot(ResourcesDir),
	do(['cp -f ',
	    ~fsR(~lpdist_dir/'mac'/'ciao-icon.icns'),
	    ' "', ResourcesDir, '"'], []),
	%
	% TODO: try not to write the output here
	wr_template(origin, ~lpdist_dir/'mac', 'configure_dotemacs.pl', [
	    'CIAOENGINE' = CiaoEngine,
	    'BINDIR' = BinDir,
	    'REALLIBDIR' = RealLibDir,
	    'DOMAIN' = Domain
        ]),
 	RBinDir = ~atom_concat(ResourcesDir, BinDir),
	do(['mkdir -p ', RBinDir], []),
	make_exec([~fsR(~lpdist_dir/'mac'/'configure_dotemacs.pl')],
 	    ~fsR(RBinDir/'configure_dotemacs')),
	%
 	RRealLibDir = ~atom_concat(ResourcesDir, RealLibDir),
 	open_output(~fsR(RRealLibDir/'sample.pl'), OStr), OStr = o(_, Str),
	sample_program_text(Str),
	close_output(OStr),
	%
	package_dmg(PackDir, [AppBundlePath]),
	do(['mv "', AppBundlePath, '" ', PackDir], []),
%	do(['rm -rf ', TmpDir],                 []), 
	true.

sample_program_text(Str) :-
	format(Str,
"% You can type code in this buffer.  
% Save with \"File->Save Buffer As...\" or \"C-x C-s\".
% Load into toplevel with \"C-c l\"
% Explore menus and buttons above.
% See also Section \"Using Ciao inside GNU emacs\" of the Ciao manual
% (\"CiaoHelp->Ciao system manual\") 

:- module(_,_).

main(Arg) :- 
	write(Arg).

", []).
