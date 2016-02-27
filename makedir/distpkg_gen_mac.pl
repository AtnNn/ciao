:- module(distpkg_gen_mac, _, [ciaopaths, make, fsyntax, assertions]).

:- doc(title, "Mac OS X Installer Generation").

:- doc(author, "R@'{e}my Haemmerl@'{e}").
:- doc(author, "The CLIP Group").

:- doc(ack, "This work builds the Ciao Macport Portfile.
                 Thanks to Edison Mera for his support.").

:- doc(copyright, "
Copyright @copyright{} 2008--2010 R@'{e}my Heammerl@'{e}/The CLIP Group.
").

% ===========================================================================

:- use_module(library(aggregates)).
:- use_module(library(distutils), [component_make/2]).
:- use_module(library(lists)).
:- use_module(library(llists), [append/2]).
:- use_module(library(system)).
:- use_module(library(streams)).
:- use_module(library(format)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(file_utils)).
:- use_module(library(make(make_rt))).
:- use_module(library(make(system_extra))).
:- use_module(library(distutils(distpkg_generator))).
:- use_module(ciaodesrc(makedir('MenuOptions'))).
:- use_module(ciaodesrc(makedir('ConfigValues'))).
:- use_module(ciaodesrc(makedir(makedir_component))).
:- use_module(ciaodesrc(makedir(makedir_aux))).
:- use_module(ciaodesrc(makedir(distpkg_gen_src))).
:- use_module(ciaodesrc(makedir(distpkg_gen_common))).

:- use_module(library(compiler(exemaker)), [make_exec/2]).

:- doc(summary, "This module provides predicates to build
@uref{macports}{http://www.macports.com} Portfile and pkg packages for
@apl{CiaoDE}. The system is designed to work on the MacOS (>= 10.4)
platform and requires Apple\'s Xcode 3.0 Developer Tools to be
installed in the machine where the packages are generated.").

:- doc(module, "
@section{Building packages}

@subsection{Requirements}

These are the main prerequisites for the build process to succeed:

@begin{itemize}
@item A user account with enough free space (at least twice that 
      necessary for a compiled local repository for package generation.)
@item A local @apl{CiaoDE} repository with binary and documentation 
      already generated.
@item A working installation of @apl{CiaoDE}.
@item Apple\'s Xcode 3.0 Developer Tools (for package generation only).
@end{itemize}

The usual @apl{CiaoDE} software requirements also apply, the build
process should complain if anything is missing.

@subsection{Instructions}

@begin{itemize}
@item @tt{ciaosetup installer_macport}
@item @tt{ciaosetup installer_pkg}
@end{itemize}

@apl{ciaosetup installer_macport} produces a macport portfile which
depends on a source tgz distribution. If the tarball is available in
the package directory the command will automatically produce it.
Notice that because the portfile includes a checksum of the tarball
source distribution it is dependant on this archive. Since macports
are used online the tarball used to produce the portfile should be the
same as the one available online on the Ciao website.


@apl{ciaosetup installer_pkg} produces a pkg (i.e., a standard
 \"Managed Install\" MacOs Package). This action assumes that binaries
 and documentation have been locally built using correct configure
 options.

 
@section{Packager's maintenance guide}

The system comprises a skeleton for the macport Portfile
(@file{Portfile.skel}) that should be upadted in case of changes in
@apl{ciaosetup}'s actions and options but also in case of changes in
the Ciao website.

Since the macport Portfile includes the electronic adress where the
tarball source distribution resides (on the Ciao website) then any
change in the architecture of the website should be reflected here.
").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MacOS Binary Package                                     % 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

installer_pkg <- [descfile] :-
	initvals,
	installer_pkg.

:- doc(installer_pkg/0, " @apl{installer_pkg} will create a
	temporary directory @apl{__tmp_for_packagemaker} in the
	current directory, install ciao into this directory as root
	build and generate the package using @apl{package_pkg}, called
	with default arguments").

make_temp_dir :=
	~codes_atom(~do_str_without_nl([mktemp, ' -d',
		    ' /tmp/CiaoDE_package_XXXXX'], fail)).

installer_pkg :-
	bolder_message("creating MacOS package"),
	working_directory(WorkSpace, '.'),
	PackDir = ~atom_concat(WorkSpace, '/package'),
	TmpDir = ~make_temp_dir,
	DestDir = ~atom_concat(TmpDir, '/root'),
	mkdir_perm(DestDir, ~perms),
	install_to_destroot(DestDir),
	generate_uninstaller(DestDir, UninstallerPath),
	package_pkg(DestDir, TmpDir, PackDir, WorkSpace, 'Ciao',
	    ~component_version_patch(ciaode), PkgName),
	generate_uninstaller_wrapper(TmpDir, UninstallerPath,
	    UninstallWrapperPath),
	package_dmg(PackDir, [PkgName, UninstallWrapperPath]),
	do(['rm -rf ', TmpDir], []).



install_to_destroot(DestDir) :-
	do(['./ciaosetup install_to_destroot --destdir=', DestDir], []).



:- doc(package_pkg/6, " @apl{package_pkg(Destpath, TmpDir,
	PkgPath, WorkPath, Name, Version)} will create a MacOS pkg
	package assuming that @var{Destpath} is the root build where
	Ciao has been installed, @var{Tmpdir} is a temporary
	directory where the predicate can store temporary files,
	@var{PkgPath} is the directory to store the package once it
	has been constructed, and @var{WorkPath} is the work path for
	compilation. @var{Name} is the name of the distribution and
	@var{Version} the Ciao version").


package_pkg(DestPath, TmpDir, PkgPath, WorkPath, Name, Version, PkgName) :-
	%
	InfoFile = ~atom_concat(WorkPath, '/Info.plist'),
%	DescritpionFile = ~atom_concat(WorkPath, '/Description.plist'),
	RessourcePath = ~atom_concat(TmpDir, '/pkg_ressouces/English.lproj'),
	PackageMaker =
'/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker',
	PkgName = ~atom_concat([PkgPath, '/', ~component_packname_version_patch_rev(~component_wholesystem), '.pkg']),

	mkdir_perm(RessourcePath, ~perms),
	write_welcome_html(RessourcePath, Name, Version),
%	copy_file('GPL', ~atom_concat(RessourcePath, '/Licence'), [overwrite]),
%	copy_file('makedir/clip.png', ~atom_concat(RessourcePath, '/background'), [overwrite]),   

	write_info_plist(InfoFile, Name, Version),

	bold_message("Packaging binary distribution for MacOS"),
	do(['PMResourceLocale=English ',
		PackageMaker,
		' --root-volume-only ', '--verbose',
		' --root ', DestPath,
		' --out ', PkgName,
		' --resources ', RessourcePath,
		' --title ', ~atom_concat([Name, '-', Version]),
		' --info ', InfoFile,
		' --target ', ~target_os_version,
		' --domain ', system,
		' --id ', ~atom_concat('es.upm.fi.dia.clip.', Name)
	    ], []).

package_dmg(PkgPath, List) :-
	bold_message("Generating the dmg image"),
	DmgName = ~atom_concat([PkgPath, '/', ~component_packname_version_patch_rev(~component_wholesystem), '.dmg']),
	bold_message("Wrapping the package into a dmg image"),
	process_list_for_src(List, Tail),
	do(['hdiutil create ', DmgName, ' -volname ', ~component_packname_version_patch_rev(~component_wholesystem)
		|Tail], []).

process_list_for_src([],     []).
process_list_for_src([H|T1], [' -srcfolder "', H, '"'|T2]) :-
	process_list_for_src(T1, T2).

generate_uninstaller(DestDir, Path) :-
	bold_message("Generating bash script for uninstallation"),
	reallibdir(RealLibDir),
	libdir(LibDir),
	do_str(['(cd ', DestDir, ' ; ', 'find */*/* | sort -r)'], fail, Str),
	atom_concat([RealLibDir, '/', delete_ciao], Path),
	atom_concat([DestDir,    '/', Path],        RealPath),
	open_output(RealPath, Streams),
	add_prefix_suffix(Str, "rm -fd ", " || true", NStr),
	format(
"#!/bin/bash

if [ x\"$1\" != xNO_ASK_FOR_CONFIRMATION ]; then
    echo \"Are you sure you want to uninstall Ciao?(yes/NO)\"
    read answer 
    if [ x\"$answer\" != xyes ]; then 
	echo uninstallation canceled!
	exit 1
    fi
fi

~s", [NStr]),
	format("rm -df ~a ~a ~a || true\n", [Path, RealLibDir, LibDir]),
	close_output(Streams).


generate_uninstaller_wrapper(TmpDir, Path, AppPath) :-
	bold_message("Generating uninstaller wrapper"),
	atom_concat([TmpDir, '/', 'uninstaller.applescript'], TxtPath),
	atom_concat([TmpDir, '/', 'Uninstall Ciao.app'],      AppPath),

	open_output(TxtPath, Stream), Stream = o(_, Stream_),
	format(Stream_, "set PosixPath to \"~a\"

tell application \"Finder\" to set file_exists to exists PosixPath as POSIX file
if file_exists then
	beep
	display dialog \"Are you sure yo want de uninstall Ciao?\"
	do shell script PosixPath & \" NO_ASK_FOR_CONFIRMATION\" with administrator privileges
	display dialog \"Ciao uninstalled with success\" buttons \"OK\" default button \"OK\"
else
	display dialog \"Ciao does not seem to be installed on your system.\" buttons \"OK\" default button \"OK\"
end if
", [Path]),
	close_output(Stream),
	do(['osacompile -ai386 -o "', AppPath, '"  "', TxtPath, '"'], []).




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
% MacPort Portfile                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

homeURL("http://ciaohome.org").
packageDir("/packages/").
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

'Portfile' <- [~packagefilename(tgz)] #
  "Generate MacPorts Protfile. The source distribution is generated if missing."
	:-
	generate_portfile('makedir/mac/Portfile.skel', 'package/Portfile').

:- doc(generate_portfile/2, "@apl{generate_portfile( SkelFileName,
DestFileName )} generates macport portfile @var{DestFileName} assuming that 
 the skeleton of is @var{SkelFileName}").

generate_portfile(SkelFileName, DestFileName) :-
	PackageNameVersion =
            ~atom_codes(~component_packname_version_patch_rev(~component_wholesystem)),
	Rev = ~atom_codes(~svn_revision_atom),
	replace_strings_in_file([
          ["<v>Version</v>", ~atom_codes(~component_version_patch(ciaode))],
          ["<v>PackageNameVersion</v>", PackageNameVersion],
          ["<v>HomeURL</v>", ~homeURL],
          ["<v>MasterURL</v>", ~append([~homeURL, ~packageDir, Rev, "/", PackageNameVersion])],
	  ["<v>MD5CheckSum</v>", ~md5sum(~packagefilename(tgz))]],
	    SkelFileName, DestFileName).

:- doc(md5sum/2, " @apl{md5sum(File, CheckSum)} unifies
	@var{CheckSum} with the MD5 checksum of the file specified by
	@var{File}. @var{CheckSum} will be a string of 32 hexadecimal
	codes.").

%:- pred md5sum(File, Result) : string => string.

md5sum(File, Result) :-
	exec('md5sum', [File], In, Out, Err, wait, _Pid, ErrCode),
	(
	    ErrCode == 0,
	    read_codes(Out, Result1),
	    length(Result, 32),
	    append(Result, _, Result1),
	    check_hexa(Result) ->
	    close(Out), close(Err), close(In),
	    true
	;
	    repeat_str(Err),
	    close(Out), close(Err), close(In),
	    fail
	).

read_codes(Str, Result) :-
	get_code(Str, C),
	(
	    (C = -1; C=10) ->
	    Result = []
	;
	    Result = [C|T],
	    read_codes(Str, T)
	).

repeat_str(Str) :-
	get_code(Str, C),
	(
	    C = -1 ->
	    true
	;
	    put_code(C),
	    repeat_str(Str)
	).

check_hexa([H|T]) :-
	(
	    H >= 48, H < 58 ;
	    H >= 65, H < 71 ;
	    H >= 97, H < 103
	),
	check_hexa(T).
check_hexa([]).

write_xml_header(Str) :-
	format(Str, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple Computer//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
\n", []).


write_info_plist(File, Name, Version) :-
	open_output(File, OStr),
	OStr = o(_, Str),
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

write_welcome_html(RessourcePath, Name, Version) :-
	open_output(~atom_concat(RessourcePath, '/Welcome.html'), OStr), OStr =
	o(_, Str),
	format(Str, "<html lang=\"en\">
<head>
	<meta http-equiv=\"content-type\" content=\"text/html; charset=iso-8859-1\">
	<title>Install ~w</title>
</head>
<body>
<font face=\"Helvetica\"><b>Welcome to the ~w for Mac OS X Installer</b></font>
<p>
<font face=\"Helvetica\">Ciao is a public domain, next generation multi-paradigm programming environment with a unique set of features</font>
<p>
<font face=\"Helvetica\"><a href=\"~s\">~s</a></font><p>
	<font face=\"Helvetica\">This installer guides you through the steps necessary to install ~w ~w for Mac OS X. To get started, click Continue.</font>
</body>
</html>\n", [Name, Name, ~homeURL, ~homeURL, Name, Version]),
	close_output(OStr).



installer_app <- [descfile] :-
	initvals,
	installer_app.

installer_app :-
	bolder_message("creating MacOS bundle"),

	Domain = "es.upm.fi.dia.clip.ciaode",
	reallibdir(RealLibDir),
	ciaobindir(BinDir),
	atom_concat([~enginedir, '/ciaoengine.DARWINi86'], CiaoEngine),
	working_directory(WorkSpace, '.'),
	PackDir = ~atom_concat(WorkSpace, '/package'),
	TmpDir = ~make_temp_dir,

	BundlePath = ~atom_concat(TmpDir, '/CiaoDE.app'),
	RessourcesDir = ~atom_concat(BundlePath, '/Contents/Resources'),

	set_name_value(emacs_type, 'MacOSBundle'),
	component_make(ciao, emacs_support), % invoke 'emacs_support' on component 'ciao'
	del_name_value(emacs_type),


	replace_strings_in_file(
	    [["<VERSION>", ~atom_codes(~component_packname_version_patch_rev(~component_wholesystem))],
		["<REALLIBDIR>", ~atom_codes(RealLibDir)],
		["<CIAOENGINE>", ~atom_codes(CiaoEngine)],
		["<BINDIR>",     ~atom_codes(BinDir)],
		["<DOMAIN>",     Domain]],
	    'makedir/mac/CiaoDE.applescript.skel',
	    'makedir/mac/CiaoDE.applescript'),

	do(['osacompile -ai386 -o "', BundlePath, '"  "',
		'makedir/mac/CiaoDE.applescript', '"'], []),

	replace_strings_in_file(
	    [["<VERSION>", ~atom_codes(~component_packname_version_patch_rev(~component_wholesystem))],
		["<SORTVERSION>", ~atom_codes(~component_version_patch(ciaode))],
		["<DOMAIN>",      Domain]],
	    'makedir/mac/Info.plist.skel',
	    ~atom_concat(TmpDir, '/CiaoDE.app/Contents/Info.plist')),

	do(['rm -f "', RessourcesDir, '/applet.icns"'], []),

	install_to_destroot(RessourcesDir),
	do(['cp -f makedir/mac/ciao-icon.icns "', RessourcesDir, '"'], []),

	replace_strings_in_file(
	    [["<CIAOENGINE>", ~atom_codes(CiaoEngine)],
		["<BINDIR>",     ~atom_codes(BinDir)],
		["<REALLIBDIR>", ~atom_codes(RealLibDir)],
		["<DOMAIN>",     Domain]],
	    'makedir/mac/configure_dotemacs.pl.skel',
	    'makedir/mac/configure_dotemacs.pl'),
	do(['mkdir -p ', RessourcesDir, BinDir], []),
	make_exec(['makedir/mac/configure_dotemacs.pl'],
	    ~atom_concat([RessourcesDir, BinDir, '/configure_dotemacs'])),

	open_output(~atom_concat([RessourcesDir, RealLibDir, '/sample.pl']),
	    OStr), OStr = o(_, Str),
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

", []),
	close_output(OStr),

	package_dmg(PackDir, [BundlePath]),
	do(['mv "', BundlePath, '" ', PackDir], []),
	do(['rm -rf ', TmpDir],                 []).
