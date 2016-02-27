% ===========================================================================
:- module(_, _, [make, fsyntax]).
% ===========================================================================
:- doc(title,  "Windows Installer Generator.").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").
% ===========================================================================

:- use_module(library(aggregates)).
:- use_module(library(autoconfig)).
:- use_module(library(distutils)).
:- use_module(library(streams)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(messages)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(file_utils)).
:- use_module(library(make(system_extra))).
:- use_module(ciaodesrc(makedir('ConfigValues'))).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).
:- use_module(ciaodesrc(makedir(makedir_common))).


:- doc(iscc/1, "Specifies the path where the Inno Setup Compiler
   is. ISCC is required to build a pretty windows installer.  To get
   the latest version vist http://www.jrsoftware.org/isdl.php and
   download the QuickStart Pack.").

iscc := '/cygdrive/c/Program\\ Files/Inno\\ Setup\\ 5/iscc.exe'.

default_dir_name := ~atom_codes(~packname(~wholesystem)).

installer_win32 <- :-
	simple_message("Creating ISS scripts, please be patient ..."),
	ciaode_iss('CiaoDE.iss', 'file_list.iss',
	    ~atom_codes(~version(~wholesystem))),
	file_list_iss('file_list.iss'),
	installer_win32.

installer_win32_test <- :-
	ciaode_iss('CiaoDE_test.iss', 'file_list_test.iss',
	    ~atom_codes(~version(~wholesystem))),
	file_list_test_iss('file_list_test.iss'),
	installer_win32_test.

'CiaoDE.iss' <- ['makedir/CiaoDE.iss.skel'] :: FileName :-
	OutputBaseFileName = ~atom_codes(~version(~wholesystem)),
	ciaode_iss(FileName, 'file_list.iss', OutputBaseFileName).

'CiaoDE_test.iss' <- ['makedir/CiaoDE.iss.skel'] :: FileName :-
	OutputBaseFileName = ~append(~atom_codes(~version(~wholesystem)),
	    "-test"),
	ciaode_iss(FileName, 'file_list_test.iss', OutputBaseFileName).

'file_list.iss' <- [] :: FileName :-
	file_list_iss(FileName).

'file_list_test.iss' <- [] :: FileName :-
	file_list_test_iss(FileName).

% 'ciao/Win32/wsetup.cpx' <- ['ciao/Win32/wsetup.pl'] :-
% 	make_exec(['ciao/Win32/wsetup.pl'], 'ciao/Win32/wsetup.cpx').

installer_win32 :-
	installer_win32_('CiaoDE.iss').

installer_win32_test :-
	installer_win32_('CiaoDE_test.iss').

installer_win32_(FileIss) :-
	simple_message("Creating Windows installer ~w, please be patient ...",
	    [~version(~wholesystem)]),
	do([~iscc, ' ', FileIss], ~command_option),
	string_to_file("", ~atom_concat(~output_dir_name,
		'/NODISTRIBUTE')).

ciaode_iss(FileName, FileListName, OutputBaseFileName) :-
	atom_codes(~componentversion(~wholesystem), ComponentVersion),
	ciaode_revision_string(S),
	flatten([ComponentVersion, " (r" || S, ")"], Version),
	replace_strings_in_file([
		["<MyAppName>", "Ciao Development Environment"],
		["<MyAppVerName>", "Ciao Development Environment " ||
		    Version],
		["<OutputBaseFileName>", OutputBaseFileName],
		["<MyAppPublisher>",     "The CLIP Laboratory"],
		["<LicenseFile>",        ~license_file],
		["<MyAppExeName>", ~atom_codes(~atom_concat(['ciaosh-',
				~vers, ~get_ciao_ext]))],
		["<SourceDir>",        ~source_dir],
		["<OutputDir>",        ~output_dir],
		["<ManualIcons>",      ~get_manual_icons],
		["<DefaultDirName>",   ~default_dir_name],
		["<CiaoEngineBinDir>", ~ciao_engine_bin_dir_c],
		["<FileListName>",     ~atom_codes(FileListName)]
	    ],
	    'makedir/CiaoDE.iss.skel', FileName).

get_manual_icons(S) :-
	findall(["Name: {group}\\" || SPackName,
		" Manual in PDF; Filename: {app}\\build\\doc\\" || SVersionMain,
		".pdf; WorkingDir: {app}\\build\\doc\n"],
	    ( component(Component), vpmain(Component, VersionMain),
		packname(Component, PackName),
		atom_codes(PackName,    SPackName),
		atom_codes(VersionMain, SVersionMain) ), L),
	flatten(L, S).

file_list_iss(FileName) :-
	file_list_type_iss(all, FileName).

file_list_test_iss(FileName) :-
	file_list_type_iss(test, FileName).

file_list_type_iss(Type, FileName) :-
	open_output(FileName, Output),
	output_file_list(Type),
	close_output(Output).

output_file_list(Type) :-
	% TODO: Verify that this line is necessary
	mkdir_perm('build', ~perms),
	% TODO: Verify that this code is OK
	mkdir_perm('build/doc', ~perms),
	do_file_list(Type).

do_file_list(Type) :-
	(
	    current_file(Type, Source, DestDir),
	    display_file_entry(Source, DestDir),
	    fail
	;
	    true
	).

current_file(all, Source, DestDir) :-
	(
	    ciao_bin_distribution(FileName),
	    atom_concat('./', BaseFile, FileName),
	    fullwinname(BaseFile, Source),
	    fullwinpath(BaseFile, DestDir)
	;
	    extra_file(Source, DestDir)
	).

current_file(test, Source, DestDir) :-
	extra_file(Source, DestDir).

ciao_engine_posix_bin_dir :=
	~atom_concat(['build/objs/', ~get_platform]).
ciao_engine_bin_dir := ~winpath(relative, ~ciao_engine_posix_bin_dir).
ciao_engine_bin_dir_c := ~atom_codes(~ciao_engine_bin_dir).

distribute_dir('./') :=
'./ciao/doc|./ciao/engine|./lpdoc/doc|./ciaopp/doc|./doc|./ciao/optim_comp'.

ciao_bin_distribution(FileName) :-
	distribute_dir(DistributeDir, SkipDirs),
	enum_distpkg_codeitem_contents(bin, DistributeDir, SkipDirs, _, _, FileName).

extra_file(~winpath(~extra_system_file)) :=
	~atom_concat(~ciao_engine_bin_dir, '\\').

each_line(Lines0, Line) :-
	append(Line0, [0'\n|Lines], Lines0) ->
	(
	    Line = Line0
	;
	    each_line(Lines, Line)
	)
    ;
	Line = Lines0.

extra_system_file('/usr/bin/sh.exe').
extra_system_file(A) :-
	do_str(['ldd /usr/bin/sh.exe|grep \"/usr/bin\"|',
		'sed -e s:".* => \\(.*\\) (0.*":"\\1":g'], nofail, Lines),
	each_line(Lines, Line),
	Line \= [],
	atom_codes(A, Line).
extra_system_file('/usr/bin/cyggsl-0.dll').
extra_system_file('/usr/bin/cyggslcblas-0.dll').
extra_system_file('/usr/lib/lapack/cygblas.dll').
extra_system_file := ~atom_concat([~ciao_engine_bin_dir, '/ciaoengine',
		~get_exec_ext]).
extra_system_file := ~atom_concat([~ciao_engine_bin_dir, '/libciao.dll']).

display_file_entry(Source, DestDir) :-
	display_list(['Source: ', Source, '; DestDir:{app}\\', DestDir,
		'\n']).

license_file := ~atom_codes(~winpath(relative, ~atom_concat(
		    ~component_src(ciaode), '/LGPL'))).

source_dir := ~atom_codes(~winpath(relative, ~component_src(ciaode))).
output_dir := ~atom_codes(~winpath(relative, ~output_dir_name)).

fullwinname(File, WinName) :-
	winpath(relative, File, WinName).

fullwinpath(File, WinPath) :-
	extract_unix_filepath(File, Path),
	winpath(relative, Path, WinPath).

decompose_win_filename_string(FullName, Dir, Name, Extension) :-
	decompose_filename_string(FullName, 0'\\, Dir, Name, Extension).

decompose_unix_filename_string(FullName, Dir, Name, Extension) :-
	decompose_filename_string(FullName, 0'/, Dir, Name, Extension).

decompose_filename_string(FullName, PathSeparator, Dir, Name, Extension) :-
	append(Main, [PathSeparator|SubPath], FullName),
	!,
	decompose_filename2_string(Main, PathSeparator, Dir, SubPath, Name,
	    Extension).
decompose_filename_string(FullName, PathSeparator, "", Name, Extension) :-
	append(Name, [PathSeparator|Extension], FullName),
	!.
decompose_filename_string(Name, _, "", Name, "").

decompose_filename2_string(Main, PathSeparator, Dir, SubPath, Name,
	    Extension) :-
	decompose_filename_string(SubPath, PathSeparator, Dir2, Name,
	    Extension),
	append(Main, [PathSeparator|Dir2], Dir).

extract_win_filepath(FullName, Path) :-
	extract_filepath(FullName, 0'\\, Path).

extract_unix_filepath(FullName, Path) :-
	extract_filepath(FullName, 0'/, Path).

extract_filepath(FullName, PathSeparator, Path) :-
	decompose_filename_string(~atom_codes(FullName), PathSeparator,
	    PathC, _, _),
	atom_codes(Path, PathC).
