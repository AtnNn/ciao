:- module(pbundle_gen_win32, _, [ciaopaths, make, fsyntax]).

:- doc(title,  "pbundle Generation as Windows Installer").
:- doc(author, "Edison Mera").

:- doc(module, "This module provides the definitions used to generate
   installers for the Windows platform. Currently the installer is
   based on the @em{Inno Setup Compiler} (ISCC). Thus, ISCC is required
   in order to build a graphical installer. To get the latest version
   visit @href{http://www.jrsoftware.org/isdl.php} and download the
   @em{QuickStart Pack}.").

% ===========================================================================

:- use_module(library(aggregates)).
:- use_module(library(lpdist(distutils)), [enum_pbundle_codeitem_contents/6]).
:- use_module(library(streams)).
:- use_module(library(lists)).
:- use_module(library(llists)).
:- use_module(library(messages)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(file_utils)).
:- use_module(library(system_extra)).
:- use_module(library(lpdist(ciao_config_options))).
:- use_module(library(lpdist(ciao_bundle_db))).
:- use_module(library(lpdist(pbundle_gen_common))).
:- use_module(library(lpdist(makedir_aux)),
	[fsR/2, wr_template/4, prepare_doc_dirs/0]).

% ===========================================================================

:- doc(iscc/1, "Specifies the path where the @em{Inno Setup Compiler} is
   installed.").

iscc := '/cygdrive/c/Program\\ Files/Inno\\ Setup\\ 5/iscc.exe'.

default_dir_name := ~atom_codes(~bundle_packname(~bundle_wholesystem)).

gen_pbundle__win32 <- :- gen_pbundle__win32.

gen_pbundle__win32 :-
	simple_message("Creating ISS scripts, please be patient... "),
	FileListName = 'file_list.iss',
	create_iss_file(FileListName),
	file_list_iss(FileListName),
	gen_pbundle__win32_.

gen_pbundle__win32_ :-
	FileIss = 'CiaoDE.iss',
	simple_message("Creating Windows installer ~w, please be patient... ",
	    [~bundle_packname_version_patch_rev(~bundle_wholesystem)]),
	do([~iscc, ' ', FileIss], ~command_option),
	create_pbundle_output_dir.

% TODO: too many definitions here are hardwired
create_iss_file(FileListName) :-
	OutputBaseFileName = ~atom_codes(~bundle_packname_version_patch_rev(~bundle_wholesystem)),
	%
	BundleVersion = ~atom_codes(~bundle_version_patch(~bundle_wholesystem)),
	RevStr = ~bundle_svn_revstr,
	Version = ~flatten([BundleVersion, " (r" || RevStr, ")"]),
	%
	wr_template(cwd, ~lpdist_dir/'win32', 'CiaoDE.iss', [
	    'MyAppName' = "Ciao",
	    'MyAppVerName' = "Ciao"||Version,
	    'OutputBaseFileName' = OutputBaseFileName,
	    'MyAppPublisher' = "The CLIP Laboratory",
	    'LicenseFile' = ~license_file,
 	    'MyAppExeName' = ~fsR(concat_verk(ciao, plexe, 'ciaosh')),
	    'CiaoVersion' = ~bundle_version(ciao), 
	    'SourceDir' = ~source_dir,
	    'OutputDir' = ~output_dir,
	    'ManualIcons' = ~get_manual_icons,
	    'DefaultDirName' = ~default_dir_name,
	    'CiaoEngineExec' = ~winpath(relative, ~fsR(concat_k(exec, ~engine_bin_dir/('ciaoengine')))),
	    'FileListName' = ~winpath(full, ~fsR(bundle_src(ciaode)/FileListName))
	]).

get_manual_icons(S) :-
	findall(["Name: {group}\\" || SPackName,
		 " Manual in PDF; Filename: {app}\\build\\doc\\" || SVersionMain,
		 ".pdf; WorkingDir: {app}\\build\\doc\n"],
	    ( registered_bundle(Bundle), bundle_name_version_patch(Bundle, VersionMain),
		bundle_packname(Bundle, PackName),
		atom_codes(PackName,    SPackName),
		atom_codes(VersionMain, SVersionMain) ), L),
	flatten(L, S).

% Compute the list of files that will be included in the ISS file
file_list_iss(FileName) :-
 	prepare_doc_dirs, % TODO: is it necessary?
	open_output(FileName, Output),
	output_file_list,
	close_output(Output).

output_file_list :-
	( % (failure-driven loop)
	  current_file(Source, DestDir),
	    display_file_entry(Source, DestDir),
	    fail
	; true
	).

current_file(Source, DestDir) :-
	(
	    ciao_bin_distribution(FileName),
	    atom_concat('./', BaseFile, FileName),
	    fullwinname(BaseFile, Source),
	    fullwinpath(BaseFile, DestDir)
	;
	    extra_file(Source, DestDir)
	).

engine_bin_dir := ~fsR('build'/'objs'/(~get_platform)).

skip_distribute_dir('./') :=
    './ciao/doc|./ciao/engine|./lpdoc/doc|./ciaopp/doc|./doc|./ciao/optim_comp'.

ciao_bin_distribution(FileName) :-
	skip_distribute_dir(DistributeDir, SkipDirs),
	enum_pbundle_codeitem_contents(bin, DistributeDir, SkipDirs, _, _, FileName).

extra_file(Source, DestDir) :-
	Source = ~winpath(~extra_system_file), % (nondet)
	DestDir = ~atom_concat(~winpath(relative, ~engine_bin_dir), '\\').

each_line(Lines0, Line) :-
	( append(Line0, [0'\n|Lines], Lines0) ->
	    ( Line = Line0
	    ; each_line(Lines, Line)
	    )
        ; Line = Lines0
        ).

extra_system_file('/usr/bin/sh.exe').
extra_system_file(A) :-
	do_str(['ldd /usr/bin/sh.exe|grep \"/usr/bin\"|',
		'sed -e s:".* => \\(.*\\) (0.*":"\\1":g'], nofail, Lines),
	each_line(Lines, Line),
	Line \= [],
	atom_codes(A, Line).
% TODO: hardwired, why?
extra_system_file('/usr/bin/cyggsl-0.dll').
extra_system_file('/usr/bin/cyggslcblas-0.dll').
extra_system_file('/usr/lib/lapack/cygblas-0.dll').
% TODO: hardwired, why?
extra_system_file := ~fsR(concat_k(exec, ~engine_bin_dir/('ciaoengine'))).
extra_system_file := ~fsR(concat_k(ext('.dll'), ~engine_bin_dir/'libciao')).

display_file_entry(Source, DestDir) :-
	display_list(['Source: ', Source, '; DestDir:{app}\\', DestDir, '\n']).

license_file := ~atom_codes(~winpath(relative, ~fsR(bundle_src(ciaode)/'LGPL'))).

source_dir := ~atom_codes(~winpath(relative, ~fsR(bundle_src(ciaode)))).
output_dir := ~atom_codes(~winpath(relative, ~pbundle_output_dir)).

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

decompose_filename2_string(Main, PathSeparator, Dir, SubPath, Name, Extension) :-
	decompose_filename_string(SubPath, PathSeparator, Dir2, Name, Extension),
	append(Main, [PathSeparator|Dir2], Dir).

extract_win_filepath(FullName, Path) :-
	extract_filepath(FullName, 0'\\, Path).

extract_unix_filepath(FullName, Path) :-
	extract_filepath(FullName, 0'/, Path).

extract_filepath(FullName, PathSeparator, Path) :-
	decompose_filename_string(~atom_codes(FullName), PathSeparator, PathC, _, _),
	atom_codes(Path, PathC).

% ===========================================================================

% % TODO: this target is probably not necessary
% gen_pbundle__win32_test <- :-
% 	ciaode_iss('CiaoDE_test.iss', 'file_list_test.iss',
% 	    ~atom_codes(~bundle_packname_version_patch_rev(~bundle_wholesystem))),
% 	file_list_test_iss('file_list_test.iss'),
% 	gen_pbundle__win32_test.

% TODO: targets that are not necessary
% 'CiaoDE.iss' <- ['makedir/CiaoDE.iss.skel'] :: FileName :-
% 	OutputBaseFileName = ~atom_codes(~bundle_packname_version_patch_rev(~bundle_wholesystem)),
% 	ciaode_iss(FileName, 'file_list.iss', OutputBaseFileName).
% 
% 'CiaoDE_test.iss' <- ['makedir/CiaoDE.iss.skel'] :: FileName :-
% 	OutputBaseFileName = ~append(~atom_codes(~bundle_packname_version_patch_rev(~bundle_wholesystem)),
% 	    "-test"),
% 	ciaode_iss(FileName, 'file_list_test.iss', OutputBaseFileName).
% 
% 'file_list.iss' <- [] :: FileName :-
% 	file_list_iss(FileName).
% 
% 'file_list_test.iss' <- [] :: FileName :-
% 	file_list_test_iss(FileName).

% 'ciao/Win32/wsetup.cpx' <- ['ciao/Win32/wsetup.pl'] :-
% 	make_exec(['ciao/Win32/wsetup.pl'], 'ciao/Win32/wsetup.cpx').

% % TODO: this target is probably not necessary
% gen_pbundle__win32_test :-
% 	gen_pbundle__win32_('CiaoDE_test.iss').

% file_list_test_iss(FileName) :-
% 	file_list_type_iss(test, FileName).
