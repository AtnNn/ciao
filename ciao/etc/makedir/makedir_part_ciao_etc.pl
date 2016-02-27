% ===========================================================================
:- module(_, _, [make, fsyntax, assertions, define_flag, regexp]).
% ===========================================================================
:- doc(title,  "Ciao etc Compilation/Installation installer").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").
% ===========================================================================

:- use_module(library(make(system_extra)), [replace_strings_in_file/3,
		copy_file/3, del_file_nofail/1, del_files_nofail/1, do/2,
		'-'/1, '--'/1, delete_file/1]).
:- use_module(library(llists),     [flatten/2]).
:- use_module(library(lists),      [append/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(terms)).
:- use_module(library(distutils)).
:- use_module(library(autoconfig)).
:- use_module(library(distutils(setperms))).
:- use_module(ciaodesrc(makedir('ConfigValues'))).
:- use_module(ciaodesrc(makedir('DOCCOMMON'))).

plutility :=
	fileinfo|
	get_deps|
	pldiff|
	viewpo|
	lpmake|
	cleandirs|
	plindent|
	show_asr|
	show_deps|
	compiler_output|
	synch_actions|
	checkline.

%dotcshrc     := ~atom_concat(~srcbindir, '/DOTcshrc').
%dotprofile   := ~atom_concat(~srcbindir, '/DOTprofile').
basefile := ~atom_concat([~srcbindir, '/', ~versionmain]).
ciao_get_arch := 'ciao_get_arch'.

shscript := ~ciao_get_arch|~basemain.

abs_plutility := ~atom_concat([~srcbindir, '/', ~plutility,
		'-', ~vers, ~get_ciao_ext]).
abs_ciao_get_arch := ~atom_concat([~srcbindir, '/', ~ciao_get_arch,
		'-', ~vers]).
abs_shscript := ~atom_concat([~srcbindir, '/', ~shscript,
		'-', ~vers]).

exe_header := ~atom_concat(~component_src(ciao), '/lib/compiler/header').

all <- [comp_message, 'DOTcshrc', 'DOTprofile'|~append(
		~findall(P, abs_plutility(P)),
		~findall(P, abs_shscript(P)))] :- true.

install <- [] # "Install ciao shell utilities." :-
	ciaobindir(BinDir),
	libdir(LibDir),
	build_root(BuildRoot),
	atom_concat(BuildRoot, BinDir, BuildBinDir),
	vers(Vers),
	versionmain(VersionMain),
	srcbindir(SrcBinDir),
	mkdir_perm(BuildBinDir, ~perms),
% Copy shell initialization files
	copy_file('DOTprofile', ~atom_concat(BuildRoot, ~reallibdir),
	    [overwrite]),
	-set_perms(~atom_concat([BuildRoot, ~reallibdir,
		    '/DOTprofile']), ~perms),
	do(['ln -sf ', ~relreallibdir, '/DOTprofile ', BuildRoot, LibDir,
		'/DOTprofile'], nofail),
	copy_file('DOTcshrc', ~atom_concat(BuildRoot, ~reallibdir), [
		overwrite]),
	do(['ln -sf ', ~relreallibdir, '/DOTcshrc ', BuildRoot, LibDir,
		'/DOTcshrc'], nofail),
% Copy startup script
	atom_concat([SrcBinDir,   '/', VersionMain], SourceVersionMain),
	atom_concat([BuildBinDir, '/', VersionMain], TargetVersionMain),

	del_file_nofail(TargetVersionMain),
	copy_file(SourceVersionMain, TargetVersionMain, [overwrite]),
	-set_exec_perms(TargetVersionMain, ~perms),
	--copy_file(~versionmain, ~atom_concat([BuildBinDir, '/', ~basemain
		]),
	    [overwrite, symlink]),
	( install_prolog_name(yes) ->
	    --copy_file(~versionmain, ~atom_concat([BuildBinDir, '/prolog'
		    ]),
		[overwrite, symlink])
	;
	    true
	),
	get_ciao_ext(Ext),
	(
	    (
		plutility(BaseFile),
		atom_concat([SrcBinDir, '/', BaseFile, '-', Vers, Ext],
		    SourceFile),
		atom_concat([BaseFile, '-', Vers, Ext],
		    TargetFile)
	    ;
		shscript(BaseFile),
		atom_concat([SrcBinDir, '/', BaseFile, '-', Vers],
		    SourceFile),
		atom_concat([BaseFile, '-', Vers],
		    TargetFile)
	    ),
	    --delete_file(~atom_concat([BuildRoot, BinDir, '/',
			TargetFile])),
	    --delete_file(~atom_concat([BuildRoot, BinDir, '/',
			BaseFile])),
%	    -copy_file(SourceFile, ~atom_concat([BuildRoot,BinDir,'/',
%	        TargetFile]), [overwrite]),
	    -copy_file(~atom_concat([SrcBinDir, '/', TargetFile]),
		~atom_concat(BuildRoot, BinDir), [overwrite]),
%	    -copy_file(~atom_concat([SrcBinDir, '/', BaseFile]),
%		~atom_concat(BuildRoot, BinDir), [overwrite]),
	    --copy_file(TargetFile, ~atom_concat([BuildBinDir, '/',
			BaseFile]),
		[overwrite, symlink]),
	    -set_exec_perms(~atom_concat([BuildBinDir, '/', BaseFile]),
		~perms),
	    -set_exec_perms(~atom_concat([BuildBinDir, '/', TargetFile]),
		~perms),
	    fail
	;
	    true
	).

uninstall <- :-
	justuninstall(~instype).

justuninstall(src).
justuninstall(ins) :- douninstall.

douninstall :-
	ciaobindir(BinDir),
	build_root(BuildRoot),
	libdir(LibDir),
	reallibdir(RealLibDir),
	del_files_nofail([
		~atom_concat([BuildRoot, LibDir, '/DOTprofile']),
		~atom_concat([BuildRoot, LibDir, '/DOTcshrc']),
		~atom_concat([BuildRoot, BinDir, '/prolog']),
		~atom_concat([BuildRoot, RealLibDir, '/DOTprofile']),
		~atom_concat([BuildRoot, RealLibDir, '/DOTcshrc']),
		~atom_concat([BuildRoot, BinDir, '/', ~basemain]),
		~atom_concat([BuildRoot, BinDir, '/', ~versionmain]),
		~atom_concat([BuildRoot, LibDir, '/NewUser'])]),
	get_ciao_ext(Ext),
	vers(Vers),
	(
	    (
		plutility(BaseFile),
		atom_concat([BaseFile, '-', Vers, Ext], File)
	    ;
		shscript(BaseFile),
		atom_concat([BaseFile, '-', Vers], File)
	    ),
% 	    display('deleting '),display([~atom_concat([BinDir,'/',File]),
% 	    ~atom_concat([BinDir,'/',BaseFile])]),nl,
	    --delete_file(~atom_concat([BuildRoot, BinDir, '/', BaseFile
		    ])),
	    --delete_file(~atom_concat([BuildRoot, BinDir, '/', File
		    ])),
	    fail
	;
	    true
	),
	( install_prolog_name(yes) ->
	    --delete_file(~atom_concat([BuildRoot, ~ciaobindir,
			'/prolog']))
	;
	    true
	).

optimizing_compiler_csh_string :=
	~optimizing_compiler_csh_string_(~optimizing_compiler).

optimizing_compiler_sh_string :=
	~optimizing_compiler_sh_string_(~optimizing_compiler).

optimizing_compiler_csh_string_(no) := "".
optimizing_compiler_csh_string_(yes) :=
	~flatten(["eval `", ~atom_codes(~ciao_lib_dir),
		"/optim_comp/ciaotool csh-env`"]).

optimizing_compiler_sh_string_(no) := "".
optimizing_compiler_sh_string_(yes) :=
	~flatten(["eval $(", ~atom_codes(~ciao_lib_dir),
		"/optim_comp/ciaotool bash-env)"]).

% Generate shell initialization files
'DOTcshrc' <- ['DOTcshrc.skel', '../SETTINGS'] :: FileName :-
	normal_message("Creating ~w", [FileName]),
	replace_strings_in_file([
		["<v>CiaoDocDir</v>", ~atom_codes(~docdir)],
		["<v>CiaoBinDir</v>", ~atom_codes(~ciaobindir)],
		["<v>ConfigOptimizingCompiler</v>",
		    ~optimizing_compiler_csh_string
		]],
	    'DOTcshrc.skel', FileName).

'DOTprofile' <- ['DOTprofile.skel', '../SETTINGS'] :: FileName :-
	normal_message("Creating ~w", [FileName]),
	replace_strings_in_file([
		["<v>CiaoDocDir</v>", ~atom_codes(~docdir)],
		["<v>CiaoBinDir</v>", ~atom_codes(~ciaobindir)],
		["<v>ConfigOptimizingCompiler</v>",
		    ~optimizing_compiler_sh_string
		]],
	    'DOTprofile.skel', FileName).

% Generate startup script
~basefile <- ['ciao.skel', '../SETTINGS'] :: FileName :-
	normal_message("Creating ~w", [FileName]),
	dobasefile(FileName).

dobasefile(FileName) :-
	ciao_extra_commands(ExtraCommands),
	replace_strings_in_file([
		["<v>ExtraCommands</v>", ExtraCommands],
		["<v>CiaoVersion</v>",   ~atom_codes(~vers)],
		["<v>CiaoSuffix</v>",    ~atom_codes(~get_ciao_ext)],
		["<v>CiaoBinDir</v>",    ~atom_codes(~ciaobindir)]],
	    'ciao.skel', FileName),
	--copy_file(~versionmain,
	    ~atom_concat([~srcbindir, '/', ~basemain]),
	    [overwrite, symlink]),
	( install_prolog_name(yes) ->
	    --copy_file(~versionmain,
		~atom_concat([~srcbindir, '/prolog']),
		[overwrite, symlink])
	;
	    true
	),
	-set_exec_perms(FileName, ~perms).

~abs_ciao_get_arch <- [ciao_get_arch] :: FileName :-
	normal_message("Creating ~w", [FileName]),
	copy_file(ciao_get_arch, FileName, [overwrite]),
	--copy_file(~atom_concat('ciao_get_arch-', ~vers),
	    ~atom_concat([~srcbindir, '/ciao_get_arch']), [overwrite,
		symlink]).

% Generate initial user message:  It Must be created after NewUser
% I think this must not be created here, even more, the extension is incorrect

% 'NewUser-install' <- ['../NewUser'] :-
% 	replace_strings_in_file([
% 	    ["<LIBROOT>",  ~atom_codes(~ciaolibroot)],
% 	    ["<LPDOCDIR>", ~atom_codes(~ciaodocroot)]],
% 	    ~atom_concat(~component_src(ciao),'/NewUser'),
% 	    'NewUser-install').

comp_message <- :-
	bold_message("Compiling utilities in etc directory").

plsource(FileName, Vers, SrcBinDir, BaseFile, AbsSourceFile) :-
	vers(Vers),
	srcbindir(SrcBinDir),
	get_ciao_ext(Ext),
	atom_concat([SrcBinDir, '/', BaseFile, '-', Vers, Ext], FileName),
	atom_concat(BaseFile, '.pl', SourceFile),
	atom_concat([~component_src(ciao), '/etc/', SourceFile],
	    AbsSourceFile).

plsource_dep(FileName, Vers, SrcBinDir, BaseFile, SourceFile, SourceFile) :-
	plsource(FileName, Vers, SrcBinDir, BaseFile, SourceFile).

~abs_plutility <- [~plsource_dep(FileName, Vers, SrcBinDir, BaseFile,
		AbsSourceFile),
	    ~exe_header] :: FileName :-
	normal_message("Creating ~w", [BaseFile]),
	build_executable(AbsSourceFile, Vers, SrcBinDir).

cleanetc <- :-
	del_file_nofail(~abs_plutility), fail ; true.

% 	compile_util(FileBase,FileBase).

% compile_util(FileBase,File) :-
% 	get_ciao_ext(Ext),
% 	display('Compiling '),display(File),display('...\n'),
% 	catch(make_exec([FileBase],~atom_concat([File,Ext])), E,
% 	(display(E),throw(E))).
%	do(['ln -sf ', File, Ext, ' ', FileBase], nofail).

% realclean <- [distclean] :- true.
