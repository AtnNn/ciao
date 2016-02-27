:- module(_, [], [assertions, fsyntax]).

:- use_module(library(aggregates)).

:- use_module(library(distutils)).
:- use_module(library(distutils(dirutils))).
:- use_module(library(distutils(distpkg_meta))).

:- use_module(library(terms), [atom_concat/2]).

:- use_module(lpdocsrc(src(autodoc_settings))).
:- use_module(lpdocsrc(src(autodoc_html_template))).

% ---------------------------------------------------------------------------

:- export(fmt_distpkg_download/2).
fmt_distpkg_download(Which, R) :-
	% Load metadata of all the packages
	PkgDir = ~distpkg_root,
	AllPkgMetas = ~distpkg_load_metas_at(PkgDir),
	%
	( Which = latest ->
	    PkgMeta = ~newest_pkgmeta(AllPkgMetas)
	; Which = stable ->
	    StableRev = ~setting_value(stable_revision),
	    ( PkgMeta = ~lookup_pkgmeta(StableRev, AllPkgMetas) ->
	        true
	    ; throw(pkgmeta_revision_not_found(StableRev))
	    )
	; fail
	),
	fmt_distpkg_download_(PkgMeta, R).

fmt_distpkg_download_(PkgMeta, R) :-
	SrcR = ~gen_source_list(PkgMeta),
	DocR = ~gen_manual_list(PkgMeta),
	%
	PkgName = ~distpkg_attr(PkgMeta, distpkg_name),
	PkgDate = ~distpkg_attr(PkgMeta, distpkg_date),
	PkgVerNice = ~distpkg_attr(PkgMeta, distpkg_version_nice),
	Params = [src_formats = SrcR,
	       	  manuals = DocR,
	       	  distpkg_name = string_esc(~atom_codes(PkgName)),
	       	  distpkg_date = string_esc(~atom_codes(PkgDate)),
	       	  distpkg_version_nice = string_esc(~atom_codes(PkgVerNice))],
        R = html_template_internal('download.html', Params).

gen_source_list(PkgMeta) := R :-
	Ds0 = ~distpkg_attr(PkgMeta, code),
	Ds = ~resolve_dfiles(Ds0, PkgMeta),
	gen_download_list(Ds, yes, R).

gen_manual_list(PkgMeta) := R :-
	Ds0 = ~distpkg_attr(PkgMeta, docs),
	Ds = ~resolve_dfiles(Ds0, PkgMeta),
	gen_download_list(Ds, no, R).

% ---------------------------------------------------------------------------
% Generate a download list of files.
%
% Each file is represented by a tuple:
%   distpkg_item_r(Kind, Desc, Path, Url): Kind is the file kind, Desc its description,
%     Path the filesystem path to the file and Url the URL where it accessible
%
% UseCGI=yes iff the file needs to be downloaded through a custom CGI
% application (e.g., to keep track of download statistics).
%
% TODO: Document

gen_download_list(Ds, UseCGI, R) :-
	gen_download_list_(Ds, UseCGI, BodyR),
	R = htmlenv(table, [class="download_table"], [
	      htmlenv(tbody, BodyR)
            ]).

gen_download_list_([], _, []).
gen_download_list_([DFile|R], UseCGI, [Row|Next]) :- !,
	DFile = distpkg_item_r(PkgFileKind, PkgFileTitle, PkgFilePath, PkgFileUrl),
	%
	format_sizeof(PkgFilePath, Size),
	% The href for the package file points to our download_cgi
	( UseCGI = yes -> Url = ~wrap_download_cgi(PkgFileUrl)
	; Url = PkgFileUrl
	),
	%
	Row = ~file_html_row(PkgFileKind, Url, PkgFileTitle, Size),
	%
	gen_download_list_(R, UseCGI, Next).

% Obtain a URL that passes through the download CGI
wrap_download_cgi(Url) := NewUrl :-
	Maillist = ~maillist,
	DownloadCGI = ~download_cgi,
	NewUrl = ~atom_concat([DownloadCGI, '?url=', Url, '&list=', Maillist]).

% This is the location of the download script
download_cgi := 'http://www.clip.dia.fi.upm.es/download_cgi/download.cgi'.
% Mailing list recommended (leave empty for no list)
maillist := 'ciao-users'.

% A table row for file download entry
% The url is in @var{Url}, its size is @var{Size}
file_html_row(Kind, Url, Desc, Size) := R :-
	distpkg_file_kind_info(Kind, KindText, KindImage),
	%
	DescR = htmlenv1(img, [src = ~img_url(KindImage), border="0"]),
	%
	( Desc = "" -> KindText2 = raw(KindText)
	; KindText2 = [raw(Desc), raw(" - "), raw(KindText)]
	),
	LinkR = htmlenv(a, [href = ~atom_codes(Url)], KindText2),
	%
	R = htmlenv(tr, [
              htmlenv(td, [align="right"], DescR), 
              htmlenv(td, [align="left"], [LinkR, htmlenv1(br, []), htmlenv(strong,raw(Size))])
            ]).

% Information of each package file kind
% TODO: some images like download_debian.png, download_fedora.png, etc. are unused
% TODO: The images are in the website skel (CIAOROOT/website/skel/images/)
distpkg_file_kind_info(tar_gz, "Source (All Platforms)", 'download_sources.png').
distpkg_file_kind_info(i386_rpm, "RPM based Linux (Fedora, Redhat, Centos, Mandriva)", 'download_rpms.png').
distpkg_file_kind_info(i386_deb, "Debian based Linux (Ubuntu, XUbuntu, Knoppix)", 'download_debs.png').
distpkg_file_kind_info(windows, "Windows 2000/XP/Vista", 'download_windows.png').
distpkg_file_kind_info(macosx, "Mac OS X", 'download_mac.png').
%
distpkg_file_kind_info(manual_ps, "PS", 'download_manual.png').
distpkg_file_kind_info(manual_pdf, "PDF", 'download_manual.png').

% ---------------------------------------------------------------------------
% Find and format the size of a file

:- use_module(library(system), [file_exists/1, file_property/2, working_directory/2]).
:- use_module(library(format)).

format_sizeof(File, Size) :-
	sizeof_file(File, Size0),
	formatted_size(Size0, Size).

sizeof_file(FileName, Size) :-
	( file_exists(FileName) ->
	    file_property(FileName, size(Size))
	; working_directory(W, W),
	  warning(['In dir ', W, ', file ', FileName, ' not found']),
	  Size = 0
	).

formatted_size(Size, FmtSize) :-
	(
	    Size > 2** 20 * 0.1 ->
	    Size2 is Size / 2** 20,
	    sformat(FmtSize, "~2f Mb", [Size2])
	;
	    Size > 2** 10 * 0.7 ->
	    Size2 is Size / 2** 10,
	    sformat(FmtSize, "~1f kb", [Size2])
	;
	    sformat(FmtSize, "~0f bytes", [Size])
	).

% ---------------------------------------------------------------------------

% The absolute distpkg root directory
distpkg_root := ~setting_value(distpkg_localdir).

% The URL to the distpkg root (for downloading)
distpkg_url := Url :-
	Url0 = ~setting_value(htmlurl),
	Url = ~atom_concat(~path_name(Url0), ~setting_value(distpkg_localurl)).

% ---------------------------------------------------------------------------
% Load all the package metadata found in a given directory

distpkg_load_metas_at(PkgDir) := AllPkgMetas :-
	AllPackageF = ~matching_files(~atom_concat(PkgDir, '/*/desc.tmpl')),
	AllPkgMetas = ~distpkg_load_metas(AllPackageF).

matching_files(Pattern) := Files :-
	findall(File, enum_matching_file(Pattern, File), Files).

enum_matching_file(FileC, RealFile) :-
	has_wildcards(FileC),
	!,
	get_abs_or_rel_path_with_wildcards(FileC, RealFile),
	\+ atom_concat(_, '/current/desc.tmpl', RealFile).
enum_matching_file(FileC, RealFile) :-
	( get_abs_path(FileC, RealFile) ->
	    true
	; message(error, ['File ', FileC, ' not found\n'])
	).

% ---------------------------------------------------------------------------

distpkg_load_metas([], []).
distpkg_load_metas([F|Fs], [V|Vs]) :-
	V = ~distpkg_load_meta(F),
	distpkg_load_metas(Fs, Vs).

% ---------------------------------------------------------------------------

newest_pkgmeta(AllPkgMetas) := PkgMeta :-
	SortedPkgMetas = ~sort_pkgmetas_by_version(AllPkgMetas),
	SortedPkgMetas = [PkgMeta|_].

% ---------------------------------------------------------------------------

% Lookup a distpkg
lookup_pkgmeta(Rev, AllPkgMetas) := PkgMeta :-
	% Find a pkgmeta whose 'basedir' param is Rev
	member(PkgMeta, AllPkgMetas),
	distpkg_has_name(PkgMeta, Rev),
	!.

% ---------------------------------------------------------------------------
% Obtain the resolved files (usable for a download list) of a PkgMeta

resolve_dfiles([], _, []).
resolve_dfiles([D|Ds], PkgMeta, [R|Rs]) :-
	D = distpkg_item(Kind, Desc, File),
	Path = ~distpkg_file_path(PkgMeta, File),
	Url = ~distpkg_file_url(PkgMeta, File),
	R = distpkg_item_r(Kind, Desc, Path, Url),
	resolve_dfiles(Ds, PkgMeta, Rs).

% URL to a distpkg item
distpkg_file_url(PkgMeta, PkgFile) := Url :-
	BaseDir = ~distpkg_attr(PkgMeta, basedir),
	atom_concat(~distpkg_root, Rest, BaseDir),
	PkgUrl = ~distpkg_url,
	Url = ~atom_concat([PkgUrl, Rest, '/', PkgFile]).

% path to a distpkg item
distpkg_file_path(PkgMeta, PkgFile) := File :-
	BaseDir = ~distpkg_attr(PkgMeta, basedir),
	File = ~atom_concat([BaseDir, '/', PkgFile]).

% ---------------------------------------------------------------------------

% TODO: todo items from previous version
:- doc(bug, "Nicer formatting of dates").
:- doc(bug, "Redesign @apl{download_cgi}").
:- doc(bug, "Are the texts of LGPL and GPL available?").

