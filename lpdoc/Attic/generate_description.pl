% OLD CODE TO GENERATE HTML DESCRIPTIONS OF DOCUMENTS
% (This was part of autodoc.pl, but is unused)

%% ---------------------------------------------------------------------------
:- pred generate_description(Format, Main, OutputBase, LibPaths, SysLibPaths,
	    PathAliasF, Opts, ODir)
	: supported_format * filename * filename * list(atm) * list(atm)
	* filename * list(miscopt) * filename

# "Generates a @concept{brief description of the application or
      library} in a file. This file is intended for inclusion in a
      larger file that is a catalog of aplications or libraries. The
      file is produced in the format given by @var{Format}. @var{Main}
      is the name of a the source file which is the main file of the
      application. The name of the output file depends on @var{Format}
      -- see see @pred{supported_format/1} in library
      @lib{autodocformats}. @var{LibPaths} is a list of
      @concept{library paths} the module being documented may
      use. @var{SysLibPaths} is similar to @var{LibPaths} but provides
      paths to @em{system} libraries. @var{PathAliasF} is the name of
      a module containing path aliases.".
%% ---------------------------------------------------------------------------

generate_description(Format, Main, OutputBase, LibPaths, SysLibPaths,
	    PathAliasF, Opts, ODir) :-
	set_output_dir(ODir),
	%% For now, only html supported
	Format=html,
	Indices=[],

	IdxFormatSuffix=htmlindex,
	file_processing(
	    LibPaths, SysLibPaths, PathAliasF, Main, OutputBase,
	    IdxFormatSuffix, ODir,
	    Name, NDName, _M, I, _Base, Dir, IdxO,
	    IdxDirO, RefsName, []),
	DocSt = docstate(Format, Name, Indices, Opts, I),
	%
	try_finally(
	    refstream_open(RefsName, CS),
	    (
		get_last_version(_LVersion, Version, Dir, DocSt),
		get_doc(title,   note, DocSt, TitleR),
		get_doc(summary, note, DocSt, SummaryR),

		verbose_message("{Converting ~w into ~w", [I, IdxO]),
		try_finally(
		    open(IdxDirO, write, IdxOS),
		    (
			format(IdxOS,
			    "<A NAME=""~w""></A><HR>~n<H1 class=appltitle>", [
				Name]),
			format(IdxOS,
			    "<B class=applname>~w",                          [
				NDName]),
			( is_nonempty_doctree(TitleR) ->
			    format(IdxOS, ":</B> <em>", []),
			    doctree_write(TitleR, DocSt, IdxOS),
			    format(IdxOS, "</em></H1>~n~n", [])
			; format(IdxOS, "</B></H1>~n~n", [])
			),
			doctree_write(SummaryR, DocSt, IdxOS),
			format(IdxOS, "~n~n", []),

			( version_date(Version, Date),
			  version_numstr(Version, VerStr)
			-> 
			    format(IdxOS,
				"<H2>Current version (~s of ~w):</H2>~n~n",
				[VerStr, Date])
			; true )
		    ),
		    close(IdxOS)
		)
	    ),
	    refstream_close(CS)
	),
	verbose_message("}"),
	ttyflush,
	!.
generate_description(_, _, _, _, _, _, _, _) :-
	error_message("formatting could not be completed", []).
