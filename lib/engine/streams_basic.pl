:- module(streams_basic, [
        open/3, close/1,
        set_input/1, current_input/1,
        set_output/1, current_output/1,
        character_count/2, line_count/2, line_position/2,
        flush_output/1, flush_output/0, clearerr/1,
        current_stream/3, stream_code/2,
        absolute_file_name/2, absolute_file_name/7,
        sourcename/1, stream/1, io_mode/1
        ],
        [assertions, isomodes]).

:- comment(title, "Basic file/stream handling").

:- comment(author, "Daniel Cabeza").
:- comment(author, "Mats Carlsson").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module,"This module provides basic predicates for handling
   files and streams, in order to make input/output on them.").

:- use_module(engine(internals), [
        '$find_file'/8, '$ferror_flag'/2, '$open'/3]).

:- impl_defined([
        stream_code/2, close/1, current_input/1, set_input/1,
        current_output/1, set_output/1,
        character_count/2, line_position/2, line_count/2, 
        flush_output/0, flush_output/1, clearerr/1, current_stream/3]).

:- comment(open(File, Mode, Stream), "Open @var{File} with mode
   @var{Mode} and return in @var{Stream} the stream associated with the
   file.  No extension is implicit in @var{File}.").

:- true pred open(+sourcename, +io_mode, ?stream) + iso
	# "Normal use.".
:- true pred open(+int, +io_mode, ?stream) # "In the special case that
        @var{File} is an integer, it is assumed to be a file descriptor
        passed to Prolog from a foreign function call. The file
        descriptor is connected to a Prolog stream (invoking the UNIX
        function @tt{fdopen}) which is unified with @var{Stream}.".

open(FileName, Mode, S) :-
	nonvar(Mode),
	io_mode(Mode), !,
	(   integer(FileName) -> AbsoluteFileName=FileName
	;   absolute_file_name(FileName, '', '', '.', AbsoluteFileName, _, _)
	),
	'$open'(AbsoluteFileName, Mode, S).
open(_, Mode, _) :- var(Mode), !,
        throw(error(instantiation_error, open/3-2)).
open(_, Mode, _) :-
        throw(error(domain_error(io_mode, Mode), open/3-2)).

:- comment(io_mode/1, "Can have the following values:
   @begin{description}

   @item{@tt{read}} Open the file for input.

   @item{@tt{write}} Open the file for output.  The file is created if
   it does not already exist, the file will otherwise be truncated.

   @item{@tt{append}} Open the file for output.  The file is created if
   it does not already exist, the file will otherwise be appended to.

   @end{description}").


:- prop io_mode(M) + regtype # "@var{M} is an opening mode ('read',
        'write' or 'append').".

io_mode(read).
io_mode(write).
io_mode(append).

:- comment(close(Stream), "Close the stream @var{Stream}.").

:- true pred close(+stream) + iso.

:- comment(set_input(Stream), "Set the current input stream to
   @var{Stream}.  A notion of @index{current input stream} is maintained
   by the system, so that input predicates with no explicit stream
   operate on the current input stream.  Initially it is set to
   @tt{user_input}.").

:- true pred set_input(+stream) + iso.

:- comment(current_input(Stream), "Unify @var{Stream} with the
   @concept{current input stream}.").

:- true pred current_input(?stream) + iso.

:- comment(set_output(Stream), "Set the current output stream to
   @var{Stream}.  A notion of @index{current output stream} is maintained
   by the system, so that output predicates with no explicit stream
   operate on the current output stream.  Initially it is set to
   @tt{user_output}.").

:- true pred set_output(+stream) + iso.

:- comment(current_output(Stream), "Unify @var{Stream} with the
   @concept{current output stream}.").

:- true pred current_output(?stream) + iso.

:- comment(character_count(Stream,Count), "@var{Count} characters have
   been read from or written to @var{Stream}.").

:- true pred character_count(+stream,?int).

:- comment(line_count(Stream,Count), "@var{Count} lines have been read
   from or written to @var{Stream}.").

:- true pred line_count(+stream,?int).

:- comment(line_position(Stream,Count), "@var{Count} characters have
   been read from or written to the current line of @var{Stream}.").

:- true pred line_position(+stream,?int).

:- comment(flush_output(Stream), "Flush any buffered data to output
   stream @var{Stream}.").

:- true pred flush_output(+stream) + iso.

:- comment(flush_output, "Behaves like @tt{current_output(S),
   flush_output(S)}").

:- true pred flush_output + iso.

:- comment(clearerr(Stream), "Clear the end-of-file and error indicators
           for input stream @var{Stream}.").

:- true pred clearerr(+stream).

:- comment(current_stream(Filename,Mode,Stream), "@var{Stream} is a
   stream which was opened in mode @var{Mode} and which is connected to
   the absolute file name @var{Filename} (an atom) or to the file
   descriptor @var{Filename} (an integer).  This predicate can be used
   for enumerating all currently open streams through backtracking.").

:- true pred current_stream(?atm,?io_mode,?stream).
:- true pred current_stream(?int,?io_mode,?stream).

:- comment(stream_code(Stream,StreamCode), "@var{StreamCode} is the file
   descriptor (an integer) corresponding to the Prolog stream
   @var{Stream}.").

:- true pred stream_code(+stream,?int).
:- true pred stream_code(-stream,+int).

:- comment(file_search_path(Alias, Path), "The @concept{path alias}
   @var{Alias} is linked to path @var{Path}.  Both arguments must be
   atoms.  New facts (or clauses) of this predicate can be asserted to
   define new path aliases.  Predefined path aliases in Ciao are:

@begin{description}

@item{@tt{library}} Initially points to all Ciao library paths.  See
   @pred{library_directory/1}.

@item{@tt{engine}} The path of the @concept{Ciao engine builtins}.

@item{@tt{.}} The current path (@tt{'.'}).

@end{description}
").

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

:- comment(library_directory(Path), "@var{Path} is a library path (a
   path represented by the @concept{path alias} @tt{library}).
   Predefined library paths in Ciao are @tt{'$CIAOLIB/lib'},
   @tt{'$CIAOLIB/library'}, and @tt{'$CIAOLIB/contrib'}, given that
   @tt{$CIAOLIB} is the path of the root ciao library directory.  More
   library paths can be defined by asserting new facts (or clauses) of
   this predicate.").

:- multifile library_directory/1.
:- dynamic library_directory/1.


:- comment(absolute_file_name(RelFileSpec, AbsFileSpec), "If
   @var{RelFileSpec} is an absolute pathname then do an absolute
   lookup. If @var{RelFileSpec} is a relative pathname then prefix the
   name with the name of the current directory and do an absolute
   lookup.  If @var{RelFileSpec} is a path alias, perform the lookup
   following the path alias rules (see @prop{sourcename/1}). In all
   cases: if a matching file with suffix @tt{.pl} exists, then
   @var{AbsFileSpec} will be unified with this file.  Failure to open
   a file normally causes an exception.  The behaviour can be
   controlled by the @tt{fileerrors} @concept{prolog flag}.").

:- true pred absolute_file_name(+RelFileSpec, -AbsFileSpec) 
   :: sourcename * atm
   # "@var{AbsFileSpec} is the absolute name (with full path) of 
      @var{RelFileSpec}.".

absolute_file_name(File, Abs) :-
	File==user, !, Abs=user.
absolute_file_name(File, Abs) :-
	absolute_file_name(File, '_opt', '.pl', '.', Abs, _, _).

:- comment(
   absolute_file_name(Spec, Opt, Suffix, CurrDir, AbsFile, AbsBase, AbsDir),
   "@var{AbsFile} is the absolute name (with full path) of @var{Spec},
   which has an optional first suffix @var{Opt} and an optional second
   suffix @var{Suffix}, when the current directory is @var{CurrDir}.
   @var{AbsBase} is the same as @var{AbsFile}, but without the second
   suffix, and @var{AbsDir} is the absolute path of the directory where
   @var{AbsFile} is.  The Ciao compiler invokes this predicate with
   @var{Opt}=@tt{'_opt'} and @var{Suffix}=@tt{'.pl'} when searching
   source files.").

:- true pred absolute_file_name(+sourcename,+atm,+atm,+atm,-atm,-atm,-atm).

absolute_file_name(Spec, Opt, Suffix, _CurrDir, AbsFile, AbsBase, AbsDir) :-
        nonvar(Spec),
        functor(Spec, Alias, 1),
        arg(1,Spec,Name),
	atom(Name), !,
	(
            file_search_path(Alias, Dir),
	    atom(Dir),
	    '$find_file'(Dir, Name, Opt, Suffix, true,
                         AbsFile, AbsBase, AbsDir) ->
                true
	;
            (
                '$ferror_flag'(on, on) ->
                throw(error(existence_error(source_sink,Spec),
                            absolute_file_name/7-1))
            ;
                fail
            )
	).
absolute_file_name(Name, Opt, Suffix, CurrDir, AbsFile, AbsBase, AbsDir) :-
	atom(Name), !,
	'$find_file'(CurrDir, Name, Opt, Suffix, _, AbsFile, AbsBase, AbsDir).
absolute_file_name(X, _, _, _, _, _, _) :-
	throw(error(domain_error(source_sink, X), absolute_file_name/7-1)).

:- comment(stream/1, "Streams correspond to the file pointers used at
   the operating system level, and usually represent opened files.
   There are four special streams which correspond with the operating
   system standard streams:
@begin{description}

@item{@tt{user_input}} The standard input stream, i.e. the terminal, usually.

@item{@tt{user_output}} The standard output stream, i.e. the terminal, usually.

@item{@tt{user_error}} The standard error stream.

@item{@tt{user}} The standard input or output stream, depending on context.

@end{description}

").

:- true prop stream(S) + regtype # "@var{S} is an open stream.".

%  This does not capture the "not closed" fact
stream(user_input).
stream(user_output).
stream(user_error).
stream(user).
stream('$stream'(X,Y)) :- 
	int(X), int(Y).

:- comment(sourcename/1, "A source name is a flexible way of referring
   to a concrete file. A source name is either a relative or absolute
   filename given as:

@begin{itemize}

@item an atom, or

@item a unary functor (which represents a @index{path alias}, see
      below) applied to a @em{relative} path, the latter being given
      as an atom.

@end{itemize}

In all cases certain filename extensions (e.g., @tt{.pl}) can be
implicit.  In the first form above, file names can be relative to the
current directory.  Also, file names beginning with @tt{~} or @tt{$}
are treated specially.  For example,

@begin{description}

@item{@tt{'~/ciao/sample.pl'}}
is equivalent to @tt{'/home/staff/herme/ciao/sample.pl'}, if
@tt{/home/staff/herme} is the user's home directory.  (This is also
equivalent to @tt{'$HOME/ciao/sample.pl'} as explained below.)

@item{@tt{'~bardo/prolog/sample.pl'}}
is equivalent to @tt{'/home/bardo/prolog/sample.pl'}, if
@tt{/home/bardo} is bardo's home directory.

@item{@tt{'$UTIL/sample.pl'}}
is equivalent to @tt{'/usr/local/src/utilities/sample.pl'}, if
@tt{/usr/local/src/utilities} is the value of the environment variable
@tt{UTIL}.

@end{description}

   The second form allows using path aliases. Such aliases allow
   refering to files not with absolute file system paths but with
   paths which are relative to predefined (or user-defined) abstract
   names.  For example, given the path alias @tt{myutils} which has
   been defined to refer to path @tt{'/home/bardo/utilities'}, if that
   directory contains the file @tt{stuff.pl} then the term
   @tt{myutils(stuff)} in a @decl{use_module/1} declaration would
   refer to the file @tt{'/home/bardo/utilities/stuff.pl'} (the
   @tt{.pl} extension is implicit in the @decl{use_module/1}
   declaration).  As a special case, if that directory contains a
   subdirectory named @tt{stuff} which in turn contains the file
   @tt{stuff.pl}, the same term would refer to the file
   @tt{'/home/bardo/utilities/stuff/stuff.pl'}.  If a path alias is
   related to several paths, all paths are scanned in sequence until a
   match is found.  For information on predefined path aliases or how
   to define new path aliases, see @pred{file_search_path/2}.  ").

:- true prop sourcename(F) + regtype # "@var{F} is a source name.".

sourcename(S) :- atm(S).
sourcename(S) :- struct(S).

:- comment(version_maintenance,dir('../../version')).


