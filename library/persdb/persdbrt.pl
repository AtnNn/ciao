:- module(persdbrt,
        [passertz_fact/1, 
	 pretract_fact/1, 
	 pcurrent_fact/1,
         init_persdb/0, 
	 initialize_db/0, 
	 make_persistent/2, 
	 update_files/2],
        [assertions,regtypes]).

:- use_module(engine(internals), [term_to_meta/2]).
:- use_module(library(lists)).
:- use_module(library(streams)).
:- use_module(library(read)).
:- use_module(library(system)).
:- use_module(library(file_locks)).
%:- use_module(engine(basic_props)).

:- comment(bug,"Shouldn't the declarations for persistent_dir/2 be
   inside persdb.pl? (would save a lot of writing).").

:- comment(bug,"make_persistent/2 should really be persistent/2 (since
   it doesn't really make a predicate persistent but rather declares
   it as such, i.e., we do not use make_data/1, we use data/1) ?").

:- comment(bug,"having to use pcurrent_fact is not so nice").

:- comment(bug,"passert, pretract, etc. should really be overloaded
   versions of assert, retract etc., i.e., it should be possible to
   use assertz_fact directly").

:- comment(bug,"we need also asserta_fact, right? Otherwise isn't it
   difficult to do many things?").

%% ---------------------------------------------------------------------------
:- comment(title, "Persistent predicate database").

:- comment(subtitle,"A Generic Database Interface").
:- comment(subtitle,"Technical Report CLIP 9/98.0").
:- comment(subtitle,"RadioWeb (ESPRIT Project 25562) Report D3.1.M1-A1").
:- comment(subtitle,"ECCOSIC (Fulbright Project 98059)").
%% :- comment(subtitle,"@em{Draft printed on:} @today{}").
:- comment(subtitle,"December 26, 1998").

:- comment(author, "J.M. Gomez, D. Cabeza, and M. Hermenegildo").
:- comment(author, "@tt{clip@@dia.fi.upm.es}").
:- comment(author, "@tt{http://www.clip.dia.fi.upm.es/}").
:- comment(author, "The CLIP Group").
:- comment(author, "Facultad de Inform@'{a}tica").
:- comment(author, "Universidad Polit@'{e}cnica de Madrid").

:- comment(copyright,"
Copyright @copyright{} 1997-98 The Clip Group.

@include{Copyright.Manuals}
").

:- comment(summary,"This library provides a means to define and modify
   @em{persistent predicates}. A persistent predicate is a relation
   such that any updates made to it from a program remain even after
   the execution of that program terminates.  Persistent predicates
   appear to a program as ordinary predicates, but their definitions
   are stored in files which are automatically maintained by the
   library. Any changes to the persistent predicates are recorded
   atomically and transactionally in these files. This essentially
   implements a light-weight, simple, and at the same time powerful
   deductive database, which is accessed via a generic data access
   method.  A companion library (@lib{persdb_sql}) provides a similar
   notion of persistence but uses external relational databases as
   storage instead. This essentially provides a high-level programmer
   interface, using the same generic data access method, to relational
   databases.").

:- comment(module,"

   @section{Introduction to persistent predicates}

   This library implements a @em{generic persistent predicate
   database}. The basic notion implemented by the library is that of a
   @concept{persistent predicate}. The persistent predicate concept
   provides a simple, yet powerful generic persistent data access
   method @cite{radioweb-D3.1.M1-A1,radioweb-ta}. A persistent
   predicate is a special kind of dynamic, data predicate that
   ``resides'' in some persistent medium (such as a set of files, a
   database, etc.) that is typically external to the program using
   such predicates. The main effect is that any changes made to to a
   persistent predicate from a program ``survive'' across
   executions. I.e., if the program is halted and restarted the
   predicate that the new process sees is in precisely the same state
   as it was when the old process was halted (provided no change was
   made in the meantime to the storage by other processes or the
   user).

   Persistent predicates appear to a program as ordinary predicates,
   and calls to these predicates can appear in clause bodies in the
   usual way. However, the definitions of these predicates do not
   appear in the program. Instead, the library maintains automatically
   the definitions of predicates which have been declared as
   persistent in the persistent storage. 

   @concept{Updates to persistent predicates} can be made by calling
   predicates similar to @pred{assertz_fact/1} and
   @pred{retract_fact/1}. The library makes sure that each update is a
   @concept{transactional update}, in the sense that if the update
   terminates, then the permanent storage has definitely been
   modified.  For example, if the program making the updates is halted
   just after the update and then restarted, then the updated state of
   the predicate will be seen. This provides security against possible
   data loss due to, for example, a system crash.  Also, due to the
   atomicity of the transactions, persistent predicates allow
   @concept{concurrent updates} from several programs.

   @section{Persistent predicates, files, and relational databases}

   The concept of persistent predicates provided by this library
   essentially implements a light-weight, simple, and at the same time
   powerful form of relational database (a @concept{deductive
   database}), and which is standalone, in the sense that it does not
   require external support, other than the file management
   capabilities provided by the operating system.  This is due to the
   fact that the persistent predicates are in fact stored in one or
   more auxiliary files in a given directory.

   This type of database is specially useful when building small to
   medium-sized standalone applications in Prolog which require
   persistent storage. In many cases it provides a much easier way of
   implementing such storage than using files under direct program
   control. For example, interactive applications can use persistent
   predicates to represent their internal state in a way that is close
   to the application. The persistence of such predicates then allows
   automatically restoring the state to that at the end of a previous
   session. Using persistent predicates amounts to simply declaring
   some predicates as such and eliminates having to worry about
   opening files, closing them, recovering from system crashes, etc.

   In other cases, however, it may be convenient to use a relational
   database as persistent storage. This may be the case, for example,
   when the data already resides in such a database (where it is
   perhaps accessed also by other applications) or the volume of data
   is very large. @lib{persdb_sql} @cite{radioweb-D3.1.M2-A2} is a
   companion library which implements the same notion of persistent
   predicates used herein, but keeping the storage in a relational
   database. This provides a very natural and transparent way to
   access SQL database relations from a Prolog program. In that
   library, facilities are also provided for reflecting more complex
   @em{views} of the database relations as predicates. Such views can
   be constructed as conjunctions, disjunctions, projections, etc. of
   database relations, and may include SQL-like aggregation
   operations.

   A nice characteristic of the notion of persistent predicates used
   in both of these libraries is that it abstracts away how the
   predicate is actually stored. Thus, a program can use persistent
   predicates stored in files or in external relational databases
   interchangeably, and the type of storage used for a given predicate
   can be changed without having to modify the program (except for
   replacing the corresponding @pred{persistent/2} declarations).

   An example application of the @lib{persdb} and @lib{persdb_sql}
   libraries (and also the @lib{pillow} library @cite{pillow-www6}),
   @comment{should be pillow-ws, but formats weird}is @apl{WebDB}
   @cite{radioweb-D3.1.M2-A3}. @apl{WebDB} is a generic, highly
   customizable @em{deductive database engine} with an @em{html
   interface}. @apl{WebDB} allows creating and maintaining
   Prolog-based databases as well as relational databases (residing in
   conventional relational database engines) using any standard WWW
   browser.

   @section{Using file-based persistent predicates}

   Persistent predicates can be declared statically, using
   @decl{persistent/2} declarations (which is the preferred method,
   when possible), or dynamically via calls to
   @pred{make_persistent/2}.  Currently, persistent predicates may
   only contain facts, i.e., they are @em{dynamic} predicates of type
   @pred{data/1}, and @em{should be declared as such}.

   Predicates declared as persistent are linked to a directory, and
   the persistent state of the predicate will be kept in several files
   in that directory.  The files in which the persistent predicates
   are stored are in readable, plain ASCII format, and in Prolog
   syntax. One advantage of this approach is that such files can also
   be created or edited by hand, in a text editor, or even by other
   applications.

   An example definition of a persistent predicate implemented by files
   follows:

@begin{verbatim}
:- persistent(p/3,dbdir).

:- multifile persistent_dir/2.
:- data persistent_dir/2.

persistent_dir(dbdir, '/home/clip/public_html/db').
@end{verbatim}

   The first line declares the predicate @tt{p/3} persistent.  The
   argument @tt{dbdir} is a key used to index into a fact of the
   relation @pred{persistent_dir/2}, which specifies the directory
   where the corresponding files will be kept.  The effect of the
   declaration, together with the @pred{persistent_dir/2} fact, is
   that, although the predicate is handled in the same way as a normal
   data predicate, in addition the system will create and maintain
   efficiently a persistent version of @tt{p/3} via files in the
   directory @tt{/home/clip/public_html/db}.

   The level of indirection provided by the @tt{dbdir} argument makes
   it easy to place the storage of several persistent predicates in a
   common directory, by specifying the same key for all of them.  It
   also allows changing the directory for several such persistent
   predicates by modifying only one fact in the program. Furthermore,
   the @pred{persistent_dir/2} predicate can even be dynamic and
   specified at run-time.

   @section{Implementation Issues}

   We outline the current implementation approach.  This
   implementation attempts to provide at the same time efficiency and
   security. To this end, up to three files are used for each
   predicate (the @concept{persistence set}): the @concept{data file},
   the @concept{operations file}, and the @concept{backup file}. In
   the @concept{updated state} the facts (tuples) that define the
   predicate are stored in the data file and the operations file is
   empty (the backup file, which contains a security copy of the data
   file, may or may not exist).

   While a program using a persistent predicate is running, any
   insertion (assert) or deletion (retract) operations on the
   predicate are performed on both the program memory and on the
   persistence set. However, in order to incurr only a small overhead
   in the execution, rather than changing the data file directly, a
   record of each of the insertion and deletion operations is
   @em{appended} to the operations file. The predicate is then in a
   @concept{transient state}, in that the contents of the data file do
   not reflect exactly the current state of the corresponding
   predicate. However, the complete persistence set does.

   When a program starts, when it is halted, or, periodically, when it
   is idle for some time, all pending operations in the operations
   file are performed on the data file. A backup of the data file is
   created first to prevent data loss if the system crashes during
   this operation.  The order in which this updating of files is done
   ensures that, if at any point the process dies, on restart data
   will be completely recovered. This process of updating the
   persistence set can also be triggered at any point in the execution
   of a program by calling @pred{update_files/2}.

   @section{Using persistent predicates from the top level}

   Special care must be taken when using persistent predicates from
   the top level. This includes not only defining persistent
   predicates on the fly from de top level (which is not really very
   useful in practice) but also the more frequent case of loading into
   the top level modules or user files which use persistent
   predicates. As mentioned before, the persistence set is updated
   automatically each time a program using the corresponding
   persistent predicates is run or halted. However, since the top
   level itself is also a standard program, persistent predicates
   would only be updated whenever the top level is started, when they
   are typically still not loaded. 

   If a program launched from a top level needs to update the
   persistence sets of any persistent predicate it must be done by
   calling the @pred{update_files/2} method explicitly.

   @comment{In fact, this is the most logical way to do it because as
   long as there is a top level holding the state of the persistent
   predicates, there is no need to waste machine resources making
   updates.}

").

:- comment(usage, "Typically, this library is used including the
   'persdb' package into the syntax list of the module, or using the
   @decl{syntax/1} declaration:
@begin{description}
@item{In a module:}
@begin{verbatim}
        :- module(bar, [main/1], [persdb]).
@end{verbatim}
        or
@begin{verbatim}
        :- module(bar, [main/1]).
        :- include(library(persdb)).
@end{verbatim}
@item{In a @em{user} file:}
@begin{verbatim}
        :- syntax([persdb]).
@end{verbatim}
        or
@begin{verbatim}
        :- include(library(persdb)).
@end{verbatim}
@end{description}
   This syntax file loads the run-time and compile-time versions 
   of the library (@tt{persdbtr.pl} and @tt{persdbrt.pl}) and includes some
   needed declarations.").

:- decl persistent(PredDesc,Keyword) => predname * keyword

# "Declares the predicate @var{PredDesc} as persistent. @var{Keyword}
   is the @concept{identifier of a location} where the persistent
   storage for the predicate is kept. In this case it will be the
   persistence set, located in a directory, which must exist. The
   location @var{Keyword} is described in the @pred{persistent_dir}
   predicate, which must contain a fact in which the first argument
   unifies with @var{Keyword}.".

%% This declaration is expanded in persdbtr 
:- data persistent/5. % F/A (modulo expanded) is persistent and uses files
                      % FILE_OPS, FILE and FILE_BAK

:- pred persistent_dir(Keyword,Location_Path) :  keyword * directoryname

# "Relates identifiers of locations (the @var{Keyword}s) with
   descriptions of such locations (@var{Location_Path}s).
   @var{Location_Path} is @bf{a directory} and it means that the
   definition for the persistent predicates associated with
   @var{Keyword} is kept in files in that directory. These files, in
   the updated state, contain the actual definition of the predicate
   in Prolog syntax (but with module names resolved).".

   %% Note: it has to be declared as multifile and data

:- multifile persistent_dir/2.
:- data persistent_dir/2.

:- meta_predicate passertz_fact(fact).

% passertz_fact(P, D) asserts a predicate in both the dynamic and the
% persistent databases.  
:- pred passertz_fact(Fact) : callable
# "Persistent version of @pred{assertz_fact/1}: the current instance
   of @var{Fact} is interpreted as a fact (i.e., a relation tuple) and
   is added at the end of the definition of the corresponding
   predicate.  The predicate concerned must be declared
   @decl{persistent}.  Any uninstantiated variables in the @var{Fact}
   will be replaced by new, private variables.".

passertz_fact(MPred):-
        init_persdb,
        term_to_meta(Pred, MPred),
        functor(Pred, F, N),
        current_fact(persistent(F, N, File_ops, _, File_bak)), !,
        delete_bak_if_no_ops(File_ops, File_bak),
        add_term_to_file(a(Pred), File_ops),
        assertz_fact(MPred).
passertz_fact(MPred):-
        term_to_meta(Pred, MPred),
        throw(error(type_error(persistent_data,Pred), passertz_fact/2-1)).

:- pred pretract_fact(Fact) : callable
# "Persistent version of @pred{retract_fact/1}: deletes on
   backtracking all the facts which unify with @var{Fact}.  The
   predicate concerned must be declared @decl{persistent}.".

:- meta_predicate pretract_fact(fact).

% pretract_fact(P) retracts a predicate in both, the dynamic and the 
% persistent databases.
pretract_fact(MPred):-
        init_persdb,
        term_to_meta(Pred, MPred),
        functor(Pred, F, N),
        current_fact(persistent(F, N, File_ops, _, File_bak)), !,
        retract_fact(MPred),
        delete_bak_if_no_ops(File_ops, File_bak),
        add_term_to_file(r(Pred), File_ops).
pretract_fact(MPred):-
        term_to_meta(Pred, MPred),
        throw(error(type_error(persistent_data,Pred), pretract_fact/2-1)).

:- pred pcurrent_fact(Fact) : callable
# "Persistent version of @pred{current_fact/1}: the fact @var{Fact}
   exists in the current database.  The predicate concerned must be
   declared @decl{persistent}.  Provides on backtracking all the facts
   (tuples) which unify with @var{Fact}.".

:- meta_predicate pcurrent_fact(fact).

pcurrent_fact(Mpred) :-
        init_persdb,
        current_fact(Mpred).

:- data db_initialized/0.

:- pred init_persdb # "Executes @pred{initialize_db/0} if no
   initialization has been done yet.  Needed when accesing persistent
   predicates before doing any of @pred{passertz_fact/1},
   @pred{pretract_fact/1}, or @pred{pcurrent_fact/1}.".

init_persdb :-
        current_fact(db_initialized), !.
init_persdb :-
        initialize_db,
        assertz_fact(db_initialized).

:- multifile '$is_persistent'/2.

:- pred initialize_db # "@cindex{database initialization} Initializes
   the whole database, updating the state of the declared persistent
   predicates.".

initialize_db :-
        '$is_persistent'(Spec, Key),
        make_persistent(Spec, Key),
        fail.
initialize_db.

:- pred make_persistent(PredDesc,Keyword) : predname * keyword
# "Dynamic version of the @decl{persistent} declaration.".

:- meta_predicate make_persistent(spec, ?).

make_persistent(Spec, Key) :-
        term_to_meta(F/A, Spec),
        persistent_dir(Key, Dir),
        add_final_slash(Dir, DIR),
        get_pred_files(DIR, F, A, File, File_ops, File_bak),
        assertz_fact(persistent(F, A, File_ops, File, File_bak)),
        ini_persistent(File_ops, File, File_bak).

ini_persistent(File_ops, File, File_bak):- 
        lock_file(File, Fd1, _),        
        lock_file(File_ops, Fd2, _),    
        lock_file(File_bak, Fd3, _),
        ( file_exists(File) ->
            ( file_exists(File_bak) ->
                ( file_exists(File_ops) ->  % Operations maybe not concluded
                    delete_file1(File),
                    mv(File_bak, File),
                    secure_update(File, File_ops, File_bak, NewTerms)
                ; delete_file1(File_bak), % operations done
                  file_to_term_list(File, NewTerms, [])
                )
            ; secure_update(File, File_ops, File_bak, NewTerms)
            )
        ; mv(File_bak, File), % System crash
          secure_update(File, File_ops, File_bak, NewTerms)
        ),
        unlock_file(Fd1, _),
        unlock_file(Fd2, _),
        unlock_file(Fd3, _),
        process_terms(NewTerms).

% Steps in the process of updating:
%  f  o
%     o  b
%  f+ o  b
%  f+    b
%  f+

secure_update(File, File_ops, File_bak, NewTerms):-
        file_to_term_list(File, Terms, Terms_),
        ( file_exists(File_ops) ->
            file_to_term_list(File_ops, Lops, []),
            make_operations(Lops, Terms, Terms_, NewTerms),
            mv(File, File_bak),
            term_list_to_file(NewTerms, File),
            delete_file1(File_ops),
            delete_file1(File_bak)
        ; Terms_ = [],
          NewTerms = Terms
        ).

make_operations([], Terms, [], Terms).
make_operations([Operation|Operations], Terms, Terms_, NewTerms):-
        make_operation(Operation, Terms, Terms_, NTerms, NTerms_),
        make_operations(Operations, NTerms, NTerms_, NewTerms).

% Adds a fact to the tail of the list
make_operation(a(Pred), Terms, [Pred|Terms_], Terms, Terms_).
% Removes the first fact which unify (exists)
make_operation(r(Pred), Terms, Terms_, NTerms, Terms_) :-
        select(Pred, Terms, NTerms), !.

% process_terms(Facts) asserts the data contained in Facts into the
%  dynamic database

process_terms([]).
process_terms([Fact|Facts]) :-
        term_to_meta(Fact, MFact),
        assertz_fact(MFact),
        process_terms(Facts).

:- pred update_files(PredDesc, Arity) => predname * int
# "Updates the state of the given persistent predicate and its
   corresponding persistence set.".

update_files(Pred, Arity) :-
        current_fact(persistent(Pred, Arity, File_ops, File, File_bak)),
        lock_file(File, Fd1, _),        
        lock_file(File_ops, Fd2, _),    
        lock_file(File_bak, Fd3, _),
        ( file_exists(File) ->
            ( file_exists(File_bak) ->
                ( file_exists(File_ops) ->  % Operations maybe not concluded
                    delete_file1(File),
                    mv(File_bak, File),
                    secure_update(File, File_ops, File_bak, _)
                ; delete_file1(File_bak) % operations done
                )
            ; secure_update(File, File_ops, File_bak, _)
            )
        ; mv(File_bak, File), % System crash
          secure_update(File, File_ops, File_bak, _)
        ),
        unlock_file(Fd1, _),
        unlock_file(Fd2, _),
        unlock_file(Fd3, _),
        fail.
update_files(_, _).
       

% file_to_term_list(File, Terms, Terms_) reads a list of terms Terms from a
%  file File, Terms_ is the tail of the list
file_to_term_list(File, Terms, Terms_) :-
        file_exists(File, 6), !,
        current_input(OldInput),
        open(File, read, Stream),
        set_input(Stream),
        read(T),
        read_terms(T, Terms, Terms_),
        set_input(OldInput),
        close(Stream).
file_to_term_list(_File, Terms, Terms).

read_terms(end_of_file, Ts, Ts) :- !.
read_terms(T, [T|Ts], Ts_) :-
        read(T1),
        read_terms(T1, Ts, Ts_).

% term_list_to_file(Terms, File) writes a list of terms Terms onto a file
%  File
term_list_to_file(Terms, File) :-
        current_output(OldOutput),
        open(File, write, Stream),
        set_output(Stream),
        display_term_list(Terms),
        close(Stream),
        set_output(OldOutput).    

display_term_list([]).
display_term_list([T|Ts]) :-
        display_term(T),
        display_term_list(Ts).

% This ensure that we not create an operations file in a transient state
delete_bak_if_no_ops(File_ops, File_bak) :-
        ( file_exists(File_ops) -> true
	; file_exists(File_bak) -> delete_file1(File_bak)
	; true).

% add_term_to_file(Term,File) adds the term Term to a file File
add_term_to_file(Term,File) :-
        current_output(OldOutput),
        lock_file(File, FD, _),
        open(File,append,Stream),
        set_output(Stream),
        display_term(Term),
        close(Stream),
        unlock_file(FD, _),
        set_output(OldOutput).        

get_pred_files(Dir, Name, Arity, File, File_ops, File_bak):-
        number_codes(Arity, AS),
        atom_codes(A, [0'-|AS]),
        atom_concat(Name, A, NameA),
        atom_concat(Dir, NameA, PathName),
        atom_concat(PathName, '.pl', File),
        atom_concat(PathName, '_ops.pl', File_ops),
        atom_concat(PathName, '_bak.pl', File_bak).

delete_file1(File):-
	(file_exists(File)->
	 delete_file(File)
	;
	 true).

% :- pred mv(Path1, Path2) ; "Rename a file, or create target.".
mv(Source, Target):-
        file_exists(Source), !,
        atom_codes(Source, Lso),
        atom_codes(Target, Lta),
        append("/bin/mv "||Lso, " "||Lta, Lcommand),
        atom_codes(Command, Lcommand),
        system(Command).
mv(_Source, Target):-
        create(Target).


% :- pred create(Path) ; "Creates a file.".
create(Path):-
        umask(OldUmask, 0),
        open(Path, write, S),
        close(S),
        umask(_, OldUmask).

add_final_slash(Dir, DIR) :-
        atom_concat(_,'/',Dir) -> DIR = Dir ; atom_concat(Dir, '/', DIR).

:- comment(doinclude, keyword/1).

:- comment(keyword/1,"An atom which identifies a fact of the
   @pred{persistent_dir/2} relation. This fact relates this atom to a
   directory in which the persistent storage for one or more
   persistent predicates is kept.").

:- prop keyword(X) + regtype 
# "@var{X} is an atom corresponding to a directory identifier.".

keyword(X) :- atm(X).

:- comment(doinclude, directoryname/1).

:- prop directoryname(X) + regtype 
# "@var{X} is an atom which is the name of a directory.".

directoryname(X) :- atm(X).

%% ---------------------------------------------------------------------------
:- comment(version_maintenance,dir('../../version')).

:- comment(version(0*8+31,1998/12/27,19:17*20+'MET'), "Localized most
   of the documentation in this file. Updated documentation.  (Manuel
   Hermenegildo)").
%% ---------------------------------------------------------------------------

 
