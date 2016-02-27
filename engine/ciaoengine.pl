%% Since Ciao Version 0.8, changes are unified with the GlobalChangeLog 
%% in $(SRC)/version.

:- use_package(assertions).

:- comment(title,"The Ciao engine"). 

:- comment(module,"

   Currently, this contains the ChangeLog for the Ciao engine. Since
   Ciao Version 0.8, changes to the engine are unified with the
   GlobalChangeLog in @tt{$(SRC)/version}.  Previous changes do not
   have a version explicitly associated with it.  Eventually, this
   should contain a description of the engine, as a chapter of the
   Reference Manual.

   ").

%% This is so that lpdoc documents this as an application instead 
%% of as a library:
main.

% ---------------------------------------------------------------------------
:- comment(version_maintenance,dir('../version')).

:- comment(version(1*9+343,2004/04/29,11:55*29+'CEST'),
"Wait_for_Cond_Begin surrounded by brackets in several places (Manuel Carro)").

:- comment(version(1*9+330,2004/03/25,16:54*55+'CET'), "Solved some
problems in shell/n and exec/3,4} in windows.  shell/n now call
directly the command in the SHELL environment variable using
execlp. If $SHELL is does not exist (usual in Windows), one is created
at startup filling it in with the sh.exe provided with Ciao.  This is
potentially wrong, as the user may have given a value to SHELL withing
Windows, and the calling procedure may be different from that of
Unix/Cygwin (i.e., /C instead of -c) (MCL)").

:- comment(version(1*9+327,2004/03/24,16:39*18+'CET'), "A complaint
message is returned if there is no SHELL environment variable (no
exceptions are thrown because 1.9 did not have all the ISO exceptions,
which were introduced in 1.11) (MCL)").

:- comment(version(1*9+324,2004/03/10,01:40*39+'CET'), "Changed
   float_*.c /.h and term_support.c, to improve the conversion between
   float numbers and strings.  Now the exponent symbol is e for bases
   less than 10, p for bases less or equals than 25 (25=p), and _ for
   bases less or equals than 36.  (Edison Mera)").

:- comment(version(1*9+319,2004/02/26,23:59*45+'CET'), "Solved a bug
   that formatted incorrectly numbers nears to 1, before this patch
   format(\"~1f~N\",0.99). showed 010. (Edison Mera)").

:- comment(version(1*9+301,2004/02/16,17:26*09+'CET'), "In
   engine/unix_utils.c, changed prolog_c_del_env to be compatible with
   Solaris. (Edison Mera)").

:- comment(version(1*9+298,2004/02/16,17:13*26+'CET'), "number_codes/3
   now is reversible, also for floating point numbers in other bases
   than 10.  (Edison Mera)").

:- comment(version(1*9+296,2004/02/16,17:04*20+'CET'), "Now
   number_codes/3 is defined in the engine.  These change also implies
   modifications to number_codes/2.  (Edison Mera)").

:- comment(version(1*9+295,2004/02/16,17:01*45+'CET'), "In
   float_const.c, float_const.h. Improved powl_int to use directly the
   power tables.  Added fillchardigit, and char_digit, required to
   convert a char in its value. (Edison Mera)").

:- comment(version(1*9+294,2004/02/16,16:57*21+'CET'), "In
   engine/bignum.c, engine/bignum_defs.h, engine/ciao_prolog.c,
   engine/inout.c, engine/qget.c, engine/qinsert.c. Changed
   bn_from_string to support different numeric bases
   correctly. (Edison Mera)").

:- comment(version(1*9+293,2004/02/16,16:55*37+'CET'), "Added
   float_tostr.c .h and float_const.c .h to solve some bugs when
   printing floating point numbers (correction ported from ciao-1.11),
   and changed related files: Makefile, format.c, term_support.c.
   (Edison Mera)").

:- comment(version(1*9+284,2004/02/13,17:32*26+'CET'), "Changed handling
   of end-of-lines to understand unix, mac and win styles.  Added
   support for skip_line/[0,1]. (Daniel Cabeza Gras)").

:- comment(version(1*9+274,2004/01/09,16:01*17+'CET'), "Runtime
   reported by statistics/0 now matches that returned by
   statistics(runtime, _).  (Manuel Carro)").

:- comment(version(1*9+244,2003/12/22,23:35*15+'CET'), "Fixed a nasty
   C gluecode bug that inserted stack references into the heap (Jose
   Morales)").

:- comment(version(1*9+83,2003/06/05,01:13*41+'CEST'), "Fixed a trail
   overflow bug. Added upper bound checks in memory allocation
   routines to ensure that (re)allocated blocks fit in addressable
   memory. Fixed a bug that made the engine forget the wam pointer
   after wam() execution when the wam structure was reallocated. Search
   'segfault patch -- jf' comments to see the modified code.  (Jose
   Morales)").

:- comment(version(1*9+64,2003/03/06,12:52*22+'CET'), "Updated
   makefile-sysdep to compile again on Darwin (some options, yet
   unsupported by the Darwin C Compiler, sneaked in).  (MCL)").

:- comment(version(1*9+62,2003/02/26,19:12*32+'CET'), "Solved a
   segmentation violation problem which appeared when backtracking
   over concurrent predicates.  The solution was substituting

   TopConcChpt = (struct node *)X(PrevDynChpt);

   by

   TopConcChpt = (struct node *)TermToPointerOrNull(X(PrevDynChpt));

   in shdisp_r.c (the pointer was encoded when storing it to the
   choicepoint stack and it was not being uncoded when reading it
   back). (MCL)").

:- comment(version(1*9+50,2003/01/09,17:47*46+'CET'), "Changed
   configure.c to correctly compute the number of significant decimals
   that can be printed.  So now 5.347 is displayed as-is, and X = 1/3
   has all decimals as 3. (Daniel Cabeza Gras)").

:- comment(version(1*9+49,2003/01/09,14:21*59+'CET'), "Added
   ciao_free() and ciao_malloc(), now only interfaces to free and
   malloc (MCL)").

:- comment(version(1*9+48,2003/01/07,14:27*09+'CET'), "Added support
   to test conversion of a Ciao integer into a machine int, and to
   make numeric conversions through character strings.

Files changed:

term_support_defs.h
term_support.c
configure.c
ciao_prolog.h
ciao_prolog.c
bignum_defs.h
bignum.c

Public functions added (plus their state aware counterparts):

ciao_bool ciao_fits_in_int(ciao_term term);
ciao_bool ciao_to_integer_check(ciao_term term, int *res);
char *ciao_get_number_chars(ciao_term term);
ciao_term ciao_put_number_chars(char *number_string);

    (MCL)").

:- comment(version(1*9+45,2003/01/07,13:01*02+'CET'), "Changed
   optimization level from -O3 to -O2 (-O3 gave, in general, worst
   results) (MCL)").

:- comment(version(1*9+41,2002/12/12,21:51*40+'CET'), "Unbound length
   atoms can now appear in source (and in .po) files; also, checks for
   expansion of atom lengths made more uniform.  (MCL)").

:- comment(version(1*9+23,2002/11/18,14:28*24+'CET'), "Added #ifdef's
   suggested by Roberto Bagnara to the ciao_prolog.h file.  (MCL)").

:- comment(version(1*9+8,2002/05/27,16:57*51+'CEST'), "Added entries
   in makefile-sysdep to deal with gcc 3.1 different command line
   options (some -m to -f).  (MCL)").

:- comment(version(1*7+207,2002/04/23,18:58*09+'CEST'), "Makefiles
   changed to be more resilient to errors.  (MCL)").

:- comment(version(1*7+193,2002/03/25,11:51*37+'CET'), "Generalized
   the cache for "last_inserted_clause" (look for
   CACHE_INCREMENTAL_CLAUSE_INSERTION); just small improvements.
   Could not make the same thing with the try_node chain in
   incore_insert.  (MCL)").

:- comment(version(1*7+161,2001/11/27,12:53*26+'CET'), "Removed some
   warnings.  (MCL)").

:- comment(version(1*7+151,2001/11/23,15:08*49+'CET'), "Added new
   option -fno-strict-aliasing to the C command line options in order
   to work around (wrong?) optimizations and inconvenient ANSI casting
   conventions.  The Ciao engine now seems to compile correctly with
   the later gcc suite. (MCL)").

:- comment(version(1*7+150,2001/11/23,14:57*54+'CET'), "Added patch
   for Mac Os X developer tools 10.1 (thanks to @index{Kris Gybels})
   (MCL)").

:- comment(version(1*7+132,2001/10/31,10:14*18+'CET'), "Added
   initialization of SHELL environment variable under Windows when it
   is not set.  (MCL)").

:- comment(version(1*7+115,2001/07/09,19:10*00+'CEST'), "Solved a
   problem with '$/' in Windows.  (Daniel Cabeza Gras)").

:- comment(version(1*7+113,2001/07/05,17:10*06+'CEST'), "exec/3 was
   giving segmentation fault due to a unitialized variable.  Solved.
   (MCL)").

:- comment(version(1*7+68,2001/03/22,17:28*29+'CET'), "Solved a bug
   (which only showed up in Mac OS X): in the wam() label "fail:" four
   X regs were popped be it necessary or not (in case less registers
   were needed, the WAM just discarded those recovered).  This caused
   the first choicepoint of a concurrent goal to be accessed out of
   the bounds of the allocated memory in case of failure of the
   computation, and a OS signal was raised.  I have conducted some
   experiments, and the original code did not seem to bring any
   advantage in terms of execution time, so I have simplified it (the
   original code is still in place and commented).  (MCL)").

:- comment(version(1*7+67,2001/03/21,12:28*26+'CET'), "
   ThreadCreateNoGoalId not called anywhere --- removed.  (MCL)").

:- comment(version(1*7+65,2001/03/05,16:21*45+'CET'), "Added support
   for threads in DARWIN; some problems still pending, but most of it
   works.  (MCL)").

:- comment(version(1*7+60,2001/02/12,18:09*03+'CET'), "Added patches
   for a concurrency problem which appeared in nonindexed accesses to
   predicates: when post-indexing unification failed the lock was not
   unset, and backtracking to retry_instance blocked.  Solved by
   keeping a bit in the choicepoint reflecting whether the
   backtracking worker had locked the predicate, and unlocking before
   entering next_instance_conc.  (MCL)").

:- comment(version(1*7+53,2001/01/26,15:34*44+'CET'), "Changed name
   for alias redirection module .  (MCL)").

:- comment(version(1*7+48,2001/01/22,12:21*43+'CET'), "Added
   prolog_{replace,get}_stream(), aimed at replacing the file pointed
   to by the Prolog stream aliases.  (MCL)").

:- comment(version(1*7+46,2001/01/19,19:43*48+'CET'), "Fixed another bug on
   self-contained executables when called from certain web servers.  (Oscar
   Portela Arjona)").

:- comment(version(1*7+45,2001/01/18,16:38*51+'CET'), "Fixed small bug on
   self-contained executables when running without the PATH environment
   variable.  (Oscar Portela Arjona)").

:- comment(version(1*7+38,2001/01/05,18:30*05+'CET'), "Added Sparc64
   options to locks.h (MCL)").

:- comment(version(1*7+34,2000/12/23,16:42*52+'CET'), "Added patches
   to compile in Darwin (MacOS X). The new DARWINppc architecture is
   now recognized by ciao_get_arch and dynlink.c uses the
   mach-o/dydl.h interface whenever DARWIN is defined.  (Jose
   Morales)").

:- comment(version(1*7+29,2000/11/03,15:07*41+'CET'), "Added suport
   for Linux in Sparc.  (MCL)").

:- comment(version(1*7+28,2000/11/03,10:45*01+'CET'), "Improved
   behavior in non-blocking calls to concurrent facts, and solved a
   bug: a NB call which does not have matching fact at indexing level
   does not push a choicepoint.  (MCL)").

:- comment(version(1*7+27,2000/10/30,19:22*49+'CET'), "nonblocking
   calls to predicates were not protected by a lock.  Fixed.  (MCL)").

:- comment(version(1*7+26,2000/10/24,18:24*26+'CEST'), ""Fixed bug in
   fastwrite/fastread when writing/reading lists of small integers (between 1
   and 255).  (Oscar Portela Arjona)").

:- comment(version(1*7+19,2000/09/11,21:27*01+'CEST'), "Added several
   system predicates (rm,mv,mkdir,rmdir) to the Ciao Prolog libraries.
   Not removed from the 'make' library: will unify them.  (MCL)").

:- comment(version(1*7+13,2000/08/24,18:41*05+'CEST'), "Changed getc()
   in fastreading to readchar(); fastreading was not working with
   sockets!  (MCL)").

:- comment(version(1*7+2,2000/07/19,14:15*15+'CEST'), "Added patches
   to compile in Power PC contributed by Paulo Moura.  (MCL)").

:- comment(version(1*7+1,2000/07/18,15:47*04+'CEST'), "Some warnings fixed
   (Oscar Portela Arjona)").

:- comment(version(1*5+172,2000/07/12,16:45*31+'CEST'), "Partially
   solved undetected heap overflows in fast_read: no enough checks
   were made when constructing terms.  Workaround: ensuring enough
   heap space (16 kCels at the moment) before each call to fast_read.
   (MCL, DCG)").

:- comment(version(1*5+171,2000/07/07,16:10*17+'CEST'), "Solved a bug
   in calls to non-blocking failing concurrent predicates: if possibly
   matching clauses exist, the lock has to be released after the
   execution (otherwise the clause might be removed in the middle of
   an execution); this was being done by an $unlock_predicate/1 after
   $current_instance/5. But if the predicate fails,
   $unlock_predicate/1 is never reached.  It was solved by pushing a
   choicepoint even if the accessed clause is the last one, and, on
   redo, checking if the call is non-blocking before trying to wait on
   it.  next_instance_conc will leave a lock if there is a possibly
   matching clause (which will be released by $unlock_predicate/1) and
   remove it after the last clause is reached, for CONC_CLOSED and
   NON_BLOCKING calls.  (MCL)").

:- comment(version(1*5+164,2000/06/22,19:48*58+'CEST'), "Changed
   new_atom/1 to generate atoms having only alphanumeric characters.
   (MCL)").

:- comment(version(1*5+161,2000/06/01,20:23*24+'CEST'), "Fixed a bug
   which made that sometimes root directory was computed as the empty
   atom.  (Daniel Cabeza Gras)").

:- comment(version(1*5+160,2000/06/01,12:27*49+'CEST'), "Changed
   stat() in unix_utils.c to lstat() to gather information about
   links.  (MCL)").

:- comment(version(1*5+155,2000/05/29,19:34*30+'CEST'), "Changed xchgw
   to xchg in assembly code to avoid compiler complaints.  (MCL)").

:- comment(version(1*5+136,2000/05/09,14:16*01+'CEST'), "Access to
   clauses and facts is now (partially) protected.  Concurrent threads
   can now insert/retract them without colliding.  However, a
   fact/clause is not protected during its execution.  Anyway, it is
   not a good idea to retract a clause which may be being executed by
   another thread.  (MCL)").

:- comment(version(1*5+135,2000/05/09,12:49*25+'CEST'), "The lock for
   insert_definition is inside the function itself.  (MCL)").

:- comment(version(1*5+133,2000/05/05,17:32*14+'CEST'),
   "SignalObjectAndWait in Win32 (the equivalent to the POSIX
   pthread_cond_wait) exists only in NT 4.0 and Windows 2000; I am
   substituting it for WaitForSingleObject with a timeout.  (MCL)").

:- comment(version(1*5+132,2000/05/05,17:29*54+'CEST'), "Threads in
   Win32 up and running, with native locks, kernel32 critical
   sections, and wait on events to make threads sleep when waiting for
   a fact to appear.  (MCL)").

:- comment(version(1*5+131,2000/05/04,19:46*36+'CEST'), "Fixed a bug
   related to attributed variables, by changing a
   collect_goals_from_trail() call to collect_pending_unifications() in
   wam.c. (Daniel Cabeza Gras)").

:- comment(version(1*5+129,2000/05/02,20:46*52+'CEST'), "relBuf in
   prolog_find_file() is now explicitly allocated, since MAXATOM is a
   variable and some compilers do not know how to translate a variable
   array length directly.  (MCL)").

:- comment(version(1*5+128,2000/05/02,18:04*14+'CEST'), "Changed the
   implementation of general semaphores, using posix locks.  Much
   simpler and more efficient now.  (MCL)").

:- comment(version(1*5+126,2000/05/02,16:47*23+'CEST'), "Corrected
   non-initialized variable in eng_killothers() (MCL)").

:- comment(version(1*5+125,2000/04/28,21:37*08+'CEST'), "close/1
   checks for previous choicepoints which are enumerating the current
   open streams and substitutes the closed stream by the next one in
   the stream chain.  But this causes close/1 not to be O(1), and to
   run slower and slower in unexpected cases (e.g., in a TCP/IP
   client), where (apparently) no choicepoints were being pushed (but
   they actually were).  (MCL)").

:- comment(version(1*5+124,2000/04/28,21:16*05+'CEST'), "Many Prolog
   builtins call catch; it is not concurrent, so no concurrent
   applications should call them.  Making the data fact used by it
   concurrent will, for the moment, solve this problem.  (MCL)").

:- comment(version(1*5+123,2000/04/28,17:24*44+'CEST'), "Added
   processor relinquishing; it gives much better and stable
   performance in a monoprocessor execution.  It seems to overload
   execution in a Linux 2 processor box!  (MCL)").

:- comment(version(1*5+122,2000/04/28,17:22*01+'CEST'), "All the
   Signal_Cond_End replaced by Broadcast_Cond_End: this is the right
   thing to do, since all threads waiting on a fact should be awoken
   and given the opportunity of inspecting the newly added
   facts. (MCL)").

:- comment(version(1*5+121,2000/04/28,14:25*20+'CEST'), "Added macro
   for process relinquishing.  (MCL)").

:- comment(version(1*5+120,2000/04/28,14:24*59+'CEST'), "Added macro
   for condition broadcasting.  (MCL)").

:- comment(version(1*5+119,2000/04/28,14:23*49+'CEST'), "Solved a bug
   with concurrent facts wich appeared explicitly in the source file:
   their pending_x? fields were not being initialized in
   compile_term_aux().  (MCL)").

:- comment(version(1*5+112,2000/04/07,17:44*20+'CEST'), "Added code
   for library-based locking in Windows -- not tested.  (MCL)").

:- comment(version(1*5+111,2000/04/07,17:43*52+'CEST'), "Code for
   locking (in locks.h) cleaned up.  (MCL)").

:- comment(version(1*5+98,2000/03/30,14:58*35+'CEST'), "Removed some
   old dependencies in the Makefile, renamed some internal C fuctions
   to match the Prolog predicates.  (MCL)").

:- comment(version(1*5+76,2000/03/21,12:06*21+'CET'), "Changed thread
   urgent termination: killing a goal now sets an internal event on
   the wam executing it.  wam() checks it and exits.  After some time,
   the thread is checked, and explicitly cancelled if it has not
   exited.  (MCL)").

:- comment(version(1*5+75,2000/03/21,12:06*05+'CET'), "Added support
   for threads in Windows 32, by using the native Windows thread
   management primitives.  Internal code for threads totally
   rewritten.  (MCL)").

:- comment(version(1*5+60,2000/03/08,17:36*34+'CET'), "Changed
   prolog_find_file() so that _opt filenames are not given if older than
   regular ones (Daniel Cabeza Gras)").

:- comment(version(1*5+52,2000/02/10,20:08*28+'CET'), "Improved
   behavior of catch & throw under choicepoint reallocation.  This
   solved a bug when aborting in the middle of a debugging session
   (USE_TAGGED_CHOICE_START). May affect the time behavior of the cut?
   (MCL)").

:- comment(version(1*5+46,2000/02/07,11:05*53+'CET'), "Load of compressed
   bytecode performance improved: load time becomes about 125% (with
   buff. input) and less than 60% (without it).  (Oscar Portela Arjona)").

:- comment(version(1*5+39,2000/02/02,13:17*35+'CET'), "Added support
   for loading compressed bytecode from .po files. Preliminary tests
   show a compression ratio of 1:3 while the load time become about
   150% if using buffered input or about 70% else (note: however, the
   load of compressed bytecode is faster when using buffered input)
   (OPA).  (Oscar Portela Arjona)").

:- comment(version(1*5+37,2000/01/26,13:12*16+'CET'), "Atom length
   added to the atom structure, and used in all the code.  Minor
   speedups in regular applications.  Look for USE_ATOM_LEN. (MCL)").

:- comment(version(1*5+36,2000/01/25,16:37*41+'CET'),
   "new_atom_check() updated to be smarter when reclaiming more atom
   space.  (MCL)").

:- comment(version(1*5+35,2000/01/25,14:16*47+'CET'), "Size of
   user-created atoms is now variable and unbound (look for the macro
   USE_DYNAMIC_ATOM_SIZE).  Some atoms created inside the engine still
   have a fixed, maximum length (STATICMAXATOM).  (MCL)").

:- comment(version(1*5+33,2000/01/03,17:08*17+'MET'), "Changed thread
   primitives to use pthread_equal() instead of == (MCL)").

:- comment(version(1*5+32,2000/01/03,13:37*19+'MET'),
   "unload_if_present() was giving a segmentation violation when
   loading .so files; a wrong variable name was the culprit. Solved.
   (MCL)").

:- comment(version(1*5+31,1999/12/29,15:44*18+'CET'), "Added a
   heuristic to the loading of incore clauses which avoids traversing
   a list of choices: the last try chain, the number of node inserted,
   and the point of insertion is cached, and used later if possible
   (look for CACHE_INCREMENTAL_CLAUSE_INSERTION); this speeds up
   things quite a lot. Unfortunately, the whole method is still
   O(N^2). Looking into that.  (MCL)").

:- comment(version(1*5+30,1999/12/29,15:41*00+'CET'), "The engine now
   accepts a lot of clauses per predicate (as many as the maximum
   value of an unsigned long int).  Unfortunately inserting N compiled
   clauses is still O(N^2).  (MCL)").

:- comment(version(1*5+29,1999/12/29,15:40*48+'CET'), "Use an internal
   buffer of QLBFSIZE chars to store the contents of the .po files
   being read in.  When the buffer is full, we fill it again at once;
   the previous method was calling getc() once and again.  Preliminary
   tests show this method to be between 3 times (for dynamic
   executables, as ciaosh) to 5 times (for static stuff, as ciaoc)
   faster. Look for #defined BUFFERED_PO (MCL)").

:- comment(version(1*5+28,1999/12/29,15:40*30+'CET'),
   "prolog_new_atom() redesigned to use an atom size and a
   quasi-linear congruential method which behave well with the hash
   function in the atom table routines.  (MCL)").

:- comment(version(1*5+22,1999/12/17,16:59*51+'MET'), "Solved yet
   another problem with remote backtrackig (finally, it seems to
   work); additionally, stacks were not being reused in the case of
   not compiling with threads!  (MCL)").

:- comment(version(1*5+20,1999/12/16,18:23*29+'MET'), "Ciao was
   breaking in Windows after long queries, due to the lack of
   defined(THREADS) in critical parts of the code.  Seems to be solved
   now.  (MCL)").

:- comment(version(1*5+6,1999/12/06,14:50*37+'MET'), "'behavior' added
   to the Behavior field of 'int_info'.  (MCL)").

:- comment(version(1*5+5,1999/12/06,14:47*35+'MET'), "Goal# could not
   be deduced from wam; corrected.  (MCL)").

:- comment(version(1*3+111,1999/11/22,13:00*41+'MET'),
   "backtrack_goal/1 now seems to work correctly.  Changing the low
   level primitives, anyway.  (MCL)").

:- comment(version(1*3+93,1999/11/07,19:05*33+'MET'), "Added a line in
   init_interpreted() to make interpreted predicates not concurrent by
   default; set_property() may change this behavior later.  (MCL)").

:- comment(version(1*3+86,1999/10/21,19:11*21+'MEST'), "Added in
   unix_utils.c the special prefix '$' for filenames which represents
   the Ciao library directory (same output by ciaolibdir/1).  (Daniel
   Cabeza Gras)").

:- comment(version(1*3+74,1999/10/07,10:58*02+'MEST'), "Removed
   warnings in the compilation of qinsert.c and qget.c.  (MCL)").

:- comment(version(1*3+61,1999/09/27,16:09*04+'MEST'), "Number of
   existent predicates is now counted more properly (look for
   'num_of_predicates').  (MCL)").

:- comment(version(1*3+47,1999/08/18,11:34*44+'MEST'), "Changed
   RTLD_NOW to RTLD_LAZY to avoid link-time error messages when
   loading .so files which depend on one another.  (MCL)").

:- comment(version(1*3+46,1999/08/07,13:44*00+'MEST'), "Solved some
   configuration problems in IRIX.  (MCL)").

:- comment(version(1*3+40,1999/07/25,19:41*01+'MEST'), "Changes mainly
   in Win32, such that the ciao library directory is located using the
   path to the ciaoengine, not the environment variable CIAOROOT.  Also,
   if no HOME env. var. is found, directory ~/ is the ciao library
   directory (Daniel Cabeza Gras)").

:- comment(version(1*3+25,1999/07/08,17:33*53+'MEST'), "NT changed to
   Win32 (Daniel Cabeza Gras)").

:- comment(version(1*3+24,1999/07/07,20:17*10+'MEST'), "CIAOLIB and
   CIAOROOT are now recognized by the engine in order to look for the
   libraries.  (MCL & DCG)").

:- comment(version(1*3+12,1999/07/02,17:39*30+'MEST'), "Culprit and
   ErrArgNo now private per thread.  (MCL)").

:- comment(version(1*3+9,1999/07/01,16:57*12+'MEST'),
   "reclassify_atoms commented out; it was not used anywhere.
   (MCL)").

:- comment(version(1*1+5,1999/06/10,18:20*45+'MEST'), "Fixed bug in
   prolog_file_properties regarding accepting an instantiated
   modification time (Daniel Cabeza Gras)").

:- comment(version(1*1+4,1999/06/10,17:52*01+'MEST'), "changed
   abolish() to return TRUE in the case of an undefined predicate.
   (MCL)").

:- comment(version(1*1+3,1999/06/09,16:59*32+'MEST'), "Memory
   accounting totally revamped.  Added a count of the number of
   symbols and definition of predicates.  (MCL)").

:- comment(version(0*9+102,1999/05/27,19:29*59+'MEST'), "Improved
   account of memory used in database (dynamic+compiled predicates),
   including indexing and concurency data structures.  Explicit
   accounting is made upon every [de]allocation of program
   memory. (MCL)").

:- comment(version(0*9+101,1999/05/26,13:33*03+'MEST'), "Changed some
   compilation flags and options: THREADS implies USE_LOCKS, so the
   latter has disappeared.  FOREIGN_FILES is disabled but in Sun4 (it
   will be soon replaced by the embedded glue code generator).
   O.S. and architecture naming convention has changed to be more
   uniform (although more verbose).  makfile-sysdep files have changed
   name to follow the new naming scheme of ciao_get_arch.  (MCL)").

:- comment(version(0*9+97,1999/05/21,20:37*22+'MEST'), "Changes mainly
   to the way filenames are processed in Win32 (but also affect UNIX).
   (Daniel Cabeza Gras)").

:- comment(version(0*9+72,1999/04/30,11:53*40+'MEST'), "added
   get_os/1.  (MCL)").

:- comment(version(0*9+69,1999/04/29,15:50*55+'MEST'), "Changed some
   #defines to compile without warning on several architectures
   (unix_utils.c) (MCL).").

:- comment(version(0*9+29,1999/03/30,19:37*24+'MEST'), "Added
   'volatile' to lock counter on atoms; was needed by Sparc
   architectures.  (MCL)").

:- comment(version(0*9+28,1999/03/27,15:02*07+'MET'), "Corrected
   problems in Windows 32 concerning drive numbers.  Touched main.c
   (for library_directory), unix_utils.c (for current_directory),
   foreign.c (for filename expansion).  (MCL)").

:- comment(version(0*8+44,1999/03/12,17:17*19+'MET'), "Changed
   definition of SETJMP for Solaris Sparc: sigsetjmp() was overwriting
   a (fundamental) global variable!  (MCL)").

:- comment(version(0*8+43,1999/01/19,18:58*32+'MET'), "Moved lock on
   predicate to current_clauses in order to debug concurrency
   problems.  Will be set at a lower level afterwards. (MCL)").

:- comment(version(0*8+42,1999/01/03,18:11*24+'MST'), "Added a lock on
   predicate root to make_undefined() so that concurrent (and, in
   general, all...) predicates are correctly erased when reloading new
   code, esp. from the toplevel.  (Manuel Carro)").

:- comment(version(0*8+41,1999/01/03,17:51*21+'MST'), "Profiling (-C
   -prof[t]) is only turned on when the engine is compiled with
   -DPROFILE.  This macro is defined when ""profiled"" or ""debug"" is
   chosen in the SETTINGS file.  (Manuel Carro)").

:- comment(version(0*8+40,1999/01/02,17:20*20+'MST'), "Handles to
   concurrent goals added to the release_goal/1 call and to the
   kill_goal call (actually, anything clearing a goal state will clear
   the handles as well).  (Manuel Carro)").

:- comment(version(0*8+39,1999/01/02,17:02*15+'MST'), "handles for
   suspended calls to concurrent facts are also cleared at the end of
   a launch_goal call which does allow further interactions (i.e.,
   without an associated GoalId).  (Manuel Carro)").

:- comment(version(0*8+38,1999/01/02,16:34*30+'MST'), "Added support
   for relocation of concurrent dynamic choicepoints when doing
   choicepoint stack shifting.  Seems to work just right!  (Manuel
   Carro)").

:- comment(version(0*8+37,1999/01/02,16:33*41+'MST'), "Pending
   concurrent calls cleaned up in all CUT* instructions; could not
   test all of them (I simply could not find code which generates
   those low-level instructions!).  (Manuel Carro)").

:- comment(version(0*8+36,1999/01/02,12:59*45+'MST'), "Handles to
   concurrent invokations were not being deleted upon cut & failure
   (but they were upon explicit failure in next_instance_conc,
   though).  Code in CUT_Y (file shdisp_r.c) was added to deallocate
   wuch handles when an alternative is cut.  (Manuel Carro)").

:- comment(version(0*8+35,1998/12/29,10:53*05+'MST'), "Moved code from
   make_undefined_predicate() to set_property(): the behavior of
   concurrent predicates was not being set corectly when code was bein
   compiled and loaded into the interpreter.  It was right, though,
   when code was just loaded.  (Manuel Carro)").

:- comment(version(0*8+33,1998/12/26,12:11*43+'MST'), "Detected some
   problems in the garbage collection: even if the concurrent goals
   were totally independent and there were no sharing of variables,
   the engine was breaking in some cases.  This was due to some static
   variables, related to gc, which were wrongly shared among goals.
   These are now private to each worker.  Still testing, though.
   (Manuel Carro)").

:- comment(version(0*8+32,1998/12/23,16:56*53+'MST'), "Solaris i86 has
   a problem in the setjmp library function: some global variables get
   clobbered (even if declared as volatile), which causes SIGSEG and
   falling into an infinite loop.  Temporarily (?) solved by renaming
   the macros SETJMP and LONGJMP when Solaris && i86.  (Manuel
   Carro)").

:- comment(version(0*8+31,1998/12/23,16:54*32+'MST'), "Made separate
   entries for Solaris in sparc and i86.  The same for Linux and NT in
   alpha and i86.  ARCHFLAG defined separately as sparc, mips, and i86
   (used in the assembler lock primitives).  (Manuel Carro)").

:- comment(version(0*8+1,1998/11/05,12:13*47+'MET'), "Unified engine
   changelog with global changelog.  (Manuel Hermenegildo)").

:- comment(version(0*0+0,1998/10/12), "

	* engine/startgoal.c (startgoal): Put provision for worker
	backtracking.  All wams() are started with a NULL pointer now.
	State is saved and recovered, and set in the state according to
	the actions passed.  Releasing worker implemented.  Backtracking
	still to be worked out.  

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/02/23), "

	* Makefile (eng): generation of configure.h is now enforced from
	the toplevel Makefile, to avoid races in the engine Makefile.
	Dependencies in the engine Makefile are however retained, to force
	possible recompilation of other dependent files.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/02/17), "

	* engine/prolog_tasks.c (prolog_launch_goal): launch_goal/[1,2]
	seem to accept goals with arguments, now.  These arguments must be
	copied to the local heap; this is not done right now.

	* lib/engine/builtin.pl: added (temporary) '$ciaolibdir'.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/02/05,12:14*27+'MET'), "

        * bin/SUNSOL/main.c (main): Added C option ""-dconc"" to set
        concurrency debugging flag.

        * Concurrent access to database partially working.  A big mess of
        data structures; needs rethinking.  Also, work on space recovery
        on backtracking.

        * bin/SUNSOL/prolog_tasks.c (prolog_launch_goal_2): Introduced
        launch_goal/2, which also returns the thread Id of the new goal.
        For some misterious reason, it seems to break in some cases!

        (Manuel Carro Linares  @tt{<mcarro@@orion.ctp.fi.upm.es>})").

:- comment(version(0*0+0,1998/02/03), "

	* Library/data.pl: retract_fact/1 made blocking; a non-blocking
	version (retract_fact_nb/1) added.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/02/03), "

	* Library/data.pl: '$unlock_predicate'/1 added after every call to
	'$current_instance'/1, which should leave a lock on the predicate
	(to be later changed to the clause) when the predicate is being
	executed.

	* bin/SUNSOL/prolog_tasks.c (prolog_unlock_pred): unlocks the
	possible lock left in a predicate.  Accessed through
	'$unlock_predicate'/1, which receives the root pointer.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/02/02), "

	* Library/data.pl: retract_fact/1 is non-blocking (which I assume
	is what is usually wanted): when there are no [more] instances of
	a concurrent predicate, the call fails.  Blocking can be performed
	using a current_fact/1 before.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/02/01,23:24*21+'MET'), "

	* bin/LINUX/configure.c (turn_point): take into account building a
	LINUX system without the built-in memory manager.

        (Manuel Carro  @tt{<boris@@clip2.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/01/19), "

	* bin/SUNSOL/objareas.c (prolog_erase): erasing locked --- but is
	this taking care properly of concurrent predicates?
	(inserta): clause lock used here.
	(insertz): clause lock used here.

	* bin/SUNSOL/indexing.c (init_interpreted): clause lock created
	and inited.

	* bin/SUNSOL/objareas.h: added a per-clause lock.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/01/16), "

	* bin/SUNSOL/nondet.c (current_instance): current_fact_nb seems to
	work.  Still to be extensively tested.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/01/16), "

	* bin/SUNSOL/nondet.c (current_instance_nt): Made a kludge to have
	a non-blocking current_instance_nt.  Same for next_instance_nt.
	Pass on an extra argument to skip busy waiting for concurrent
	predicates.   Used to implement current_fact_nb/1.  Still
	incomplete: need to save it somewhere!

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/01/14), "

	* Library/data.pl: added close_fact/1 and open_fact/1.  The former
	closes an open concurrent predicate, and the latter opens a closed
	concurrent predicate.

	* bin/SUNSOL/nondet.c (close_predicate): added a predicate to
	""close"" a predicate (signal that clauses should not be waited on
	any more).

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/01/14), "

	* bin/SUNSOL/registers.h (DynamicPreserved): incremented to 7.
	X(6) in dynamic chpt now holds the root for the dynamic predicate,
	where its openness is held.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/01/13), "

	* bin/SUNSOL/indexing.c (init_interpreted): openness of predicate
	inited acording to its concurrent/not concurrent type.

	* bin/SUNSOL/objareas.c (active_instance_nt): Made a version which
	does not look at time creation and waits instead of failing when
	predicates are still open.

	* bin/SUNSOL/objareas.h: added field to interpreter indexing data
	to denote that a predicate should be waited on insted of failed
	(in fact, this is changing the indexing behavior!).

	* Library/data.pl: Changed order of literals in several
	predicates: put '$current_clauses'/2 before, because it is most
	likely to fail ('$compile_term'/2 will always compile, even if a
	non dynamic fact name is passed on).

	* bin/SUNSOL/indexing.c (set_property): '$set_property'
	understands ""concurrent"".  A concurrent ""predicate"" is also
	dynamic.  This information is stored as a bit in the predicate's
	definition.

	* bin/SUNSOL/nondet.c (predicate_property_bits):
	predicate_property/2 understands ""concurrent"".

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/01/12), "

	* Added several ""concurrent"" at several places in Compiler and
	Library.  The compiler seems to recognize the ""concurrent""
	declaration both in incore compiling and in file-to-file
	compiling; however, some pitfall makes it load the .ql files twice!

	* bin/SUNSOL/indexing.c (set_property): Added code and
	declarations to set the ""concurrent"" property to predicates.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/01/09), "

	* bin/SUNSOL/prolog_tasks.c (prolog_lock_atom): lock_term/1 and
	unlock_term/1 are now called *_atom/1, and can only lock atoms.

	* Emulator/configure.c: Changed ""configure"" program to deal with
	Linux (and probably other OS's?) strange malloc beheavior
	exceeding the addressable (due to pointer encoding in tagged
	words) range of memory. See turn_point() in configure.c.

	* bin/SUNSOL/alloc.c (mem_prog_reset): Deleted.  It was just
	setting a variable to zero, and was called only once.

	* bin/SUNSOL/support.h: mem_start not used any more (replaced by
	total_mem_count, and was probably wrong whenever MallocBase <> 0).

	* bin/SUNSOL/main.c (create_and_init_wam): save Prolog program
	space usage and restore it after wam creation.
	(main): init program space counter after allocating shared space.

	* bin/SUNSOL/alloc.c: added a counter for total memory usage
	(Linux sbrk() does not behave as Solaris').

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/01/08), "

	* bin/SUNSOL/alloc.c: Added locks to the top-level calls of the
	memory manager (checkalloc/checkrealloc/checkdealloc).

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1998/01/08), "

	* Emulator/own_malloc.c: seems to work both in Linux and Soalris.
	There were problems with Linux malloc allocating in zones of the
	memory very far away, depending on the size of the requested
	memory.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/11/27,18:14*38+'MET'), "

	* Emulator/prolog_tasks.c: this file now has the implementation of
	the Prolog interface to the management of tasks.

        (Manuel Carro  @tt{<boris@@clip2.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/11/25,13:41*15+'MET'), "

        * makefile-sysdep/mkf-SUNSOL (THREAD_FLAGS): There is now the
	possibility of choosing between Solaris and POSIX threads.  POSIX
	seems to behave better now; I have made threads system-wide
	scheduled.  Thread killing using Solaris threads seems to kill the
	whole process.

        * bin/SUNSOL/threads.h: updated Solaris part to conform to use
        POSIX threads.  Great havoc.  Wrong implementation of POSIX
        threads?

        * bin/SUNSOL/tasks.c (allow_thread_cancellation): added primitives
        to allow/disallow thread cancellation.  Also for cancelling a new
        thread.

        * bin/SUNSOL/misc.c (prolog_lock_term): now working with atoms and
        functors *which are predicate definitions*.  Extending them to
        arbitrary functors (and variables) will be a little more
        expensive.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/11/19,20:41*44+'MET'), "

        * bin/SUNSOL/misc.c (prolog_lock_term): lock_term/1 locks the lock
        associated with a term.  Only atoms by now.  Similarly,
        unlock_term/1 does the opposite.

        * bin/SUNSOL/locks.c (create_dynamic_lock): Created a pool of
        storage for dynamic locks, to be generated as needed.  Still to be
        tested.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/11/18,20:48*24+'MET'), "

        * Emulator/initial.c (new_atom_check): Added lock and
        initialization of lock to atoms.  Important: need to change lock
        addresses when reallocing atom table (or make locks point to
        somewhere else --- probably a best solution).

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/11/12,16:37*03+'MET'), "

        * bin/SUNSOL/objareas.c (prolog_erase): added locks (same as for
        clause insertion) to clause deletion.  It is quite simple-minded:
        I simply lock all the process...

        * bin/SUNSOL/misc.c (prolog_tasks_status): Added predicate
        tasks_status/0, which prints in the user stream the current status
        of the tasks.

        * bin/SUNSOL/objareas.c (inserta / insertz): added lock to clause
        insertion.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/10/28,18:37*38+'MET'), "

        * bin/SUNSOL/term_support.c (compile_term): made reentrant by
        adding lots of parameters which avoid the use of global
        variables.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/10/17,14:08*37+'MET'), "

        * bin/SUNSOL/initial.c (define_c_mod_predicate): Added lock to
        prolog_predicates_l

        * bin/SUNSOL/support.c (insert_definition): Added lock to
        prolog_predicates_l 
        
        * bin/SUNSOL/initial.c (init_once): Added initialization of
        predicate locks.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/10/16,16:28*30+'MET'), "

        * bin/SUNSOL/wam.c (wam): Changed all `&prolog_predicates' to
        `predicates_location'. 

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/10/15,14:15*24+'MET'), "

        * bin/SUNSOL/termdefs.h: current_debugger_state and
        current_debugger_mode are now private to threads: since they point
        to heap-allocated structures, they where relocated upon GC and
        stack shifting --- but they might not point to the heap of the
        thread performing the reallocation!

        atom_buffer and atom_buffer_length are private to each thread;
        they can be accessed and expanded concurrently.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/09/24,13:29*34+'MET'), "

        * bin/SUNSOL/main.c (main): X(0) initialization moved to startwam.

        * bin/SUNSOL/initial.c (local_init_each_time): Redistributed
        start/reboot initializations between global and local to the WAM.
        
        * bin/SUNSOL/term_support.c (compile_term): changed compile_term
        and compile_term_aux to return in a new argument either NULL or
        the new WRB, if it has been expanded.

        * Emulator/tasks.c: added functions to create, search, etc. tasks
        to the task list.

        * Deleted lots of unused local variables.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/09/22,16:21*16+'MET'), "

        * Emulator: Added locks.h and threads.h file from Kish
        implementation.  Seems to work (in sample toy programs, at
        least).

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/08/29,19:54*18+'MET'), "

        * Emulator/initial.c: input_stream_ptr and output_stream_ptr moved
        to WRB and replaced by macros.

        * Emulator/support.c: the variables which (I believe) delimit the
        stack for bignums have been moved to WRB and replaced by macros;
        new args added where needed.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/08/28,17:26*38+'MET'), "

        * Emulator/wamfunction.c: split bn_div_mod and
        bn_quotient_remainder into two functions each, dealing with the
        case in which a remainder is wanted or not (there was a global
        parameter goberning that).

        * Emulator/bignum.c: (Slightly) Changed the implementation of
        rshift and lshift for the bignums case, in order to ged rid of the
        annoyin global bn_shift_dist which was used to pass a parameter,
        and should either be made global or locked when using threads.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/08/27,18:27*02+'MET'), "

        * Emulator/wambuiltin.c: added Arg to all bu2_* builtins; needed
        because of NDEREF, which calls evaluate(). Will ned to do the same
        for other builtins (because of WAMENV macros scattered accross all
        the code).

        * Emulator/wamfunction.c: All functions have been added a WRB
        argument, in order to be able to call them from within wam() using
        the provided C[2,3,4,5] macros.  The same applies to fu1_type()
        and fu1_get_attribute().

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/08/22,20:24*27+'MET'), "

        * Stack boundaries now moved into Wam Register Bank structure.
        Macros provided for its access.  Code changed to have the WRB
        accessible from everywhere it is needed.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/05/27,20:36*35+'MET'), "

        * flag '-f' (fast_load: do not load users's .ciaorc) now handled
        at Prolog level.  '-f' is respected when in interactive or in
        script mode.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/05/08,14:33*49+'MET'), "

        * Emulator/interrupt.c: Eliminated exitmsg(rc), changed calls to
        it to exit(rc).

        * Emulator/initial.c (init_each_time): Initialize debugger
        variables address_apply and address_interpret_c_goal (so that no
        reset_debugger(_) is needed at startup.

        * Emulator/wam.c (startwam): Changed in order to enable returning
        exit status with halt/1. Eliminated reinitialize/0 exit status, so
        that when booting does not enter and then reinitialize. On
        aborting, reboot/0 is called. Added ""wam_initialized = TRUE"" which
        eliminates '$context_switch' in boot.

        (Daniel Cabeza Gras  @tt{<bardo@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/04/29,16:33*50+'MET'), "

        * Emulator/main.c (main): added argument ""-q"" which sets
        current_quiet_flag (and ""-v"" which resets it), also in misc.c
        (quiet_flag) to change it. This flag is intended to be checked
        when writting informational messages.

        * Emulator/indexing.c (define_predicate): changed so that a
        multifile predicate is not erased when redefining it.  After a
        predicate is declared as multifile, the only way of delete it is
        by abolish.

        (Daniel Cabeza Gras  @tt{<bardo@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/04/22,16:33*50+'MET'), "

        * Emulator/misc.c (prolog_interpreted_clause): $interpreted_clause
        written in C because dynamic predicates in ql_execs.

        (Daniel Cabeza Gras  @tt{<bardo@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/04/21,12:59*50+'MET'), "

	* Library/Makefile (min_boot_done): Changed boot dependencies not
	to recompile uit unnecesarily.

        (Manuel Carro  @tt{<boris@@clip2.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/04/18,17:42*46+'MET'), "

	* Library/toplev.pl: Added calls to after_solution_hook,
 	after_query_hook and exit_prolog_hook.

        (El Bardo Leopardo  @tt{<bardo@@clip2.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/04/16,14:29*45+'MET'), "

        * Emulator/main_exec.c (ciao_kick_start): libciao now jumps to the
        boot/0 predicate, instead of main/0.  A minimal file, with error
        handling, etc., should be always included.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/04/15,20:25*10+'MET'), "

        * Emulator/main.c (main): Boot code for ql files in main checks
        for ql scripts.
        (main): Changed boot code to load several qls from one file (now
        it exits when EOF is found).

        * Emulator/qread.c (prolog_qread): Now it checks for ql scripts;
        if so, we skip until next crtl-L.

        * Emulator/initial.c (initialize_intrinsics): All C defined
        predicates are now public (to avoid allowing C predicates to be
        abolished by the user).

        * Emulator/objareas.c (context_switch): Changed predicate storage
        to use only one list (user_predicates dropped). 

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/02/25,19:35*06+'MET'), "

        * Emulator/interrupt.c: Added ""interactive"" variable, which should
        control the printing of several messages.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/02/13,18:07*56+'MET'), "

        * Emulator/unix_utils.c (prolog_getarch): added the built-in
        get_arch/1, which returns an atom representing the architecture
        for which the emulator has been compiled.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/02/13,12:45*50+'MET'), "

        * Library/toplev.pl: shell_files/2: added a rule to link a
        .so file. For now, the module name is obtained from the name of
        the file. It should be explicitly passed in a future.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/02/12,16:21*04+'MET'), "

        * Library/toplev.pl: Added rules to search and load a .so file,
        respecting the following rules:

            - .ql and .pl files are preferred over .so files

            - between .ql and .pl, the most recently changed is preferred

            - no file is loaded if it has not been changed since last loaded.

        * Emulator/foreign.c (prolog_find_file): Only regular (not
        directory) names are returned by absolute_file_name.

        * Emulator/misc.c (compare_aux): Changed standard term order to
        agree with the Prolog standard.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/02/10,16:21*32+'MET'), "

        * Emulator/dynlink.c (prolog_dynlink): dynlink/2 predicate
        working: dynlink(+File, +Module) dynamically loads the File.so
        shared object, and installs the Prolog-accesible predicates
        accesible in Module

        * lib/sockets/Makefile: shared libraries creation working for
        Solaris, SunOS and Linux.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/01/24,19:14*51+'MET'), "

        * Implemented atom_concat(+,+,?) builtin in term_support.c.

        (Daniel Cabeza Gras  @tt{<bardo@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/01/24,11:28*19+'MET'), "

        * Emulator/initial.c (define_c_mod_predicate): Allows defining new
        C predicates (at the level of those which use Argdecl) _after_ the
        system is already running. This would allow dynamically linking
        .so files which define new predicates written in C.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1997/01/20,21:08*11+'MET'), "

        * Added unix(popen/3) command, changed dummy.pl, initial.c,
        streams.c (perhaps it should go in unix_utils.c?)

        (Daniel Cabeza Gras  @tt{<bardo@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/11/15,21:17*09+'MET'), "

        * bin/SUNSOL/wam.c (wam): w->insn is now used to store the pc when
        not in wam(). ""p"" it is (re)loaded at entry and after succesful
        completion of C predicates.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/11/15,13:33*24+'MET'), "

        * Changed ""echo"" for ""write0"" through all the code.

        (Daniel Cabeza Gras  @tt{<bardo@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/11/13,19:36*31+'MET'), "

        * Added predicate write0/1 which calls wr_tagged_rec in builtin.c.

        (Daniel Cabeza Gras  @tt{<bardo@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/11/07,20:00*28+'MET'), "

        * Added ++, -- as functions and postfix operators, e.g., Y is X++ .

        (Manuel Carro & Daniel Cabeza @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/11/04,20:20*00+'MET'), "

        * Added pause/1, get_pid/1, walltime/1.

        (Manuel Carro  @tt{<boris@@pizarro.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/10/23,13:45*09+'MET'), "

        * Library/toplev.pl: Ciao now reads the ~/.ciaorc initialization file.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/10/22,19:12*38+'MET'), "

        * Emulator/sockets.c (prolog_select_socket): Done -- seems to work.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/10/21,19:29*45+'MET'), "

        * Emulator/sockets.c (prolog_connect_to_socket): opens a stream
        connected to a given port and host.

        (prolog_bind_socket): creates a socket bound to a (local) port;
        returns the socket id.

        (prolog_socket_accept): waits for a new connection on a socket and
        returns a stream to this connection

        (new_socket_stream): makes a stream from a socket number (a file
        descriptor). The stream has mode 's' (special, since a socket is
        readable/writeable at a time).

        (Manuel Carro  & Daniel Cabeza)").

:- comment(version(0*0+0,1996/09/27,11:44*30+'MET'), "

        * Emulator/Makefile: changed to create a script which loads ql
        files. I was unable to produce a working saved state in Solaris.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/09/11,14:32*13+'MET'), "

        * Made separate directories for different architectures under 'bin'.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/09/09,15:50*48+'MET'), "

        * Makefile (setup): Added 'setup' which creates links to hold the
        objects and binaries in different directories for the
        architectures.

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/08/01,19:45*26+'MET'), "

        * Emulator/compat.h: added compatibility macros for signal
        handling

        (Manuel Carro  @tt{<boris@@mayor.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/07/31,18:47*13+'MET'), "

        * lib/attrdecl.pl: multifile declarations for attribute handling
        predicates. This file must be loaded before any file declaring
        verify_attributes/2 or combine_attributes/2. All files using these
        predicates must be loaded either compiled or interpreted.
        (See file lib/freeze.pl and lib/clpr/clpq.pl).

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/07/30,17:30*16+'MET'), "

        * lib/clpr/clpq.pl: Renamed clpr's freeze/2 to clpr_freeze to
        avoid name clashes with the standard freeze/2. The
        attributes-based version of freeze/2 (in Library) cannot be used,
        because clp[qr] puts more information in the attributes.

        * Library/freeze.pl: Added freeze/2 and frozen/2 based on
        attributed variables. Debugger still not tested, though.

        * Library/toplev.pl (break_body): Changed toplevel to take into
        account attributed variables: attributes are displayed, and not
        simply taken as frozen goals.

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/07/26,14:08*43+'MET'), "

        * Emulator/attr.c (fu1_type): changed ""cva"" atom to ""attv"" atom
        through all the code (note: this changes type/2 behavior).

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/07/25,19:32*48+'MET'), "

        * Added clpq in ciao/lib/clpr. Loaded with [clpq].

        * Added attributed variables based on SICStus 2.1 patch.
        Seems to work. Debugger not tested.

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/07/11,16:43*41+'MET'), "

        * Makefile (version-CK): Created the directory Emulator-CK and
        fixed Makefiles, names, etc. to use the name Ciao instead of
        SICStus.

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/06/12,19:39*00+'MET'), "

        * lib/exec/Emulator/main_exec.c (ciao_init): flag -b <file.ql>
        allows loading a ql file

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/06/11,16:36*50+'MET'), "

        * lib/exec/Emulator/qinsert.c (qinsert): created qinsert.c to handle
        C generated qlfiles.

        * Emulator/main.c (main): Moved some initialization code to first
        block of main().

        * lib/exec/Emulator/Makefile (libciao): Added libciao target

        * lib/exec/Makefile: changed to include libciao target and
        position independent code flags.

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/05/13,22:23*45+'MET'), "

        * Changed Library/rdtok.pl for Library/tokenize.pl (changed inout.c),
        added prolog flag ""character_escapes"" (changed Emulator/initial.c,
        Library/intrinsics1.pl).  Added get_class/2, get0_class/2 to
        Library/intrinsics1.pl.

        (Daniel Cabeza Gras  @tt{<bardo@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/05/08,19:27*20+'MET'), "

        * Code related to native code support eliminated from Emulator and
        Compiler. In Emulator original files renamed as <file>_nc.

        (Daniel Cabeza Gras  @tt{<bardo@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/05/08,01:05*12+'MET'), "

        * lib/proTcXl2.1/foreign.pl: tcl/[1,2,3] now support strings.

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/05/03,20:08*49+'MET'), "

        * lib/proTcXl2.1/Makefile (PROTCL_SOURCE): SETTINGS is now
        consulted to find out the source directory.

        * Makefile: SRC is now set in settings

        * TO_SOLVE: write here the problems you find, just to make sure
        they are eventually dealt with.

        * lib/proTcXl2.1/tcl.pl: Tcl interface is partially working. Still
        to be tested. Most of the original (Micha's) code, dealing with
        variables substitution, rewritten. 

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/04/30,12:48*46+'MET'), "

        * lib/proTcXl2.1/misc_eclipse.pl: Added miscellaneous Prolog
        predicates not found in SICStus 0.7.

        * lib/proTcXl2.1/Makefile: Added 'tags' rule to make tags entry
        for Prolog and C files.

        * lib/proTcXl2.1/xlib.pl: Removed module declarations.

        * Emulator/inout.c: Defined macros SAVE_XREGS and RESTORE_XREGS to
        save/restore X registers when SP_read_hook is called (C could call
        prolog again).
        (readchar): Added call to (*SP_read_hook)() if defined. A warning
        is printed the first time it is called if we are debugging.

        * mkf-* files, available_processors and getarch moved to directory
        makefile-sysdep

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/04/29,21:53*06+'MET'), "

        * lib/proTcXl2.1/tk_common.pl: Commented module declaration.
        Changed 

        * lib/proTcXl2.1/protcl.c (_FileCmdProc): this function is only
        used when the host prolog is Eclipse; #ifdef'ed

        * Emulator/sicstus.c: skeletons for C functions accessing SICStus
        internal stuff. By now, tcl/tk cannot call Prolog.

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/04/29,20:06*43+'MET'), "

        * Compiler/plwam.p4: extracted dict.pl, listutil.pl

        (Daniel Cabeza Gras  @tt{<bardo@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/04/29,19:39*29+'MET'), "

        * Emulator/sicstus.h: Created sicstus.h to put definitions used by
        C programs.

        * Emulator/inout.c (readchar): Added *(SP_read_hook)() to compile
        proTcXl -- but not yet working!

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/04/24,14:54*25+'MET'), "

        * Emulator/foreign.c (prolog_prepare_foreign_files): some
        functions used in other parts of the emulator have been moved to
        unix_utils.c
        
        Added #ifdef's to avoid compiling functions used only when loading
        foreign files. Added dummy functions which print errors if
        one tries to link a foreign file and the emulator has not been
        compiled with that capability.

        * Makefile: Added some configuration files from former &-Prolog
        versions to help porting

        * SETTINGS has a FOREIGN_FILES flag to enable/disable linking of
        foreign files

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").

:- comment(version(0*0+0,1996/04/17,17:36*00+'MET'), "

        * Changed name of RCS directories to _RCS_ to prevent gmake from using
        the RCS stored files

        * Changed toplevel Makefile and some C sources to use gcc and gmake

        (Manuel Carro  @tt{<boris@@aguirre.dia.fi.upm.es>})").
% ---------------------------------------------------------------------------

