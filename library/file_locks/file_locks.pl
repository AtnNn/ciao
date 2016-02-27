:- module(file_locks, [lock_file/3, unlock_file/2], [assertions]).
%%  A Prolog interface to block files using C

:- comment(title,"File locks").
:- comment(author, "J. Gomez, D. Cabeza, M. Carro").
:- comment(module,"This module implements file locks: the ability to
        lock a fiel so that other processes cannot access it until the
          file is unlocked.  @bf{It is, however, not working.}  The
          predicates do nothing. Proper implementation is planned for
          a near future.").  

:- pred lock_file(File, LockType, Result) : atm * atm * atm # "Tries
        to lock @var{File} with @var{LockType} and returns the result
          (either @tt{true} or @tt{false}) in @var{Result}.".
lock_file(_, _, _).

:- pred unlock_file(File, Result) : atm * atm # "Tries
        to unlock @var{File} the result
          (either @tt{true} or @tt{false}) in @var{Result}.".
unlock_file(_,_).

:- comment(bug, "No doing anything helpful.").

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*7+159,2001/11/27,11:58*24+'CET'), "Added comments
in file locks not doing anything.  (MCL)").

:- comment(version(1*7+157,2001/11/26,18:43*01+'CET'), "Removed C
files not yet working (MCL)").
