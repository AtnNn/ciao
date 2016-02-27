:- module(atomic_basic, [
        name/2, atom_codes/2, number_codes/2, number_codes/3,
        atom_length/2, atom_concat/3, sub_atom/4],
        [assertions, isomodes]).

:- use_module(engine(internals), ['$prolog_radix'/2]).

:- comment(title, "Basic predicates handling names of constants").

:- comment(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- comment(module, "The Ciao system provides builtin predicates which
   allow dealing with names of constants (atoms or numbers).  As an atom
   name must be of less than 512 characters, to handle sequences of more
   characters, strings (character code lists) must be used.").

% Defined in C

:- impl_defined([
        name/2, atom_codes/2, number_codes/2,
        atom_length/2, atom_concat/3, sub_atom/4]).

:- comment(name(Const,String), "@var{String} is the list of the ASCII
   codes of the characters comprising the name of @var{Const}.  Note
   that if @var{Const} is an atom whose name can be interpreted as a
   number (e.g. '96'), the predicate is not reversible, as that atom
   will not be constructed when @var{Const} is uninstantiated.  Thus it
   is recommended that new programs use the ISO-compliant predicates
   @pred{atom_codes/2} or @pred{number_codes/2}, as these predicates do
   not have this inconsistency.").

:- true pred name(+constant,?string).
:- true pred name(-constant,+string)
   # "If @var{String} can be interpreted as a number, @var{Const} is unified
      with that number, otherwise with the atom whose name is @var{String}.".

:- comment(atom_codes(Atom,String), "@var{String} is the list of the ASCII
   codes of the characters comprising the name of @var{Atom}.").

:- true pred atom_codes(+atm,?string) + iso.
:- true pred atom_codes(-atm,+string) + iso.

:- comment(number_codes(Number,String), "@var{String} is the list of the
   ASCII codes of the characters comprising a representation of
   @var{Number}.").

:- true pred number_codes(+num,?string) + iso.
:- true pred number_codes(-num,+string) + iso.

:- comment(number_codes(Number,String,Base), "@var{String} is the list
   of the ASCII codes of the characters comprising a representation of
   @var{Number} in base @var{Base}.").

:- true pred number_codes(+num,?string,+int).
:- true pred number_codes(-num,+string,+int).

number_codes(N, S, Base) :-
        integer(Base),
        '$prolog_radix'(Rdx, Base),
        ( number_codes(N, S) ->
            '$prolog_radix'(_, Rdx)
        ; '$prolog_radix'(_, Rdx),
          fail
        ).

:- comment(atom_length(Atom,Length), "@var{Length} is the number of
   characters forming the name of @var{Atom}.").

:- true pred atom_length(+atm,?int) + iso.

:- comment(atom_concat(Atom_1,Atom_2,Atom_12), "@var{Atom_12} is the
   result of concatenating @var{Atom_1} followed by @var{Atom_2}.").

:- true pred atom_concat(+atom,+atom,?atom) + iso
   # "Concatenate two atoms.".
:- true pred atom_concat(-atom,-atom,+atom) + iso
   # "Non-deterministically split an atom.".
:- true pred atom_concat(-atom,+atom,+atom) + iso
   # "Take out of an atom a certain suffix (or fail if it cannot be done).".
:- true pred atom_concat(+atom,-atom,+atom) + iso
   # "Take out of an atom a certain prefix (or fail if it cannot be done).".

:- comment(sub_atom(Atom,Before,Length,Sub_atom), "@var{Sub_atom} is
   formed with @var{Length} consecutive characters of @var{Atom}
   after the @var{Before} character.  For example, the goal
   @tt{sub_atom(summer,1,4,umme)} succeeds.").

:- true pred sub_atom(+atm,+integer,+integer,?atm).

:- comment(version_maintenance,dir('../../version')).

