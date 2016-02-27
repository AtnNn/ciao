:- module(iso_byte_char,
        [char_code/2, atom_chars/2, number_chars/2,char_codes/2,
         get_byte/1, get_byte/2,
         peek_byte/1, peek_byte/2,
         put_byte/1, put_byte/2,
         get_char/1, get_char/2,
         peek_char/1, peek_char/2,
         put_char/1, put_char/2], [assertions,isomodes]).

:- doc(title, "The Iso Byte Char module").

:- doc(author, "The CLIP Group").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Edison Mera (documentation)").
:- doc(author, "Manuel Hermenegildo (minor mods)").

:- doc(module, "This module provides some basic predicates
   according to the ISO specification of byte and char
   manipulation.").


:- pred char_code(+atm, ?character_code) + iso.
:- pred char_code(-atm, +character_code) + iso.

:- doc(char_code(Char,Code), "Succeeds iff the character code of
   the one char atom @var{Char} is @var{Code}.").

char_code(Ch, C) :- 
	    atom_codes(Ch, [C]).

% Corrected assertions:
:- pred atom_chars(+atm,?list(atm)) + iso.
:- pred atom_chars(-atm,+list(atm)) + iso.

% Were wrong:
% :- pred atom_chars(+atm,?list(character_code)) + iso.
% :- pred atom_chars(-atm,+list(character_code)) + iso.

:- doc(atom_chars(Atom,Chars), "Succeeds iff @var{Chars} is a list
   whose elements are the one-char atoms whose names are the
   successive characters of the name of atom @var{Atom}").

atom_chars(Atom, Chars) :-
        atom(Atom), !,
        atom_codes(Atom, S),
        char_codes(Chars, S).
atom_chars(Atom, Chars) :-
        char_codes(Chars, S),
        atom_codes(Atom, S).


:- pred number_chars(+num, ?list(atm)) + iso.
:- pred number_chars(-num, +list(atm)) + iso.

:- doc(number_chars(Number,Chars),"Success iff @var{Chars} is a
   list whose elements are the one-char atoms corresponding to a
   character sequence of @var{Number} which could be output").

number_chars(Number, Chars) :-
        number(Number), !,
        number_codes(Number, S),
        char_codes(Chars, S).
number_chars(Number, Chars) :-
        char_codes(Chars, S),
        number_codes(Number, S).


:- pred char_codes(+list(atm), ?list(character_code)).
:- pred char_codes(-list(atm), +list(character_code)).

char_codes([], []).
char_codes([Char|Chars],[C|Cs]) :-
        char_code(Char, C),
        char_codes(Chars, Cs).


:- pred get_byte(?int) + iso # "Same as @pred{get_byte/2}, but
   use the current input.".

get_byte(B) :- get_code(B).


:- pred get_byte(@stream, ?int) + iso.

:- doc(get_byte(Stream, Byte), "Is true iff @var{Byte} unifies
   with the next byte to be input from the target @var{Stream}.").

get_byte(S,B) :- get_code(S,B).

:- pred peek_byte(?int) + iso # "Same as @pred{peek_byte/2}, but
   use the current input.".

peek_byte(B)   :- peek_code(B).


:- pred peek_byte(@stream, ?int) + iso # "Is true iff @var{Byte} unifies
   with the next byte to be input from the target @var{Stream}.".

peek_byte(S,B) :- peek_code(S,B).

:- pred put_byte(+int) + iso # "Same as @pred{put_byte/2}, but
   use the current input.".

put_byte(B)   :- put_code(B).

:- pred put_byte(@stream, +int) + iso.

:- doc(put_byte(Stream,Byte), "Is true.  Procedurally,
   @pred{putbyte/2} is executed as follows:

   a) Outputs the byte @var{Byte} to the target stream.

   b) Changes the stream position of the target stream to take account
      of the byte which has been output.

   c) The goal succeeds.").

put_byte(S,B) :- put_code(S,B).

:- pred get_char(?atm) + iso # "Same as @pred{get_char/2}, but
   use the current input.".

get_char(C)   :- get_code(B), char_code(C,B).

:- pred get_char(@stream, ?atm) + iso.

:- doc(get_char(Stream, Char), "Is true iif @var{Char} unifies
   with the next character to be input from the target
   @var{Stream}.").

get_char(S,C) :- get_code(S,B), char_code(C,B).

:- pred peek_char(?atm) + iso # "Similar to @pred{peek_code/1},
   but using char instead of code.".

peek_char(C)   :- peek_code(B), char_code(C,B).

:- pred peek_char(@stream, ?atm) + iso # "Similar to
   @pred{peek_code/2}, but using char instead of code.".

peek_char(S,C) :- peek_code(S,B), char_code(C,B).

:- pred put_char(+atm) + iso # "Similar to @pred{put_code/1},
   but using char instead of code.".

put_char(C)   :- char_code(C,B), put_code(B).

:- pred put_char(@stream, +atm) + iso # "Similar to
   @pred{put_code/2}, but using char instead of code.".

put_char(S,C) :- char_code(C,B), put_code(S,B).
