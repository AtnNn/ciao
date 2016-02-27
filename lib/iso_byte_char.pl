:- module(iso_byte_char,
        [char_code/2, atom_chars/2, number_chars/2,
         get_byte/1, get_byte/2,
         peek_byte/1, peek_byte/2,
         put_byte/1, put_byte/2,
         get_char/1, get_char/2,
         peek_char/1, peek_char/2,
         put_char/1, put_char/2], []).

char_code(Ch, C) :- atom_codes(Ch, [C]).

atom_chars(Atom, Chs) :-
        atom(Atom), !,
        atom_codes(Atom, S),
        char_codes(Chs, S).
atom_chars(Atom, Chs) :-
        char_codes(Chs, S),
        atom_codes(Atom, S).

number_chars(Number, Chs) :-
        number(Number), !,
        number_codes(Number, S),
        char_codes(Chs, S).
number_chars(Number, Chs) :-
        char_codes(Chs, S),
        number_codes(Number, S).

char_codes([], []).
char_codes([Ch|Chs],[C|Cs]) :-
        char_code(Ch, C),
        char_codes(Chs, Cs).


get_byte(B)   :- get_code(B).
get_byte(S,B) :- get_code(S,B).
peek_byte(B)   :- peek_code(B).
peek_byte(S,B) :- peek_code(S,B).
put_byte(B)   :- put_code(B).
put_byte(S,B) :- put_code(S,B).

get_char(C)   :- get_code(B), char_code(C,B).
get_char(S,C) :- get_code(S,B), char_code(C,B).
peek_char(C)   :- peek_code(B), char_code(C,B).
peek_char(S,C) :- peek_code(S,B), char_code(C,B).
put_char(C)   :- char_code(C,B), put_code(B).
put_char(S,C) :- char_code(C,B), put_code(S,B).

