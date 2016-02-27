:- module(iso_incomplete, [close/2, stream_property/2], [assertions]).

:- comment(title, "Incomplete ISO Prolog predicates").

:- comment(author, "The CLIP Group").

:- comment(module, "This module implements some ISO Prolog predicates,
   but that are not complete yet.").

open(F, M, S, _) :- open(F, M, S).
close(S, _) :- close(S).

stream_property(S, P) :- % It is not complete
        current_stream(File, Mode, S),
        ( P = file_name(File)
        ; P = mode(Mode)
        ; Mode = read ->
            P = input
        ; P = output
        ).

% at_end_of_stream :- not_yet_implemented.
% at_end_of_stream(_) :- not_yet_implemented.
% 
% set_stream_position(_,_) :- not_yet_implemented.
% 
% char_conversion(_,_) :- not_yet_implemented.
% current_char_conversion(_,_) :- not_yet_implemented.

%% *** Delete this comment after reading: it is only a reminder! ***
%% 
%% The "assertions" library needs to be included in order to support
%% ":- comment(...,...)." declarations such as below, i.e., insert: 
%% 
%% :- module(_,_,[assertions]).
%% 
%% At the beginning of the file:
%% The following version comment(s) can be moved elsewhere in the 
%% file. Subsequent version comments will always be placed above 
%% the last one inserted.


:- comment(version_maintenance,dir('../version')).

:- comment(version(1*9+263,2003/12/31,11:55*21+'CET'), "Added
   documentation.  (Edison Mera)").

