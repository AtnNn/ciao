:- module(mysql_use_client, [main/0], [persdb_mysql, assertions]).

%% This module exists only to force compilation of the corresponding .so


main.

:- comment(version_maintenance,dir('../../version')).


%% Note that the "assertions" library needs to be included in order
%% to support ":- comment(...,...)." declarations such as these.
%% These version comment(s) can be moved elsewhere in the file.
%% Subsequent version comments will be placed above the last one
%% inserted.

:- comment(version(1*7+210,2002/04/29,16:15*21+'CEST'), "Created this
dummy program file only to foce automatic compilation of the mysql
client shared libraries.  The .c are not compiled into .so when only a
.po is made; this matter should be solved somehow.  (MCL)").

