:- module(install_utils, [
        error_dir/1,
        img_dir/1
                         ], []).

:- use_package(assertions).

:- comment(author, "Isabel Martín").


error_dir(Dir):-
        absolute_file_name(library(chartlib), '', '.pl', '.', _AbsFile, _AbsBase, AbsDir),
        atom_concat(AbsDir, '/errors/',Dir).
        

img_dir(Dir):-
        absolute_file_name(library(chartlib), '', '.pl', '.', _AbsFile, _AbsBase, AbsDir),
        atom_concat(AbsDir, '/images/',Dir).
        




:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*9+164,2003/12/04,17:39*40+'CET'), "Added author
info (Manuel Carro)").

