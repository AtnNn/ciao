% ===========================================================================
:- module(_,_,[functions,assertions]).
% ===========================================================================
:- comment(title,  "Ciao Future Global Compilation/Installation SETTINGS").
:- comment(author, "Edison Mera").
% ===========================================================================
:- use_module(library(terms), [atom_concat/2]).

% ===========================================================================
% Set the appropriate values below before installing the CiaoPP system.
% 
% The defaults listed are suggestions and/or the ones used for local 
% installation in the CLIP group machines.
% ===========================================================================

% Operating system type:
os := 'Unix'.

% The lpmake tool (distributed with the Ciao system)
% Unix: 
lpmake := lpmake. 
% Windows: 
% lpmake := 'lpmake.bat' . 

% The following should be erased when the system becomes free of 
% traditional makefiles.
gmake := gmake.

% The Main Ciao version
ciaoversion := '1.11'.

% The Ciao version used to build CiaoPP
prolog  := ~atom_concat('ciao-', ~ciaoversion).

% Name of the Ciao compiler
ciaoc   := ~atom_concat('ciaoc-', ~ciaoversion).

% Name of the Ciao Shell
ciaosh  := ~atom_concat('ciaosh-', ~ciaoversion).

% The path when Ciao Source code resides
% ciaosrc := ~atom_concat('/home/clip/Systems/', ~prolog).

% The Ciao library path
ciaolib := ~atom_concat('/home/clip/lib/ciao/', ~prolog).

% The Ciao engine.  The commented line is the development ciaoengine
% ciaoengine := ~atom_concat([~ciaosrc, '/bin/LINUXi86/ciaoengine']).

ciaoengine := ~atom_concat([~ciaolib, '/engine/ciaoengine']).

% Define this to be the permissions for installed execs/dirs and data files:
execmode(perm(rwx, rwx, rx)).
datamode(perm(rw, rw, r)).

% ===========================================================================
% Happy compilation!
% ===========================================================================

% ===========================================================================
% Lpdoc changelog:
% ===========================================================================

:- comment(version_maintenance,dir('version')).

% ===========================================================================
