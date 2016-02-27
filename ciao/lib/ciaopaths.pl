:- package(ciaopaths).

% (see ciaopaths_doc.pl for documentation)
%
% A simple package to make the file search paths and alias library
% names of all Ciao modules available.
% 
% TODO: Since the loading of configuration files is not very efficient
% for the time being, this is disabled by default to avoid overheads.
% 
% (Jose F. Morales)

:- use_module(library(bundle_registry(bundle_registry_load)), []).
