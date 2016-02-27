% Common definitions that are repeated in several files, but depend on
% 'distpkg_root_dir/1'.

% This avoid duplicated code. However, we would need improved language
% support for it. 

%:- use_module(library(distutils(distpkg_versions))).

distpkg_version := ~distpkg_obtain_version(~distpkg_root_dir).
distpkg_version_nice := ~distpkg_obtain_version_nice(~distpkg_root_dir).
distpkg_name_version := ~distpkg_obtain_name_version(~distpkg_name, ~distpkg_root_dir).
