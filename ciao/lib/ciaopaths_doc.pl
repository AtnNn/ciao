:- use_package([assertions,iso]).
:- doc(nodoc,assertions).

:- doc(filetype,package).

:- doc(title,"Library Paths for Ciao Bundles").
:- doc(author, "The CLIP Group").

:- doc(module,"
This package setups the file search path and library aliases to access
all the available Ciao bundles.
").

:- doc(bug, "The package is disabled by default for efficiency reasons
(forces many dependencies on minimal programs). Nevertheless, a
lighter implementation could be enabled by default for the 'ciao'
dialect in the near future (and make the package disappear).").
