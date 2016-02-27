:- use_package(assertions).

:- doc(filetype, documentation).
:- doc(title,"Stable Version").
:- doc(author,"The CLIP Group").

:- doc(pragma, section_image('Box-128.png')).

% TODO: Put this text? Put all downloads in the same page? (As well as
%   a page for documentation? -- those pages are too short otherwise)
%
% Note: from 0.8 on, 'even' versions (e.g., 1.0, 1.4, ...) are stable
% distributions, whereas odd versions (e.g., 1.1, 1.5, ...) are
% versions under testing. These 'odd'-numbered versions typically are
% meant for people collaborating in Ciao development or people who
% would like to try out new functionality and help us with testing of
% upcoming versions.

:- doc(module,"

This is the stable version for Ciao. 

You can consult the @bf{release notes} for this version through the
following links:
@begin{itemize}
@item @pbundle_href{1.14}{Ciao Manual}{ciaochanges.html}{Release notes for Ciao}
@item @pbundle_href{1.14}{LPdoc Manual}{lpdocchanges.html}{Release notes for LPdoc} (included in Ciao)
@end{itemize}

@pbundle_download{1.14}{code}

@bf{Note for Windows users:} the setup program @bf{must be executed as
Administrator}.  To do this save it first (do not execute it
directly), go to the directory where the file was downloaded,
right-click over the file, select @bf{Run as administrator} (in
Windows XP, @bf{Run as...} and then select the user Administrator),
and follow the instructions that appear on the screen.

@bf{Other platforms:} please contact us if your platform is not
currently supported and/or you wish us to include pre-packaged binary
distributions for it.  
").


