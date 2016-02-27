:- use_package(assertions).

:- doc(filetype, documentation).
:- doc(title,"Development Version").
:- doc(author,"The CLIP Group").

:- doc(pragma, section_image('Box-128.png')).

% TODO: Add smaller (sub) pbundles? That should not be hard now.
% TODO: Parts of this text are duplicated in download_stable.pl
:- doc(module," 
This is the latest development version for Ciao. Development versions
typically are meant for people collaborating in Ciao development or
people who would like to try out new functionality and help us with
testing of upcoming versions. 

You can consult the @bf{release notes} for this version through the
following links:
@begin{itemize}
@item @pbundle_href{master}{Ciao Manual}{ciaochanges.html}{Release notes for Ciao}
@item @pbundle_href{master}{LPdoc Manual}{lpdocchanges.html}{Release notes for LPdoc} (included in Ciao)
@end{itemize}

@pbundle_download{master}{code}

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


