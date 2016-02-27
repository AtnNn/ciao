#!/bin/sh
CIAOVERSION=
if [ ${CIAODOCDIR:-0} = 0 ] ; then
  cat <<EOF
Welcome to the Ciao Prolog Development System!

** WARNING **: your shell initialization scripts have not been properly 
modified for locating the Ciao on-line documentation. Please add the 
following lines to your shell initialization script:

A) If you use a csh-compatible shell (csh, tcsh, ...), add to ~/.cshrc:

  if ( -e <CIAOLIBDIR>/DOTcshrc ) then
     source <CIAOLIBDIR>/DOTcshrc
  endif

B) If you use an sh-compatible shell (sh, bash, ...), add to ~/.profile:

  if [ -f <CIAOLIBDIR>/DOTprofile ]; then
     . <CIAOLIBDIR>/DOTprofile
  fi

This will set up things so that you can access the Ciao system manuals
using the 'info' command.

Also, if you use emacs (highly recommended) you should add this line to 
your ~/.emacs file: 

  (load-file "<CIAOLIBDIR>/DOTemacs")

This will set up things so that you can access manuals from inside
emacs. The on-line manuals are available by typing C-h i in emacs or
also by visiting:

<CIAODOCDIR>/index.html

with a WWW browser.

You may also want to join on the ciao-users mailing list, in order to
receive information on new versions and solutions to problems: simply
send a message to 'ciao-users-request@clip.dia.fi.upm.es', containing
in the body the word:

subscribe 

alone in one line. Messages are strictly limited to issues directly
related to Ciao Prolog and we will of course keep your email address
strictly confidential. See for more info:

http://clip.dia.fi.upm.es/Software


EOF
#  exit 1
fi

if test $# = 0 ; then
  exec ciaosh$CIAOVERSION
elif test $1 = -c; then
  shift ; exec ciaoc$CIAOVERSION "$@"
else
  exec ciao-shell$CIAOVERSION "$@"
fi
