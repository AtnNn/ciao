#!/bin/sh
#
# conv.sicstus.sh: bootstrapping aprolog in SICStus
# by pts@fazekas.hu at Fri Jan 13 18:20:16 CET 2006
#
# Dat: see Makefile for usage
# Dat: tested with SICStus 3.12.0
#

SICSTUS=""
for P in sicstus sicstus3.12; do
  unalias "$P" 2>/dev/null
  unset -f "$P" 2>/dev/null
  if type "$P" >/dev/null 2>&1; then
    SICSTUS="$P"
    break
  fi
done

if test "$SICSTUS"; then :; else
  echo "$0: SICStus command not found" >&2
  exit 126
fi

<conv.pl awk '!/^:- *use_module\(/{print}' >conv.sicstus.pl
cat >>conv.sicstus.pl <<':- true.'
:- use_module(library(lists)). %[append/3, member/2, length/2]
error_stream(user_error).
% vvv Imp: use user_error etc.
'execution:write_exception'(_Stream, Exc) :-
  print_message(error, Exc).
:- prompt(_, ''). % Dat: no effect, but no problem, sincs SICStus does not display the prompt unless reading from STDIN
:- prolog:'$abolish'(control_c_handler, prolog),
   prolog:asserta((control_c_handler :- halt(125))).

:- true.

# vvv Dat: SICStus is smart enough to emit prompt and debug messages to STDERR.
"$SICSTUS" -l conv.sicstus.pl --goal 'print(cut__cut),nl,catch(conv,E,handle_exception(E)).' >conv.sicstus.tmp
R="$?" # Imp: Dat: won't pipe to awk, so we can check the exit code
if [ "$R" = 0 ]; then
  awk 'p{print} !p&&/^cut__cut$/{p=1}' <conv.sicstus.tmp
fi
rm -f conv.sicstus.pl conv.sicstus.tmp
exit "$R"
