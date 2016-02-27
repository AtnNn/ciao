#!/bin/sh
#
# conv.swipl.sh: bootstrapping aprolog in SWI-Prolog
# by pts@fazekas.hu at Fri Jan 13 18:20:16 CET 2006
#
# Dat: see Makefile for usage
# Dat: tested with SWI-Prolog (Version 5.4.7)
#

SWIPROLOG=""
for P in swi-prolog swiprolog swipl pl; do
  unalias "$P" 2>/dev/null
  unset -f "$P" 2>/dev/null
  if type "$P" >/dev/null 2>&1; then
    SWIPROLOG="$P"
    break
  fi
done

if test "$SWIPROLOG"; then :; else
  echo "$0: SWI-Prolog command not found" >&2
  exit 126
fi

<conv.pl awk '!/^:- *use_module\(/{print}' >conv.swipl.pl
cat >>conv.swipl.pl <<':- true.'
%%%% pts %%%%
% vvv Imp: use user_error etc.
'execution:write_exception'(_Stream, Exc) :-
  print_message(error, Exc).
error_stream(user_error).
:- prompt(_, ''). % Dat: remove '|: ' from stdout
:- catch(conv, E, handle_exception(E)), halt.

:- true.

# vvv Dat: prints `:-' for SWI-Prolog
"$SWIPROLOG" -q -s conv.swipl.pl
R="$?"
rm -f conv.swipl.pl
exit "$R"
