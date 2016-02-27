#!/bin/sh
# Grep on Ciao sources
#
# Author: Jose F. Morales
#
#   This command performs a grep on Ciao source files (avoid .svn
#   files and automatically generated files like .po, .itf, files
#   under build/).
#
# TODO: 
#   - Nothing is cached or precomputed (so search is really slow)
#   - Search is not semantic nor syntactic.

# ---------------------------------------------------------------------------
# Obtain the directory where this script is located
old_dir=`pwd`; cd `dirname $0`; self=`pwd`; cd ${old_dir}; old_dir=

# Exit immediately if a simple command exits with a non-zero status
# ---------------------------------------------------------------------------


set -e

cd ${self}/../..
find . -name '.svn' -prune -o \
       -path './build' -prune -o \
       \( -not -name '*~' \) \
       -exec grep -I -nH -e "$*" {} \; 

