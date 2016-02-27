# -*- mode: Makefile; -*-

HOSTS=
# sxde108

USER[sxde108]=edison
SUBDIR[sxde108]=src
MAKE[sxde108]=gmake
