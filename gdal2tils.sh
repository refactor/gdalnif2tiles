#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

NAME_ARG=`egrep '^\-name' ./config/vm.args 2> /dev/null`
echo $NAME_ARG

COOKIE_ARG=`grep '^\-setcookie' ./config/vm.args 2> /dev/null`
echo $COOKIE_ARG


alias NODETOOL="./nodetool $NAME_ARG $COOKIE_ARG"

