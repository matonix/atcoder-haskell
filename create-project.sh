#!/bin/bash

CMDNAME=`basename $0`
if [ $# -ne 1 ]; then
  echo "Usage: $CMDNAME <contest-name> " 1>&2
  exit 1
fi

echo "Create $1. Wait a moment..."

stack new $1 --resolver=lts-14.22 https://raw.githubusercontent.com/matonix/atcoder/master/stack-templates/atcoder.hsfile

echo "done"
