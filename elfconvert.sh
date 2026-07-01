#!/bin/sh

input=$1
output=$2

# This is a bit of a hack, but we don't know which "actual" file the :sbcl_exe
# alias rule picked out. A glob will find it- there can be only 1.
sbcl=`echo $RUNFILES/google3/third_party/lisp/sbcl/install/*`
args=(--dynamic-space-size 1024MB)
mode='(setq *evaluator-mode* :compile)'
script=$RUNFILES/google3/third_party/lisp/sbcl/src/tools-for-build/elftool

exec $sbcl ${args[@]} --eval "$mode" --load $script --eval \
      '(sb-editcore:split-core "'$input'" "'$output'")' --quit
