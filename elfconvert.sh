#!/bin/sh

# The runfiles of this shell script will contain either the ordinary (not MSAN)
# binary-distribution of SBCL, or MSAN depending on the build target's config.
# But we don't really want that! editcore.lisp is capable of operating on any
# SBCL core and executing in any SBCL. So this should always prefer non-msan,
# but that's infeasible because lisp/sbcl/binary-distribution:BUILD chooses
# for you which subdirectory you get, and you can't pick both.
case $SAR_ARGV0 in
  *-msan*) sbcl_subdir=k8-msan ;;
  *) sbcl_subdir=k8
esac
sbcl=$RUNFILES/google3/third_party/lisp/sbcl/binary-distribution/$sbcl_subdir/bin/sbcl
args=(--noinform --dynamic-space-size 512MB)
mode='(setq *evaluator-mode* :compile)'
script=$RUNFILES/google3/third_party/lisp/sbcl/src/tools-for-build/editcore

action=$1
input=$2
output=$3

case $action in
  split)
    exec $sbcl ${args[@]} --eval "$mode" --load $script --eval \
      '(sb-editcore:split-core "'$input'" "'$output'")' --quit ;;
  copy)
    exec $sbcl ${args[@]} --eval "$mode" --load $script --eval \
      '(sb-editcore::copy-to-elf-obj "'$input'" "'$output'")' --quit ;;
  *)
    echo Unknown command: $action
    exit 1
esac
