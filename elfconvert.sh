#!/bin/sh

action=$1
input=$2
output=$3

case $action in
  split)
    exec $RUNFILES/google3/third_party/lisp/sbcl/binary-distribution/k8/bin/sbcl \
      --noinform --dynamic-space-size 512MB \
      --eval '(setq *evaluator-mode* :compile)' \
      --load "$RUNFILES/google3/third_party/lisp/sbcl/src/tools-for-build/editcore" \
      --eval '(sb-editcore:split-core "'$input'" "'$output'")' --quit
    ;;
  copy)
    exec $RUNFILES/google3/third_party/lisp/sbcl/binary-distribution/k8/bin/sbcl \
      --noinform --dynamic-space-size 512MB \
      --eval '(setq *evaluator-mode* :compile)' \
      --load "$RUNFILES/google3/third_party/lisp/sbcl/src/tools-for-build/editcore" \
      --eval '(sb-editcore::copy-to-elf-obj "'$input'" "'$output'")' --quit
    ;;
  *)
    echo Unknown command: $action
    exit 1
esac
