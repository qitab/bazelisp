#!/bin/bash

# TODO(czak): Need to provide a better path here.
if ! nm -g $TEST_SRCDIR/google3/third_party/lisp/bazel/test/test |
  grep -q ' common-lisp-user::test-undefined-function-2'; then
  echo "Failed to find expected lisp symbol in symbol table"
  exit 1
fi
