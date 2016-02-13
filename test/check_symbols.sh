#!/bin/bash

# TODO(czak): Need to provide a better path here.
if ! objdump -t $TEST_SRCDIR/external/lisp__bazel/test/test |
  grep -q '  common_lisp_user\.test_undefined_function_2\$e\$$'; then
  echo "Failed to find expected lisp symbol in symbol table"
  exit 1
fi

