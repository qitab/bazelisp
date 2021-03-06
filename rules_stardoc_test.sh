#!/bin/bash
#
# Copyright 2015-2020 Google LLC
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.

set -euo pipefail

newly_generated="google3/third_party/lisp/bazel/rules.md"
pending="google3/third_party/lisp/bazel/g3doc/rules.md"

diff_output="${TEST_TMPDIR}/diff_output.txt"
set +e
diff -u -x -w "${TEST_SRCDIR}/${newly_generated}" \
    "${TEST_SRCDIR}/${pending}" >"${diff_output}" 2>&1
diff_status=$?
set -e

if [[ "$diff_status" -ne 0 ]]; then
   echo "Generated ${newly_generated} does not match checked-in ${pending}"
   echo
   echo "Run this to update:"
   echo "bazel build //third_party/lisp/bazel:rules_stardoc"
   echo "cp bazel-bin/third_party/lisp/bazel/rules.md third_party/lisp/bazel/g3doc/"
   echo
   echo "Diff:"
   echo
   cat $diff_output
   echo
   exit 1
fi

exit 0
