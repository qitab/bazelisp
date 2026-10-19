# Copyright 2015-2020 Google LLC
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.

# Copyright 2015 Google Inc.  All rights reserved.
# Author: andrzejwalczak@google.com (Andrzej Walczak)
# Description:
# Test directory for the Lisp Starlark build rules.

load(
    "//:rules.bzl",
    "lisp_binary",
    "lisp_library",
    "lisp_test",
)

licenses(["notice"])

lisp_library(
    name = "foo",
    srcs = ["foo.lisp"],
    cdeps = [":foo-ci"],
    compile_data = ["foo.data"],
    data = ["foo.data"],
    deps = [
    ],
)

lisp_library(
    name = "bar",
    srcs = [
        "bar.lisp",
        "bar2.lisp",
    ],
    deps = [":foo"],
)

cc_library(
    name = "foo-ci",
    srcs = ["foo.cc"],
)

lisp_binary(
    name = "baz",
    srcs = ["baz.lisp"],
    deps = [":bar"],
)

lisp_test(
    name = "foo-data-test",
    main = "cl-user::foo-data-test",
    deps = [":bar"],
)

lisp_test(
    name = "minimal",
    main = "values",
)

lisp_test(
    name = "minimal-allow-save-lisp",
    allow_save_lisp = True,
    main = "values",
)

lisp_library(
    name = "lib1",
    srcs = [
        # Ordered
        "a.lisp",
        "b.lisp",
        "c.lisp",
        "d.lisp",
    ],
)

lisp_library(
    name = "lib2",
    srcs = ["test.lisp"],
    nowarn = ["optional-and-key"],
    deps = [":lib1"],
)

lisp_library(
    name = "e",
    srcs = ["e.lisp"],
    deps = [":lib1"],
)

lisp_binary(
    name = "test",
    save_runtime_options = False,
    deps = [
        ":e",
        ":lib1",
        ":lib2",
    ],
)

sh_test(
    name = "check_symbols",
    srcs = ["check_symbols.sh"],
    data = ["test"],
    tags = ["nozapfhahn"],
)

sh_test(
    name = "run-test",
    srcs = ["test"],
)

cc_library(
    name = "empty",
    srcs = ["empty.cc"],
)

lisp_binary(
    name = "lisp-test-image",
    allow_save_lisp = True,
    cdeps = [":empty"],
    main = None,
    deps = [
        # Sequential
        ":lib2",
        ":e",
    ],
)

lisp_test(
    name = "lisp-test",
    size = "small",
    srcs = ["empty.lisp"],
    image = ":lisp-test-image",
)

lisp_test(
    name = "args-test",
    srcs = ["args-test.lisp"],
    args = ["--foo"],
)

lisp_library(
    name = "features",
    add_features = ["features"],
)

lisp_test(
    name = "features-test",
    srcs = ["features-test.lisp"],
    add_features = ["features-test"],
    deps = [":features"],
)

lisp_test(
    name = "main-test",
    srcs = ["main-test.lisp"],
)

# For demonstration purposes.
lisp_test(
    name = "compilation-mode",
    srcs = ["compilation-mode.lisp"],
)
