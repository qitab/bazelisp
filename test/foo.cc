// Copyright 2015-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include <iostream>

extern "C" {
  void lisp_test_function() {
    std::cout << "FOO" << std::endl;
  }
}
