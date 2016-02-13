#include <iostream>

extern "C" {
  void lisp_test_function() {
    std::cout << "FOO" << std::endl;
  }
}
