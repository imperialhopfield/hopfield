#include "Foo_stub.h"

void py_init(int argc, char *argv[]) {
    hs_init(&argc, &argv);
}

void py_exit() {
    hs_exit();
}

int fact(int n) {
    return hs_fact(n);
}
