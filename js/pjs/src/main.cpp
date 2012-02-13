#include <stdio.h>
#include "pjs.h"

int main(int argc, const char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s script.js\n", argv[0]);
        return 1;
    } else {
        pjs::init(argv[1]);
        return 0;
    }
}
