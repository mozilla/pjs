#include <stdio.h>
#include "pjs.h"

int main(int argc, const char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: spork script.js\n");
        return 1;
    } else {
        pjs::init(argv[1]);
        return 0;
    }
}
