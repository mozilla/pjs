#include <stdio.h>
#include <string.h>
#include "pjs.h"

int main(int argc, const char *argv[])
{
    if (argc == 1) {
        fprintf(stderr, "Usage: %s script.js\n", argv[0]);
        return 1;
    } else {
        int i = 1;
        while (i < argc) {
            if (!strcmp(argv[i], "--")) {
                i++;
                break;
            } else if (argv[i][0] == '-') {
                fprintf(stderr, "Unrecognized option: %s", argv[i]);
            } else {
                break;
            }
        }
        for (; i < argc; i++) {
            pjs::init(argv[i]);
        }
        return 0;
    }
}
