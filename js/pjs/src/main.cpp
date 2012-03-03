#include <stdio.h>
#include <string.h>
#include "pjs.h"

int main(int argc, const char *argv[])
{
    if (argc == 1) {
        fprintf(stderr, "Usage: %s [--proxy-arguments|--copy-arguments] script.js\n", argv[0]);
        return 1;
    } else {
        int i = 1;
        bool copyArguments = true;
        while (i < argc) {
            if (!strcmp(argv[i], "--copy-arguments")) {
                i++;
                copyArguments = true;
            } else if (!strcmp(argv[i], "--proxy-arguments")) {
                i++;
                copyArguments = false;
            } else if (!strcmp(argv[i], "--")) {
                i++;
                break;
            } else if (argv[i][0] == '-') {
                fprintf(stderr, "Unrecognized option: %s", argv[i]);
            } else {
                break;
            }
        }
        for (; i < argc; i++) {
            pjs::init(argv[i], copyArguments);
        }
        return 0;
    }
}
