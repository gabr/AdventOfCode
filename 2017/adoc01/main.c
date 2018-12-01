#include <stdio.h>
#include <string.h>

int part1(char* input) {
    int index = 0;
    int sum = 0;

    while (input[index] != '\0') {
        int a = input[index] - '0';
        int b = 0;

        if (input[index + 1] != '\0') {
            b = input[index + 1] - '0';
        } else {
            b = input[0] - '0';
        }

        if (a == b) {
            sum += a;
        }

        ++index;
    }

    return sum;
}

int part2(char* input) {
    int length = strlen(input);
    if (length % 2 != 0) {
        fprintf(stderr, "Input length is not event!");
        return(-1);
    }

    int step = length / 2;

    int index = 0;
    int sum = 0;

    int stepTmp = step;

    while (input[index] != '\0') {
        stepTmp = index + step;
        while (stepTmp >= length) {
            stepTmp -= length;
        }

        int a = input[index] - '0';
        int b = input[stepTmp] - '0';

        //printf("a[%d]: %d, b[%d]: %d\n",
        //    index, a,
        //    stepTmp, b);

        if (a == b) {
            sum += a;
        }

        ++index;
    }

    return sum;
}

int main(int argc, char** argv) {

    //printf("Arguments count: %d\n", argc);
    //printf("Arguments:\n");
    //for (int i = 0; i < argc; i++) {
    //    printf("  %d: %s\n", i, argv[i]);
    //}

    if (argc < 2) {
        return(1);
    }

    if (argc <= 2 || argv[2][0] == '1') {
        printf("%d\n", part1(argv[1]));
    }

    if (argc <= 2 || argv[2][0] == '2') {
        printf("%d\n", part2(argv[1]));
    }

    return(0);
}
