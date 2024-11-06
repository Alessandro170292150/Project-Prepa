#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>
#include <stddef.h>

int** add (int** a, int** b, int n) {
    int*** c = malloc(sizeof(int**));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] + b[i][j];
        }
    }
}

int** strassen (int** a, int** b, int n) {
    int*** c = malloc(n*n*sizeof(int));
    int*** a1 = malloc(n*n*sizeof(int));
    int*** a2 = malloc(n*n*sizeof(int));
    int*** a3 = malloc(n*n*sizeof(int));
    int*** a4 = malloc(n*n*sizeof(int));
    int*** b1 = malloc(n*n*sizeof(int));
    int*** b2 = malloc(n*n*sizeof(int));
    int*** b3 = malloc(n*n*sizeof(int));
    int*** b4 = malloc(n*n*sizeof(int));
    int*** c1 = malloc(n*n*sizeof(int));
    int*** c2 = malloc(n*n*sizeof(int));
    int*** c3 = malloc(n*n*sizeof(int));
    int*** c4 = malloc(n*n*sizeof(int));
    int*** m1 = malloc(n*n*sizeof(int));
    int*** m2 = malloc(n*n*sizeof(int));
    int*** m3 = malloc(n*n*sizeof(int));
    int*** m4 = malloc(n*n*sizeof(int));
    int*** m5 = malloc(n*n*sizeof(int));
    int*** m6 = malloc(n*n*sizeof(int));
    int*** m7 = malloc(n*n*sizeof(int));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = 0;
        } 
    }
    for (int i = 0; i < n/2; i++) {
        for (int j = 0; j < n/2; j++) {
            c1[i][j]
        } 
    }
}