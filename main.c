#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>
#include <stddef.h>


int** add (int** a, int** b, int n) {
    int** c = malloc(n*n*sizeof(int));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] + b[i][j];

        }
    }
    return c;
}

int** sous (int** a, int** b, int n) {
    int** c = malloc(n*n*sizeof(int));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] - b[i][j];

        }
    }
    return c;
}


int** product (int** a, int** b, int n) {
    int** c = malloc(n*n*sizeof(int));
    int z = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            for (int k = 0; k < n; k++) {
                z = a[i][k] + b[k][j];
            }
            c[i][j] = z;
            z = 0;
        }
    }
    return c;
}
int** strassen (int** a, int** b, int n) {
    if (n == 1) {
        return product (a b);
    }
    int** c = malloc(n*n*sizeof(int));
    int** a1 = malloc(n*n*sizeof(int));
    int** a2 = malloc(n*n*sizeof(int));
    int** a3 = malloc(n*n*sizeof(int));
    int** a4 = malloc(n*n*sizeof(int));
    int** b1 = malloc(n*n*sizeof(int));
    int** b2 = malloc(n*n*sizeof(int));
    int** b3 = malloc(n*n*sizeof(int));
    int** b4 = malloc(n*n*sizeof(int));
    int** c1 = malloc(n*n*sizeof(int));
    int** c2 = malloc(n*n*sizeof(int));
    int** c3 = malloc(n*n*sizeof(int));
    int** c4 = malloc(n*n*sizeof(int));
    int** m1 = malloc(n*n*sizeof(int));
    int** m2 = malloc(n*n*sizeof(int));
    int** m3 = malloc(n*n*sizeof(int));
    int** m4 = malloc(n*n*sizeof(int));
    int** m5 = malloc(n*n*sizeof(int));
    int** m6 = malloc(n*n*sizeof(int));
    int** m7 = malloc(n*n*sizeof(int));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = 0;
        } 
    }
    for (int i = 0; i < n/2; i++) {
        for (int j = 0; j < n/2; j++) {
            a1[i][j] = a[i][j];
            a2[i][j] = a[i+n/2][j];
            a3[i][j] = a[i][j+n/2];
            a4[i][j] = a[n/2+i][n/2+j];
            b1[i][j] = b[i][j];
            b2[i][j] = b[i+n/2][j];
            b3[i][j] = b[i][j+n/2];
            b4[i][j] = b[i+n/2][j+n/2];
            m1[i][j] = strassen (add(a1, a4, n/2) add(b1, b4, n/2), n/2);
            m2[i][j] = strassen (add(a2, a4, n/2), b1, n/2);
            m3[i][j] = strassen (a1 ,sous (b3, b4, n/2), n/2);
            m4[i][j] = strassen (a4, sous (b2, b1, n/2), n/2);
            m5[i][j] = strassen (add (a1, a3, n/2) b4, n/2);
            m6[i][j] = strassen  (sous (a2 ,a1, n/2), add (b1, b3, n/2), n/2);
            m7[i][j] = strassen (sous (a3, a4, n/2), add (b2, b4, n/2), n/2);
            c1[i][j] = sous (add (add (m1, m4, n/2), m7, n/2), m5, n/2);
            c2[i][j] = add (m3, m5, n/2);
            c3[i][j] = add (m2, m4, n/2);
            c4[i][j] = sous (add (add (m1 ,m3, n/2), m6, n/2), m2, n/2);
            c[i][j] = c1[i][j];
            c[i+n/2][j] = c2[i][j];
            c[i][j+n/2] = c3[i][j];
            c[i+n/2][j+n/2] = c4[i][j];
            free(a1[i][j]);
            free(a2[i][j]);
            free(a3[i][j]);
            free(a4[i][j]);
            free(b1[i][j]);
            free(b2[i][j]);
            free(b3[i][j]);
            free(b4[i][j]);
            free(c1[i][j]);
            free(c2[i][j]);
            free(c3[i][j]);
            free(c4[i][j]);
            free(m1[i][j]);
            free(m2[i][j]);
            free(m3[i][j]);
            free(m4[i][j]);
            free(m5[i][j]);
            free(m6[i][j]);
            free(m7[i][j]);
        }
    }
    free(a1);
    free(a2);
    free(a3);
    free(a4);
    free(b1);
    free(b2);
    free(b3);
    free(b4);
    free(c1);
    free(c2);
    free(c3);
    free(c4);
    free(m1);
    free(m2);
    free(m3);
    free(m4);
    free(m5);
    free(m6);
    free(m7);
    return c;
}

int main() {
    

    return 0;
}