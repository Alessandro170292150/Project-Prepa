#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>
#include <stddef.h>


int** add (int** a, int** b, int n) {
    int** c = malloc(n*sizeof(int*));
    for (int i = 0; i < n; i++) {
        c[i] = malloc(n*sizeof(int));
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] + b[i][j];
        }
    }
    return c;
}

int** sous (int** a, int** b, int n) {
    int** c = malloc(n*sizeof(int*));
    for (int i = 0; i < n; i++) {
        c[i] = malloc(n*sizeof(int));
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] - b[i][j];
        }
    }
    return c;
}


int** product (int** a, int** b, int n) {
    int** c = malloc(n*sizeof(int*));
    for (int i = 0; i < n; i++) {
        c[i] = malloc(n*sizeof(int));
    }
    int z = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            for (int k = 0; k < n; k++) {
                z += a[i][k] * b[k][j];
            }
            c[i][j] = z;
            z = 0;
        }
    }
    return c;
}
int** strassen (int** a, int** b, int n) {
    if (n == 1) {
        return product (a, b, n);
    }
    int** c = malloc(n*sizeof(int*));
    for (int i = 0; i < n; i++) {
        c[i] = malloc(n*sizeof(int));
    }
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
        }
    }
    m1 = strassen (add(a1, a4, n/2) , add(b1, b4, n/2), n/2);
    m2 = strassen (add(a2, a4, n/2), b1, n/2);
    m3 = strassen (a1, sous (b3, b4, n/2), n/2);
    m4 = strassen (a4, sous (b2, b1, n/2), n/2);
    m5 = strassen (add (a1, a3, n/2), b4, n/2);
    m6 = strassen  (sous (a2 ,a1, n/2), add (b1, b3, n/2), n/2);
    m7 = strassen (sous (a3, a4, n/2), add (b2, b4, n/2), n/2);
    c1 = sous (add (add (m1, m4, n/2), m7, n/2), m5, n/2);
    c2 = add (m3, m5, n/2);
    c3 = add (m2, m4, n/2);
    c4 = sous (add (add (m1 ,m3, n/2), m6, n/2), m2, n/2);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = c1[i][j];
            c[i+n/2][j] = c2[i][j];
            c[i][j+n/2] = c3[i][j];
            c[i+n/2][j+n/2] = c4[i][j];
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
void print_m(int **a, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            printf ("%d", a[i][j]);
        }
        printf("\n");
    }
}
int main() {
    int n = 2;
    int** a = malloc(n*n*sizeof(int));
    int** b = malloc(n*n*sizeof(int));
    // int** c = malloc(n*n*sizeof(int));
    int** d = malloc(n*n*sizeof(int));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == j) {
                a[i][j] = 1;
                b[i][j] = i*j;
            }
            else {
                a[i][j] = 0;
                b[i][j] = 0;
            }
        }
    }
    // c = strassen(a, b, 2);
    d = product(a, b, 2);
    // print_m(c, 2);
    print_m(d, 2);
    return 0;
}