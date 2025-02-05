#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <time.h>
#include <unistd.h>

int** add(int** a, int** b, int n){
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
    if (n == 0) {
        return product (a, b, n);
    }
    if (n == 1) {
        return product(a, b, n);
    }
    int** c = malloc(n*sizeof(int*));
    for (int i = 0; i < n; i++) {
        c[i] = malloc(n*sizeof(int));
    }
    printf("%d ", n);
    int** a1 = malloc(n*sizeof(int*));
    int** a2 = malloc(n*sizeof(int*));
    int** a3 = malloc(n*sizeof(int*));
    int** a4 = malloc(n*sizeof(int*));
    int** b1 = malloc(n*sizeof(int*));
    int** b2 = malloc(n*sizeof(int*));
    int** b3 = malloc(n*sizeof(int*));
    int** b4 = malloc(n*sizeof(int*));
    int** c1 = malloc(n*sizeof(int*));
    int** c2 = malloc(n*sizeof(int*));
    int** c3 = malloc(n*sizeof(int*));
    int** c4 = malloc(n*sizeof(int*));
    int** m1 = malloc(n*sizeof(int*));
    int** m2 = malloc(n*sizeof(int*));
    int** m3 = malloc(n*sizeof(int*));
    int** m4 = malloc(n*sizeof(int*));
    int** m5 = malloc(n*sizeof(int*));
    int** m6 = malloc(n*sizeof(int*));
    int** m7 = malloc(n*sizeof(int*));
    for (int i = 0; i < n; i++) {
        a1[i] = malloc(n*sizeof(int));
        a2[i] = malloc(n*sizeof(int));
        a3[i] = malloc(n*sizeof(int));
        a4[i] = malloc(n*sizeof(int));
        b1[i] = malloc(n*sizeof(int));
        b2[i] = malloc(n*sizeof(int));
        b3[i] = malloc(n*sizeof(int));
        b4[i] = malloc(n*sizeof(int));
        c1[i] = malloc(n*sizeof(int));
        c2[i] = malloc(n*sizeof(int));
        c3[i] = malloc(n*sizeof(int));
        c4[i] = malloc(n*sizeof(int));
        m1[i] = malloc(n*sizeof(int));
        m2[i] = malloc(n*sizeof(int));
        m3[i] = malloc(n*sizeof(int));
        m4[i] = malloc(n*sizeof(int));
        m5[i] = malloc(n*sizeof(int));
        m6[i] = malloc(n*sizeof(int));
        m7[i] = malloc(n*sizeof(int));
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = 0;
        } 
    }      
    for (int i = 0; i < (n/2); i++) {
        for (int j = 0; j < (n/2); j++) {
            a1[i][j] = a[i][j];
            a2[i][j] = a[i+(n/2)][j];
            a3[i][j] = a[i][j+(n/2)];
            a4[i][j] = a[(n/2)+i][(n/2)+j];
            b1[i][j] = b[i][j];
            b2[i][j] = b[i+(n/2)][j];
            b3[i][j] = b[i][j+(n/2)];
            b4[i][j] = b[i+(n/2)][j+(n/2)];
        }
    }
    m1 = strassen (add(a1, a4, (n/2)) , add(b1, b4, (n/2)), (n/2));
    m2 = strassen (add(a2, a4, (n/2)), b1, (n/2));
    m3 = strassen (a1, sous (b3, b4, (n/2)), (n/2));
    m4 = strassen (a4, sous (b2, b1, (n/2)), (n/2));
    m5 = strassen (add (a1, a3, (n/2)), b4, (n/2));
    m6 = strassen  (sous (a2 ,a1, (n/2)), add (b1, b3, (n/2)), (n/2));
    m7 = strassen (sous (a3, a4, (n/2)), add (b2, b4, (n/2)), (n/2));
    c1 = sous (add (add (m1, m4, (n/2)), m7, (n/2)), m5, (n/2));
    c2 = add (m3, m5, (n/2));
    c3 = add (m2, m4, (n/2));
    c4 = sous (add (add (m1 ,m3, (n/2)), m6, (n/2)), m2, (n/2));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = c1[i][j];
            c[i+(n/2)][j] = c2[i][j];
            c[i][j+(n/2)] = c3[i][j];
            c[i+(n/2)][j+(n/2)] = c4[i][j];
        }
    }
    for (int i = 0; i < n; i++) {
        free(a1[i]);
        free(a2[i]);
        free(a3[i]);
        free(a4[i]);
        free(b1[i]);
        free(b2[i]);
        free(b3[i]);
        free(b4[i]);
        free(c1[i]);
        free(c2[i]);
        free(c3[i]);
        free(c4[i]);
        free(m1[i]);
        free(m2[i]);
        free(m3[i]);
        free(m4[i]);
        free(m5[i]);
        free(m6[i]);
        free(m7[i]);
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
        printf("{ ");
        for (int j = 0; j < n; j++) {
            printf (" %d ", a[i][j]);
        }
        printf(" }");
        printf("\n");
    }
}
int main(int argc, char * argv[]) {
    int n = 8;
    int** a = malloc(n*sizeof(int*));
    int** b = malloc(n*sizeof(int*));
    int** c = malloc(n*sizeof(int*));
    int** d = malloc(n*sizeof(int*));
    for (int i = 0; i < n; i++) {
        a[i] = malloc(n*sizeof(int));
        b[i] = malloc(n*sizeof(int));
        c[i] = malloc(n*sizeof(int));
        d[i] = malloc(n*sizeof(int));
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == j) {
                a[i][j] = 1;
                b[i][j] = i*j + 1;
            }
            else {
                a[i][j] = 0;
                b[i][j] = 0;
            }
        }
    }
    time_t t1 = time(NULL);
    c = strassen(a, b, n);
    time_t t2 = time(NULL);
    d = product(a, b, n);
    time_t t3 = time(NULL);
    unsigned long dt1 = (unsigned long) difftime(t1, t2);
    unsigned long dt2 = (unsigned long) difftime(t2, t3);
    printf("%ld pour le produit \n", dt1);
    printf("%ld pour strassen \n", dt2);
    print_m(c, n);
    print_m(d, n);
    return 0;
}