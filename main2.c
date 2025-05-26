#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <time.h>
#include <unistd.h> 
#include <math.h> 
#include <omp.h>

int **allocate_matrix(int n) {
    int **a = malloc(n * sizeof(int *));
    for (int i = 0; i < n; i++) {
        a[i] = malloc(n * sizeof(int));
    }
    return a;
}

void free_matrix(int n, int **a) {
    for (int i = 0; i < n; i++) {
        free(a[i]);
    }
    free(a);
}

void add (int n, int **a, int **b, int **c){
    #pragma omp parallel for collapse(2)
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] + b[i][j];
        }
    }
}



void sous (int n, int **a, int **b, int **c) {
    #pragma omp parallel for collapse(2)
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] - b[i][j];
        }
    }
}


void product (int n, int **a, int **b,int **c) {
    int z = 0;
    #pragma omp parallel for collapse(2)
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            for (int k = 0; k < n; k++) {
                z += a[i][k] * b[k][j];
            }
            c[i][j] = z;
            z = 0;
        }
    }
}

void reset(int n, int **a, int **b, int **c) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            a[i][j] = c[i][j];
            b[i][j] = c[i][j];
        }
    }
}


void zero_m(int n, int **a) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n;  j++) {
            a[i][j] = 0;
        }
    }
    return;
}

void id(int n, int **a) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == j) {a[i][j] = 1;}
            else {a[i][j] = 0;}
        }
    }
}

void inversion(int n, int iterations, int **a, int **c) {
    int** i = allocate_matrix(n);
    id(n, i);
    int** r = allocate_matrix(n);
    add(n, a, i, r);
    int** p = allocate_matrix(n);
    for (int i = 0; i < iterations; i++) {
        product(n, p, r, p);
        add(n, c, p, c);
    }
    free_matrix(n, i);
    free_matrix(n, r);
    free_matrix(n, p);    
}

void strassen(int n, int **a, int **b, int **c) {
    if (n <= 32) {
        product(n, a, b, c);
        return;
    }

    int m = n / 2;
    int **a1 = allocate_matrix(m), **a2 = allocate_matrix(m), **a3 = allocate_matrix(m), **a4 = allocate_matrix(m);
    int **b1 = allocate_matrix(m), **b2 = allocate_matrix(m), **b3 = allocate_matrix(m), **b4 = allocate_matrix(m);
    int **c1 = allocate_matrix(m), **c2 = allocate_matrix(m), **c3 = allocate_matrix(m), **c4 = allocate_matrix(m);
    int **m1 = allocate_matrix(m), **m2 = allocate_matrix(m), **m3 = allocate_matrix(m), **m4 = allocate_matrix(m);
    int **m5 = allocate_matrix(m), **m6 = allocate_matrix(m), **m7 = allocate_matrix(m);
    int **calc1 = allocate_matrix(m), **calc2 = allocate_matrix(m), **zero = allocate_matrix(m);

    for (int i = 0; i < m; i++) {
        for (int j = 0; j < m; j++) {
            a1[i][j] = a[i][j];
            a2[i][j] = a[i + m][j];
            a3[i][j] = a[i][j + m];
            a4[i][j] = a[i + m][j + m];
            b1[i][j] = b[i][j];
            b2[i][j] = b[i + m][j];
            b3[i][j] = b[i][j + m];
            b4[i][j] = b[i + m][j + m];
            zero[i][j] = 0;
        }
    }

    // Parallélisation des appels récursifs
    #pragma omp parallel
    #pragma omp single
    {
        add(m, a1, a4, calc1); add(m, b1, b4, calc2);
        #pragma omp task
        strassen(m, calc1, calc2, m1);
        reset(m, calc1, calc2, zero);

        add(m, a2, a4, calc1);
        #pragma omp task
        strassen(m, calc1, b1, m2);
        reset(m, calc1, calc2, zero);

        sous(m, b3, b4, calc1);
        #pragma omp task
        strassen(m, a1, calc1, m3);
        reset(m, calc1, calc2, zero);

        sous(m, b2, b1, calc1);
        #pragma omp task
        strassen(m, a4, calc1, m4);
        reset(m, calc1, calc2, zero);

        add(m, a1, a3, calc1);
        #pragma omp task
        strassen(m, calc1, b4, m5);
        reset(m, calc1, calc2, zero);

        add(m, b1, b3, calc1); sous(m, a2, a1, calc2);
        #pragma omp task
        strassen(m, calc2, calc1, m6);
        reset(m, calc1, calc2, zero);

        sous(m, a3, a4, calc1); add(m, b2, b4, calc2);
        #pragma omp task
        strassen(m, calc1, calc2, m7);
        reset(m, calc1, calc2, zero);

        #pragma omp taskwait
    }

    // Combinaison des sous-matrices
    add(m, m1, m4, calc1); add(m, calc1, m7, calc2); sous(m, calc2, m5, c1);
    add(m, m3, m5, c2);
    add(m, m2, m4, c3);
    add(m, m1, m3, calc1); add(m, calc1, m6, calc2); sous(m, calc2, m2, c4);

    for (int i = 0; i < m; i++) {
        for (int j = 0; j < m; j++) {
            c[i][j] = c1[i][j];
            c[i + m][j] = c2[i][j];
            c[i][j + m] = c3[i][j];
            c[i + m][j + m] = c4[i][j];
        }
    }

    // Libération mémoire
    free_matrix(m, a1); free_matrix(m, a2); free_matrix(m, a3); free_matrix(m, a4);
    free_matrix(m, b1); free_matrix(m, b2); free_matrix(m, b3); free_matrix(m, b4);
    free_matrix(m, c1); free_matrix(m, c2); free_matrix(m, c3); free_matrix(m, c4);
    free_matrix(m, m1); free_matrix(m, m2); free_matrix(m, m3); free_matrix(m, m4);
    free_matrix(m, m5); free_matrix(m, m6); free_matrix(m, m7);
    free_matrix(m, calc1); free_matrix(m, calc2); free_matrix(m, zero);
}

void print_m(int n, int **a) {
    for (int i = 0; i < n; i++) {
        printf("{ ");
        for (int j = 0; j < n; j++) {
            printf (" %d ", a[i][j]);
        }
        printf(" }");
        printf("\n");
    }
}

int fast_pow(int base, int exponent) {
    int result = 1;
    while (exponent > 0) {
        if (exponent % 2 == 1) {
            result *= base;
        }
        base *= base;
        exponent /= 2;
    }
    return result;
}
int main() {
    int n = 15;
    int *val = malloc(n*sizeof(int));
    for (int i = 0; i < n; i++) {
        val[i] = i;
    }
    double *time_strassen = malloc(n * sizeof(double));
    double *time_product = malloc(n * sizeof(double));
    for (int i = 0; i < n; i++) {
        int size = fast_pow(2, i);
        int **a = allocate_matrix(size);
        int **b = allocate_matrix(size);
        int **c = allocate_matrix(size);
        int **d = allocate_matrix(size);
        for (int j = 0; j < size; j++) {
            for (int k = 0; k < size; k++) {
                a[j][k] = (j == k) ? 1 : 0;
                b[j][k] = (j == k) ? j * k + 1 : 0;
            }
        }
        struct timespec start, end;
        clock_gettime(CLOCK_MONOTONIC, &start);
        strassen(fast_pow(2, i), a, b, c);
        clock_gettime(CLOCK_MONOTONIC, &end);
        long seconds = end.tv_sec - start.tv_sec;
        long nanoseconds = end.tv_nsec - start.tv_nsec;
        double elapsed_ms_strassen = seconds * 1000.0 + nanoseconds / 1e6;
        printf("%f\n", elapsed_ms_strassen);
        time_strassen[i] = log(elapsed_ms_strassen);
    
        // Mesure du temps pour produit classique
        clock_gettime(CLOCK_MONOTONIC, &start);
        product(fast_pow(2, i), a, b, d);
        clock_gettime(CLOCK_MONOTONIC, &end);
        seconds = end.tv_sec - start.tv_sec;
        nanoseconds = end.tv_nsec - start.tv_nsec;
        double elapsed_ms_product = seconds * 1000.0 + nanoseconds / 1e6;
        printf("%f\n", elapsed_ms_product);
        time_product[i] = log(elapsed_ms_product);
        free_matrix(size, a);
        free_matrix(size, b);
        free_matrix(size, c);
        free_matrix(size, d);
    }
    FILE *data = fopen("data.txt", "w");
    if (!data) {
        perror("Erreur lors de la création du fichier data.txt");
        return 1;
    }

    for (int i = 0; i < n; i++) {
        fprintf(data, "%d %lf %lf\n", val[i], time_strassen[i], time_product[i]);
    }

    fclose(data);
    FILE *gnuplot = popen("gnuplot -persistent", "w");
    if (!gnuplot) {
        perror("Erreur lors de l'ouverture de gnuplot");
        return 1;
    }
    fprintf(gnuplot, "set title 'Temps de calcul : Strassen vs produit classique'\n");
    fprintf(gnuplot, "set xlabel 'Taille de la matrice (n)'\n");
    fprintf(gnuplot, "set ylabel 'Temps (ms)'\n");
    // fprintf(gnuplot, "set logscale y\n");  
    fprintf(gnuplot, "plot 'data.txt' using 1:2 with linespoints title 'Strassen', \\\n");
    fprintf(gnuplot, "     'data.txt' using 1:3 with linespoints title 'Produit classique'\n");
    pclose(gnuplot);
    return 0;
}