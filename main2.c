#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <time.h>
#include <unistd.h> 

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
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] + b[i][j];
        }
    }
}



void sous (int n, int **a, int **b, int **c) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] - b[i][j];
        }
    }
}


void product (int n, int **a, int **b,int **c) {
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

void strassen (int n, int **a, int **b, int **c) {
    if (n == 1) {
        product(1, a, b, c);
        return;
    }
    int **a1 = allocate_matrix(n/2);
    int **a2 = allocate_matrix(n/2);
    int **a3 = allocate_matrix(n/2);
    int **a4 = allocate_matrix(n/2);
    int **b1 = allocate_matrix(n/2);
    int **b2 = allocate_matrix(n/2);
    int **b3 = allocate_matrix(n/2);
    int **b4 = allocate_matrix(n/2);
    int **c1 = allocate_matrix(n/2);
    int **c2 = allocate_matrix(n/2);
    int **c3 = allocate_matrix(n/2);
    int **c4 = allocate_matrix(n/2);
    int **m1 = allocate_matrix(n/2);
    int **m2 = allocate_matrix(n/2);
    int **m3 = allocate_matrix(n/2);
    int **m4 = allocate_matrix(n/2);
    int **m5 = allocate_matrix(n/2);
    int **m6 = allocate_matrix(n/2);
    int **m7 = allocate_matrix(n/2);
    int **calc1 = allocate_matrix(n/2);
    int **calc2 = allocate_matrix(n/2);
    int **zero = allocate_matrix(n/2);
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
            zero[i][j] = 0;
        }
    }
    add((n/2), a1, a4, calc1);
    add((n/2), b1, b4, calc2);
    strassen ((n/2), calc1, calc2, m1);
    reset((n/2), calc1, calc2, zero);
    add((n/2), a2, a4, calc1);
    strassen ((n/2), calc1, b1, m2);
    reset((n/2),calc1, calc2, zero);
    sous ((n/2), b3, b4, calc1);
    strassen ((n/2), a1, calc1, m3);
    reset((n/2),calc1, calc2, zero);
    sous ((n/2), b2, b1, calc1);
    strassen ((n/2), a4, calc1, m4);
    reset((n/2),calc1, calc2, zero);
    add ((n/2), a1, a3, calc1);
    strassen ((n/2), calc1, b4, m5);
    reset((n/2),calc1, calc2, zero);
    add ((n/2), b1, b3, calc1);
    sous ((n/2), a2 ,a1, calc2);
    strassen ((n/2), calc2, calc1, m6);
    reset((n/2),calc1, calc2, zero);
    sous ((n/2), a3, a4, calc1);
    add ((n/2), b2, b4, calc2);
    strassen ((n/2), calc1, calc2, m7);
    reset((n/2),calc1, calc2, zero);
    add ((n/2), m1, m4, calc1);
    add ((n/2), calc1, m7, calc2);
    sous ((n/2), calc2, m5, c1);
    reset((n/2),calc1, calc2, zero);
    add ((n/2), m3, m5, c2);
    add ((n/2), m2, m4, c3);
    add ((n/2), m1 ,m3, calc1);
    add ((n/2), calc1, m6, calc2);
    sous ((n/2), calc2, m2, c4);
    for (int i = 0; i < (n/2); i++) {
        for (int j = 0; j < (n/2); j++) {
            c[i][j] = c1[i][j];
            c[i+(n/2)][j] = c2[i][j];
            c[i][j+(n/2)] = c3[i][j];
            c[i+(n/2)][j+(n/2)] = c4[i][j];
        }
    }
    free_matrix((n/2), a1);
    free_matrix((n/2), a2);
    free_matrix((n/2), a3);
    free_matrix((n/2), a4);
    free_matrix((n/2), b1);
    free_matrix((n/2), b2);
    free_matrix((n/2), b3);
    free_matrix((n/2), b4);
    free_matrix((n/2), c1);
    free_matrix((n/2), c2);
    free_matrix((n/2), c3);
    free_matrix((n/2), c4);
    free_matrix((n/2), m1);
    free_matrix((n/2), m2);
    free_matrix((n/2), m3);
    free_matrix((n/2), m4);
    free_matrix((n/2), m5);
    free_matrix((n/2), m6);
    free_matrix((n/2), m7);
    free_matrix((n/2), zero);
    return;
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
    int n = 12;
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
        time_strassen[i] = elapsed_ms_strassen;
    
        // Mesure du temps pour produit classique
        clock_gettime(CLOCK_MONOTONIC, &start);
        product(fast_pow(2, i), a, b, d);
        clock_gettime(CLOCK_MONOTONIC, &end);
        seconds = end.tv_sec - start.tv_sec;
        nanoseconds = end.tv_nsec - start.tv_nsec;
        double elapsed_ms_product = seconds * 1000.0 + nanoseconds / 1e6;
        time_product[i] = elapsed_ms_product;
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
    // 3. Appel de gnuplot
    FILE *gnuplot = popen("gnuplot -persistent", "w");
    if (!gnuplot) {
        perror("Erreur lors de l'ouverture de gnuplot");
        return 1;
    }
            // Commande gnuplot : tracer à partir du fichier
    fprintf(gnuplot, "set title 'Temps de calcul : Strassen vs produit classique'\n");
    fprintf(gnuplot, "set xlabel 'Taille de la matrice (n)'\n");
    fprintf(gnuplot, "set ylabel 'Temps (ms)'\n");
    fprintf(gnuplot, "plot 'data.txt' using 1:2 with linespoints title 'Strassen', \\\n");
    fprintf(gnuplot, "     'data.txt' using 1:3 with linespoints title 'Produit classique'\n");


    pclose(gnuplot);
    // Affichage des résultats
    // printf("Temps pour Strassen : %.6f ms\n", elapsed_ms_strassen);
    // printf("Temps pour produit classique : %.6f ms\n", elapsed_ms_product);
    // print_m(n, c);
    // printf("\n");
    // print_m(n, d);
    return 0;
}