#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <time.h>
#include <unistd.h> 

void add (int n, int a[n][n], int b[n][n], int c[n][n]){
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] + b[i][j];
        }
    }
}

void sous (int n, int a[n][n], int b[n][n], int c[n][n]) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] - b[i][j];
        }
    }
}


void product (int n, int a[n][n], int b[n][n],int c[n][n]) {
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

void reset(int n, int a[n][n], int b[n][n], int c[n][n]) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            a[i][j] = c[i][j];
            b[i][j] = c[i][j];
        }
    }
}


void zero_m(int n, int a[n][n]) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n;  j++) {
            a[i][j] = 0;
        }
    }
    return;
}

void strassen (int n, int a[n][n], int b[n][n], int c[n][n]) {
    if (n == 1) {
        product(1, a, b, c);
        return;
    }
    int a1[n/2][n/2];
    int a2[n/2][n/2];
    int a3[n/2][n/2];
    int a4[n/2][n/2];
    int b1[n/2][n/2];
    int b2[n/2][n/2];
    int b3[n/2][n/2];
    int b4[n/2][n/2];
    int c1[n/2][n/2];
    int c2[n/2][n/2];
    int c3[n/2][n/2];
    int c4[n/2][n/2];
    int m1[n/2][n/2];
    int m2[n/2][n/2];
    int m3[n/2][n/2];
    int m4[n/2][n/2];
    int m5[n/2][n/2];
    int m6[n/2][n/2];
    int m7[n/2][n/2];
    int calc1[n/2][n/2]; 
    int calc2[n/2][n/2];
    int zero[n/2][n/2];
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
    return;
}
void print_m(int n, int a[n][n]) {
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
    int n = 7;
    int val[n];
    for (int i = 0; i < n; i++) {
        val[i] = i;
    }
    double time_strassen[n];
    double time_product[n];
    for (int i = 0; i < n; i++) {
        int a[fast_pow(2, i)][fast_pow(2, i)];
        int b[fast_pow(2, i)][fast_pow(2, i)];
        int c[fast_pow(2, i)][fast_pow(2, i)];
        int d[fast_pow(2, i)][fast_pow(2, i)];
        for (int j = 0; j < fast_pow(2, i); j++) {
            for (int k = 0; k < fast_pow(2, i); k++) {
                if (j == k) {
                    a[j][k] = 1;
                    b[j][k] = j*k + 1;
                }
                else {
                    a[j][k] = 0;
                    b[j][k] = 0;
                }
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