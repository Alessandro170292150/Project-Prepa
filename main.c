int** add (int** a, int** b, int n) {
    int*** c = malloc(sizeof(int**));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            c[i][j] = a[i][j] + b[i][j];
        }
    }
}

int** strassen (int** a, int** b, int n) {
    int*** c = malloc(sizeof(int**));
    int*** a1 = malloc(sizeof(int**));
    int*** a2 = malloc(sizeof(int**));
    int*** a3 = malloc(sizeof(int**));
    int*** a4 = malloc(sizeof(int**));
    int*** b1 = malloc(sizeof(int**));
    int*** b2 = malloc(sizeof(int**));
    int*** b3 = malloc(sizeof(int**));
    int*** b4 = malloc(sizeof(int**));
    int*** c1 = malloc(sizeof(int**));
    int*** c2 = malloc(sizeof(int**));
    int*** c3 = malloc(sizeof(int**));
    int*** c4 = malloc(sizeof(int**));
    int*** m1 = malloc(sizeof(int**));
    int*** m2 = malloc(sizeof(int**));
    int*** m3 = malloc(sizeof(int**));
    int*** m4 = malloc(sizeof(int**));
    int*** m5 = malloc(sizeof(int**));
    int*** m6 = malloc(sizeof(int**));
    int*** m7 = malloc(sizeof(int**));
    

}