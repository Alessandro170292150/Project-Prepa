import numpy as np
import time as t
import matplotlib.pyplot as plt

def product(t1, t2):
    assert(t1 == [] or t2 == [])
    n = len(t1[0])
    z = 0
    t3 = [[0 for i in range(n)] for j in range(n)]
    for i in range(n):
        for j in range(n):
            for k in range(n):
                z += t1[i][k]*t2[k][j]
            t3[i][j] = z
            z = 0
    return t3

def strassen(t1, t2):
    n = t1.shape[0]
    if n < 32:
        return np.dot(t1, t2)
    if n % 2 != 0:
        t1 = np.pad(t1, ((0,1),(0,1)), mode='constant')
        t2 = np.pad(t2, ((0,1),(0,1)), mode='constant')
        n += 1

    elif not (n & (n - 1)) == 0:
        # On pad pour obtenir la prochaine puissance de 2
        m = 1 << (n - 1).bit_length()
        t1 = np.pad(t1, ((0, m - n), (0, m - n)), mode='constant')
        t2 = np.pad(t2, ((0, m - n), (0, m - n)), mode='constant')
        n = m

    # Découpage en sous-matrices
    a11, a12 = np.split(t1[:n//2, :], 2, axis=1)
    a21, a22 = np.split(t1[n//2:, :], 2, axis=1)
    b11, b12 = np.split(t2[:n//2, :], 2, axis=1)
    b21, b22 = np.split(t2[n//2:, :], 2, axis=1)

    # Calcul des 7 produits de Strassen
    m1 = strassen(a11 + a22, b11 + b22)
    m2 = strassen(a21 + a22, b11)
    m3 = strassen(a11, b12 - b22)
    m4 = strassen(a22, b21 - b11)
    m5 = strassen(a11 + a12, b22)
    m6 = strassen(a21 - a11, b11 + b12)
    m7 = strassen(a12 - a22, b21 + b22)

    c11 = m1 + m4 - m5 + m7
    c12 = m3 + m5
    c21 = m2 + m4
    c22 = m1 - m2 + m3 + m6

    top = np.hstack((c11, c12))
    bottom = np.hstack((c21, c22))
    result = np.vstack((top, bottom))

    return result[:t1.shape[0], :t2.shape[1]]

def repartition(n, vmax):
    x = np.arange(n)
    time_dot = np.zeros(n)
    time_strassen = np.zeros(n)
    nb_runs = 3  

    for i in range(n):
        size = 2**i
        avg_dot = 0
        avg_strassen = 0
        for _ in range(nb_runs):
            m = np.random.randint(-vmax, vmax, size=(size, size))
            p = np.random.randint(-vmax, vmax, size=(size, size))

            t1 = t.time()
            np.dot(m, p)
            t2 = t.time()
            avg_dot += (t2 - t1)

            t3 = t.time()
            strassen(m, p)
            t4 = t.time()
            avg_strassen += (t4 - t3)

        time_dot[i] = avg_dot / nb_runs
        time_strassen[i] = avg_strassen / nb_runs
        print(f"2^{i} ({size}x{size}) -> dot: {time_dot[i]:.6f}s, strassen: {time_strassen[i]:.6f}s")
    fig, ax = plt.subplots()
    ax.plot(x, time_dot, '-o', label="np.dot (classique)")
    ax.plot(x, time_strassen, '-s', label="Strassen")
    ax.set_xlabel("Puissance (2^n)")
    ax.set_ylabel("Temps (secondes)")
    ax.set_title("Temps de calcul: Strassen vs produit classique")
    ax.legend()
    ax.grid(True)
    plt.show()

repartition(10, 16)