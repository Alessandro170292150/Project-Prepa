import numpy as np
import time as t
import matplotlib.pyplot as plt

def strassen(t1, t2):
    n = t1.shape[0]
    if n == 1:
        return t1 * t2
    if n % 2 != 0:
        # Pad to make size even
        t1 = np.pad(t1, ((0,1),(0,1)), mode='constant')
        t2 = np.pad(t2, ((0,1),(0,1)), mode='constant')
        n += 1

    c = np.zeros((n, n), dtype=int)

    a1, a2 = t1[:n//2, :n//2], t1[:n//2, n//2:]
    a3, a4 = t1[n//2:, :n//2], t1[n//2:, n//2:]
    b1, b2 = t2[:n//2, :n//2], t2[:n//2, n//2:]
    b3, b4 = t2[n//2:, :n//2], t2[n//2:, n//2:]

    m1 = strassen(a1 + a4, b1 + b4)
    m2 = strassen(a2 + a4, b1)
    m3 = strassen(a1, b3 - b4)
    m4 = strassen(a4, b2 - b1)
    m5 = strassen(a1 + a3, b4)
    m6 = strassen(a2 - a1, b1 + b3)
    m7 = strassen(a3 - a4, b2 + b4)

    c[:n//2, :n//2] = m1 + m4 + m7 - m5
    c[n//2:, :n//2] = m3 + m5
    c[:n//2, n//2:] = m2 + m4
    c[n//2:, n//2:] = m1 + m3 + m6 - m2

    return c[:t1.shape[0], :t2.shape[1]]  # Trim padding

def repartition(n, vmax):
    x = np.arange(n)
    time_dot = np.zeros(n)
    time_strassen = np.zeros(n)
    nb_runs = 3  # moyenne sur plusieurs exécutions

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

    # Tracé
    fig, ax = plt.subplots()
    ax.plot(x, time_dot, '-o', label="np.dot (classique)")
    ax.plot(x, time_strassen, '-s', label="Strassen")
    ax.set_xlabel("Puissance (2^n)")
    ax.set_ylabel("Temps (secondes)")
    ax.set_title("Temps de calcul: Strassen vs produit classique")
    ax.legend()
    ax.grid(True)
    plt.show()

repartition(13, 16)