import numpy as np
import time as t
import random as rand
import matplotlib.pyplot as plt

def strassen (t1, t2):
    n = t1.shape[0]
    if n == 1:
        return t1 * t2 
    c = np.zeros_like(t1)
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
    c[:(n//2), :(n//2)] = m1 + m4 + m7 - m5
    c[(n//2):, :(n//2)] = m3 + m5
    c[:(n//2), (n//2):] = m2 + m4
    c[(n//2):, (n//2):] = m1 + m3 + m6 - m2
    return c

def repartition (n, vmax):
    x = np.linspace(0, n, num = n)
    y = np.zeros_like(x)
    for i in range(n):
        m = np.random.randint(-abs(vmax), abs(vmax), size = (2**int(x[i]), 2**int(x[i])))
        p = np.random.randint(-abs(vmax), abs(vmax), size = (2**int(x[i]), 2**int(x[i])))
        t3 = t.time()
        m2 = np.dot(m, p)
        t4 = t.time()
        dt2 = t4 - t3
        y[i] = dt2
        print(y[i])
    a, b = np.polyfit(x, y, deg = 1)
    y_est = a * x + b
    y_err = x.std() * np.sqrt(1/len(x) + (x - x.mean())**2 / np.sum((x - x.mean())**2))
    fig, ax = plt.subplots()
    ax.plot(x, y_est, '-')
    ax.fill_between(x, y_est - y_err, y_est + y_err, alpha=0.2)
    ax.plot(x, y, 'o', color='tab:brown')
    plt.show()



repartition(12, 2*10)

