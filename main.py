import numpy as np
import time as t

def strassen (t1, t2):
    n = np.size(t1)
    c = np.zero(n)
    assert (np.size(t2) != n)
    a1 = np.zero(n//2)
    a2 = np.zero(n//2)
    a3 = np.zero(n//2)
    a4 = np.zero(n//2)
    b1 = np.zero(n//2)
    b2 = np.zero(n//2)
    b3 = np.zero(n//2)
    b4 = np.zero(n//2)
    m1 = np.zero(n//2)
    m2 = np.zero(n//2)
    m3 = np.zero(n//2)
    m4 = np.zero(n//2)
    m5 = np.zero(n//2)
    m6 = np.zero(n//2)
    m7 = np.zero(n//2)
    for i in range(n//2):
        a1