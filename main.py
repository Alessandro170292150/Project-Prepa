import numpy as np
import time as t
import random as rand


def strassen (t1, t2):
    if np.size(t1) == 1:
        return t1 * t2 
    c = np.zero(n)
    a1 = t1.reshape()
    a2 = t1.reshape()
    a3 = t1.reshape()
    a4 = t1.reshape()
    b1 = t2.reshape()
    b2 = t2.reshape()
    b3 = t2.reshape()
    b4 = t2.reshape()
    a1[:, :] = t1[:n//2, :n//2]
    a2[:, :] = t1[(n//2):, :n//2]
    a3[:, :] = t1[:(n//2), (n//2):]
    a4[:, :] = t1[(n//2):, (n//2):]
    b1[:, :] = t2[:n//2, :n//2]
    b2[:, :] = t2[(n//2):, :n//2]
    b3[:, :] = t2[:(n//2), (n//2):]
    b4[:, :] = t2[(n//2):, (n//2):]    
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

def main ():
    m1 = np.fromfunction(rand.randint, (5, 5), dtype = int)
    m2 = np.fromfunction(rand.randint, (5, 5), dtype = int)