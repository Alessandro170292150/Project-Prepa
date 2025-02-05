import numpy as np
import time as t
import random as rand
import matplotlib.pyplot as plt


N = 21
x = np.linspace(0, 10, 11)
y = [3.9, 4.4, 10.8, 10.3, 11.2, 13.1, 14.1,  9.9, 13.9, 15.1, 12.5]

# fit a linear curve and estimate its y-values and their error.
a, b = np.polyfit(x, y, deg=1)
y_est = a * x + b
y_err = x.std() * np.sqrt(1/len(x) +
                          (x - x.mean())**2 / np.sum((x - x.mean())**2))

fig, ax = plt.subplots()
ax.plot(x, y_est, '-')
ax.fill_between(x, y_est - y_err, y_est + y_err, alpha=0.2)
ax.plot(x, y, 'o', color='tab:brown')

plt.show()


def regularized_inverse(A, lambda_reg=1e-6):
    n = A.shape[0]
    I = np.eye(n) 
    A_reg = A + lambda_reg * I  
    A_inv_reg = np.linalg.inv(A_reg)  
    return A_inv_reg

A = np.array([[4, 3], [6, 3]], dtype=float)
A_inv_reg = regularized_inverse(A, lambda_reg=1e-6)

print("Matrice A :")
print(A)
print("\nInverse régularisé de A :")
print(A_inv_reg)