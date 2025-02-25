import numpy as np

type, t1s, t2s = np.genfromtxt("echos.txt").T

for type, t1, t2 in zip(type, t1s, t2s):
    print(type, 0.4 / abs(t2 - t1), "m / s")
