import sys
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.colors import TABLEAU_COLORS as tableau

matplotlib.use("pgf")
plt.rcParams.update({'font.size': 7})
plt.rcParams.update({'lines.linewidth': 1.0})

max_j = 3 # No of coordinates

with open("CygLyrAql.out", "r") as f:
    max_k = int(f.readline())
    clusters = np.empty((max_k, max_j), dtype=np.float32)
    for k in range(max_k):
        _, _, ra, dec, mag = [float(w) for w in f.readline().split()]
        clusters[k, :] = [ra, dec, mag]
    max_i = int(f.readline())
    data = np.empty((max_i, max_j), dtype=np.float32)
    color = np.empty((max_i), dtype=np.uint8)
    for i in range(max_i):
        ws = f.readline().split()
        ra, dec, mag, col = float(ws[0]), float(ws[1]), float(ws[2]), int(ws[3])
        data[i, :] = [ra, dec, mag]
        color[i] = col

color_names = list(tableau)

fig = plt.figure(figsize=(4.5, 6))
ax = fig.add_axes([0.08, 0.06, 0.92, 0.94])

ax.set_xlabel("right ascension, hours")
ax.set_xlim(18.3, 21.8)
ax.set_ylabel("declination, °")
ax.set_ylim(-9.7, 57.8)

for k in range(max_k):
    mask = color == k + 1
    ax.scatter(data[mask, 0], data[mask, 1], s=(6.5 - data[mask, 2]) * 4.0, color=color_names[k])
    ax.scatter(clusters[k, 0], clusters[k, 1], s=100, marker='*', color=color_names[k], alpha=0.4)

#plt.show()
plt.savefig("CygLyrAql.pdf", backend="pgf", dpi=300)
plt.close(fig)