#/usr/bin/python3
import sys
import numpy as np
import matplotlib.pyplot as plt

try:
    out_name = sys.argv[1]
except IndexError:
    print('Output file out_name must be provided')
    out_name = input('file.out=')

# Read the number of bodies from the first line.
with open(out_name) as f:
    n = int(f.readline())

# Read the coordinates from the remaining part.
#xy = np.loadtxt("3body.out", skiprows=1)
xy = np.loadtxt(out_name, skiprows=1)

nt = xy.shape[0] // n

x = np.empty((nt, n))
y = np.empty((nt, n))

for ib in range(n):
    x[:, ib] = xy[ib:ib + nt * n:n, 0]
    y[:, ib] = xy[ib:ib + nt * n:n, 1]

xmin, xmax = np.amin(x), np.amax(x)
ymin, ymax = np.amin(y), np.amax(y)

fig = plt.figure(figsize=(6, 6), dpi=96)
ax = fig.add_axes([0.1, 0.1, 0.89, 0.89])
#fig, ax = plt.subplots()
#ax.set_aspect('equal')
ax.set_xlim((xmin, xmax))
ax.set_ylim((ymin, ymax))
for it in range(nt):
    ax.cla()
    ax.scatter(x[max(0, it - 2):it, :], y[max(0, it - 2):it, :], s=3, color='0.7')
    ax.scatter(x[it, :], y[it, :], s=5, color='0')
    ax.set_xlim((xmin, xmax))
    ax.set_ylim((ymin, ymax))
    #ax.set_xlim((-10, 10))
    #ax.set_ylim((-10, 10))
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    plt.pause(0.05)
