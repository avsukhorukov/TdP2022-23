# Converts RGB values from the classified *.out format to the same list as on
# input but with the original data triplets replaced with cluster centroids.
import sys
import numpy as np
from pathlib import Path

max_j = 3 # No of coordinates

args = sys.argv[1:]
if not args:
    print("Usage:\n\tpython3 to_centroids.py <file.out>")
    sys.exit(1)
out_filename = args[0]
txt_filename = Path(out_filename).stem + '.txt'

with open(out_filename, "r") as f:
    max_k = int(f.readline())
    clusters = np.empty((max_k, max_j), dtype=np.int32)
    for k in range(max_k):
        _, _, r, g, b = [int(w) for w in f.readline().split()]
        clusters[k, :] = [r, g, b]
    max_i = int(f.readline())
    data = np.empty((max_i, max_j), dtype=np.int32)
    for i in range(max_i):
        r, g, b, k = [int(w) for w in f.readline().split()]
        data[i, :] = clusters[k-1, :]

with open(txt_filename, 'w') as f:
    for i in range(max_i):
        f.write("{:3d} {:3d} {:3d}\n".format(*data[i, :]))

