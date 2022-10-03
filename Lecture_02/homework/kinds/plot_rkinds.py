#/usr/bin/python3
import numpy as np
#import matplotlib
import matplotlib.pyplot as plt

#matplotlib.use("qt5agg")

ranges, precisions, kinds = np.loadtxt("rkinds.txt", dtype=np.int32, unpack=True)
unique_kinds = np.unique(kinds)

for kind in np.unique(kinds)[::-1]:
    plt.scatter(ranges[kinds == kind], precisions[kinds == kind], label="kind {}".format(kind), s=9)
plt.legend(loc="upper center", ncol=2, scatterpoints=1, framealpha=1.0)
#plt.xscale('symlog', linthresh=300)
plt.xlabel("exponent range")
plt.ylabel("precision")
plt.tight_layout()
plt.show()
#plt.savefig("real_kinds_ifort.png", dpi=300)
#plt.savefig("real_kinds_gfortran.png", dpi=300)