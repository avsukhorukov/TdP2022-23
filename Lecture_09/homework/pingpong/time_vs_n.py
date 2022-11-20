#/usr/bin/python3
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import numpy as np

matplotlib.use('tkagg')

# data
#ns = np.asarray([0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 55000, 60000, 65000, 70000, 75000, 80000, 85000, 90000, 95000, 100000])
#T_n = np.asarray([5.20E-01, 6.42E+00, 4.09E+00, 4.71E+00, 5.81E+00, 6.90E+00, 7.93E+00, 8.66E+00, 9.62E+00, 1.07E+01, 1.16E+01, 1.31E+01, 1.48E+01, 1.45E+01, 1.55E+01, 1.68E+01, 1.74E+01, 1.85E+01, 1.99E+01, 2.02E+01, 2.13E+01])

data = np.loadtxt("data.txt")
ns = data[:, 0]
T_n = data[:, 1]

# fit
#xs = np.logspace(-0.1, 4.8)
#ys = 1e-8 * xs**2;

#plt.loglog(xs, ys, '-', label="$10^{-8} n^2$")
#plt.loglog(ns, T_n, 'o', label="$T_n$")
#plt.loglog(ns, t_n, 's', label="$t_n$")
plt.plot(ns, T_n, 'o', label="send-recv")

plt.legend(loc="upper left", ncol=1, scatterpoints=1, framealpha=1.0)
#plt.xscale('symlog', linthresh=300)
plt.xlabel("$n$")
plt.ylabel("time per call, s")
plt.tight_layout()
plt.show()