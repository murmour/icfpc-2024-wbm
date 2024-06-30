import matplotlib.pyplot as plt
import io

pid = 5

fname = '../../data/in/spaceship/%02d.in' % pid
xs = []
ys = []
cols = []
with io.open(fname, 'rt') as f:
    for s in f.read().strip().splitlines():
        x, y = s.split()
        xs.append(int(x))
        ys.append(int(y))
    n = len(xs)
    for i in range(n):
        if xs[i] == 0 and ys[i] == 0:
            print('(0, 0) is present!')
        a = i / n
        col = (0, a, 1 - a)
        cols.append(col)

plt.scatter(xs, ys, c=cols)
plt.show()