import png, io

prob = 5

fname = '../../data/in/lambdaman/%02d.in' % prob

with io.open(fname) as f:
    data = f.readlines()

N = 5
width = N * len(data[0].strip())
height = N * len(data)
img = []
print(data)
print(height, width)
for s in data:
    row = []
    for c in s.strip():
        if c == '#':
            col = (0, 0, 0)
        elif c == '.':
            col = (255, 255, 255)
        elif c == 'L':
            col = (255, 0, 0)
        else:
            print(c)
            assert(False)
        for i in range(N):
            row.extend(col)
    for i in range(N):
        img.append(row)

with open('p%d.png' % prob, 'wb') as f:
    w = png.Writer(width, height, greyscale=False)
    w.write(f, img)