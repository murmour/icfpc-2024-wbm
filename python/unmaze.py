
import sys


with open(sys.argv[1]) as f:
    maze = [ l.strip() for l in f.readlines() ]

len_y = len(maze)
len_x = len(maze[0])

maze = ['#' * len_x] + maze + ['#' * len_x]
maze = ['#' + l + '#' for l in maze]
maze = [list(l) for l in maze]

for i in range(len_y):
    for j in range(len_x):
        if maze[i][j] == 'L':
            x = j
            y = i
            break

path = sys.stdin.read().strip()
for c in path:
    if c == 'L' and maze[y][x-1] != '#':
        x -= 1
    if c == 'R' and maze[y][x+1] != '#':
        x += 1
    if c == 'U' and maze[y-1][x] != '#':
        y -= 1
    if c == 'D' and maze[y+1][x] != '#':
        y += 1
    maze[y][x] = ' '

maze[y][x] = 'L'

for l in maze:
    print("".join(l))

ct = 0
for l in maze:
    for c in l:
        if c == '.':
            ct += 1
print(f'dots remaining: {ct}')
