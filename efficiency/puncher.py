
import subprocess


input_file = "sudoku.smt2"
punch_file = "punch.z3"


with open(input_file) as f:
    orig = f.read()


def punch(hole: str) -> bool:
    print(hole)
    punched = orig.replace(";; placeholder", hole)
    with open(punch_file, "w") as f:
        f.write(punched)
    try:
        subprocess.check_output(["z3", punch_file])
        return True
    except subprocess.CalledProcessError as ex:
        return False


def gen_hole(var_state) -> str:
    s = "\n"
    for (i, j), n in var_state.items():
        s += f"(assert (= (board {i} {j}) V{n}))\n"
    return s


all_vars = [ (i, j) for i in range(0, 9) for j in range(0, 9) ]
var_state = {}

for v in all_vars:
    for n in range(1, 10):
        var_state[v] = n
        hole = gen_hole(var_state)
        if punch(hole):
            break
        if n == 9:
            print(f'unpunchable! stopped at {v}, {n}')
            exit(1)


print(var_state)


def state_to_num(var_state):
    n = 0
    for i, v in enumerate(reversed(all_vars)):
        n += (9 ** i) * (var_state[v] - 1)
    return n


print(state_to_num(var_state))
