# U -> 0
# R -> 1
# D -> 2
# L -> 3
import os
import subprocess

from encode import encode_str, encode_int

CHAR_MAP = {
    "U": 0b00,
    "R": 0b01,
    "D": 0b10,
    "L": 0b11,
}

REV_CHAR_MAP = {v: k for k, v in CHAR_MAP.items()}


def encode_block(s: str, count_size: int = 2, char_size: int = 2) -> int:
    if char_size < 1:
        raise ValueError("char_size must be greater than or equal to 1")

    if count_size < 1:
        raise ValueError("count_size must be greater than or equal to 1")

    block_size = count_size + char_size
    max_n = (1 << count_size) - 1

    last_c = s[0]
    last_n = 1
    res = 0

    for c in s[1:]:
        if c == last_c and last_n < max_n:
            last_n += 1
        else:
            last_n <<= char_size
            last_n += CHAR_MAP[last_c]

            res <<= block_size
            res += last_n

            last_c = c
            last_n = 1

    last_n <<= char_size
    last_n += CHAR_MAP[last_c]

    res <<= block_size
    res += last_n

    return res


def decode_block(i: int, count_size: int = 2, char_size: int = 2) -> str:
    if char_size < 1:
        raise ValueError("char_size must be greater than or equal to 1")

    if count_size < 1:
        raise ValueError("count_size must be greater than or equal to 1")

    char_bits = (1 << char_size) - 1
    count_bits = (1 << count_size) - 1

    res = ""
    while i > 0:
        c = i & char_bits
        i >>= char_size
        n = i & count_bits
        i >>= count_size
        res += REV_CHAR_MAP[c] * n

    return res[::-1]


def bench_encode(s: str) -> tuple[int, str]:
    best = encode_str(s)
    best_size = 0

    for i in range(2, 94):
        enc = encode_block(s, count_size=i)
        base94 = encode_int(enc)
        if len(base94) < len(best):
            best = base94
            best_size = i

    return best_size, best


with open("../../data/out/lambdaman/icfp.tmpl") as tmpl_file:
    PROGRAM_TEMPLATE = tmpl_file.read().strip()


def interact(request):
    return subprocess.check_output(['../kepler/interact', '-i', 'stdin'], input=request.encode())


def solve_lambdaman(n: int, send: bool = True):
    if not os.path.exists(f"../../data/out/lambdaman/{n:02}/{n:02}.amaze"):
        print(f"Skip {n}...")
        return

    print(f"{n}...")

    with open(f"../../data/out/lambdaman/{n:02}/{n:02}.amaze") as solve_file:
        solve = solve_file.read().strip()

    command = f"solve lambdaman{n} "
    size, answer = bench_encode(solve)
    program = PROGRAM_TEMPLATE.format(
            command=encode_str(command),
            answer=answer,
            block_p=encode_int(1 << (2 + size)),
            count_p=encode_int(1 << size)
        )

    with open(f"../../data/out/lambdaman/{n:02}/{n:02}_gen.icfp", "w") as icfp_file:
        icfp_file.write(program)

    print(interact(program).decode())


if __name__ == "__main__":
    for l in range(1, 21):
        solve_lambdaman(l)
