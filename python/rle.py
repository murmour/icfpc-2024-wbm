# U -> 0
# R -> 1
# D -> 2
# L -> 3

def map_char(s: str) -> int:
    if s == "U":
        return 0
    if s == "R":
        return 1
    elif s == "D":
        return 2
    elif s == "L":
        return 3
    raise ValueError("unknown char")


def unmap_char(i: int) -> str:
    if i == 0:
        return "U"
    if i == 1:
        return "R"
    if i == 2:
        return "D"
    if i == 3:
        return "L"
    raise ValueError("unknown char")


def binpow(base: int, power: int) -> int:
    if power == 0:
        return 1

    if power == 1:
        return base

    tmp = binpow(base, power // 2)
    if power % 2 == 0:
        return tmp * tmp
    else:
        return tmp * tmp * base


def encode_block(s: str, block_size: int = 4, char_size: int = 2) -> int:
    if char_size < 1:
        raise ValueError("char_size must be greater than or equal to 1")

    if block_size < char_size:
        raise ValueError("block_size must be greater than or equal to char_size")

    count_size = block_size - char_size
    max_n = binpow(2, count_size) - 1
    char_p = binpow(2, char_size)
    block_p = binpow(2, block_size)

    last_c = s[0]
    last_n = 1
    res = 0

    for c in s[1:]:
        if c == last_c and last_n < max_n:
            last_n += 1
        else:
            last_n *= char_p
            last_n += map_char(last_c)
            res *= block_p
            res += last_n
            last_c = c
            last_n = 1

    last_n *= char_p
    last_n += map_char(last_c)
    res *= block_p
    res += last_n

    return res


def decode_block(i: int, block_size: int = 4, char_size: int = 2) -> str:
    if char_size < 1:
        raise ValueError("char_size must be greater than or equal to 1")

    if block_size < char_size:
        raise ValueError("block_size must be greater than or equal to char_size")

    count_size = block_size - char_size
    char_p = binpow(2, char_size)
    count_p = binpow(2, count_size)

    res = ""
    while i > 0:
        c = i % char_p
        i //= char_p
        n = i % count_p
        i //= count_p
        res += unmap_char(c) * n

    return res[::-1]


if __name__ == "__main__":
    print(encode_block("DDLLRRUURRLLLLUUUUDDDDLLUULLUURRLLDDRRDDRRRRUUUURRRRRRDDRRRRUURRLLDDRRLLLLLLUURRLLLLLLLLDDRRRRDDDDDDLLDDDDDDDDDDUURRDDUURRDDUULLLLUULLDDDDUUUUUULLDDDDDDLLRRUULLLLDDUUUURRUUDDLLDDRRRRUUUUUULLUUUUDDRRLLDDLLDDUUUUUUUUDDDDDDRRRRDDRRDDRRUURRLLUULLUUDDRRUURRUULLRRRRRRLLUURRRRRRLLDDRRDDDDDDDDLLRRUULLRRUUUULLLLRRDDLLLLUUDDLLRRDDRRDDLLLLRRRRDDDDRRRRLLUURR"))
