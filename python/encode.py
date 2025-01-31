import argparse
import sys

CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
CHARS_MAP = {chr(i + 33): c for i, c in enumerate(CHARS)}
REVERSE_CHARS_MAP = {v: k for k, v in CHARS_MAP.items()}


def decode_str(s: str) -> str:
    res = []
    for char in s:
        if char not in CHARS_MAP:
            raise ValueError(f"Unknown character {char}")
        res.append(CHARS_MAP[char])

    return "".join(res)


def encode_str(s: str) -> str:
    res = []
    for char in s:
        if char not in REVERSE_CHARS_MAP:
            raise ValueError(f"Unknown character {char}")
        res.append(REVERSE_CHARS_MAP[char])

    return "".join(res)


def decode_int(s: str) -> int:
    res = 0
    for char in s:
        dig = ord(char) - 33
        res = res * 94 + dig
    return res


def encode_int(i: int) -> str:
    if i < 0:
        raise ValueError(f"Illegal value: {i}")

    res = []
    while i > 0:
        d = i % 94
        res.append(d + 33)
        i = i // 94

    if len(res) == 0:
        res.append(0)

    res.reverse()
    return "".join(map(chr, res))


#sol = 'DDLLRRUURRLLLLUUUUDDDDLLUULLUURRLLDDRRDDRRRRUUUURRRRRRDDRRRRUURRLLDDRRLLLLLLUURRLLLLLLLLDDRRRRDDDDDDLLDDDDDDDDDDUURRDDUURRDDUULLLLUULLDDDDUUUUUULLDDDDDDLLRRUULLLLDDUUUURRUUDDLLDDRRRRUUUUUULLUUUUDDRRLLDDLLDDUUUUUUUUDDDDDDRRRRDDRRDDRRUURRLLUULLUUDDRRUURRUULLRRRRRRLLUURRRRRRLLDDRRDDDDDDDDLLRRUULLRRUUUULLLLRRDDLLLLUUDDLLRRDDRRDDLLLLRRRRDDDDRRRRLLUURR'
#sol = 'UUDRLRRRLULDU'
sol7 = 'RUUURRRUUUUUUUUULLLLDDDDRUURDDUULDDLLUUUULLLLDDUURRRRDDLDDLUUDDRUURDDRUUUURRRRDDDRRRDDDDDDDDDDDDRRRUUURRUUULLLLRRRRDDDLLDDDRRDDDLLLLLLLLLLLUUURRRUUURRLLLLRRDDDLLLDDDLLLUUULLLUUURRLLLLLDDDLLLUUULLUUURRRRRDDUUUUUUUDDDDDRRLLLLLLLDDDRRDDDLLDDDRRRRRRRRRRLLLLLLLLLLUUURRRRRUUURRRDDDRRRDDDRRRRRRRRRRRRRRUUULLLLLUUUUUULLRRUUUUUUUUUUUUUUUUUUURRRRRDDDDDDDLLLLRRRRUUULLLLRRRRUUUULLLLLLLLLLLDDDDRRRDDDLLLRRRUUURRLLLLLLLLUUUDDDLLLDDDRRRLLLUUULLRRRRRRRRUUUURRRRRRDDDDDDDDDDDDDRRRRRLLLLLLLLDDDLLLLLLLLRRRRRRRRDDDLLLDDDLLLUUULLLUUUUUULLLLLLLLRRRRRUUUUUULLLLLUUUUUUURRRRLLLLDDDDRRRRLLLLDDDRRRRRUUUUUUURRRRRR'

if __name__ == "__main__":
    sys.set_int_max_str_digits((1 << 31) - 1)
    mp = {'D': 0, 'R': 1, 'U': 2, 'L': 3}
    n = 0
    for c in sol7:
        n = 4 * n + mp[c]
    print(n)
    print(encode_int(n))


    #a_s = input()
    # b_s = input()
    #a = decode_str(a_s)
    # b = decode_int(b_s)
    #print(a)
    # print(b)
    # print(a+b)
    # print(decode_str("""=/22%#4j}9/5}3/,6%$},!-"$!-!.Z}7)4(}!}3#/2%}/&}V]^_~"""))
    # print(encode_str("get lambdaman21"))
    # print(encode_int(32))
