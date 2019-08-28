from itertools import chain, product
from collections import deque
from timeit import default_timer as timer
from gmpy2 import is_prime

def write_int(dest, sz, n):
    dest.write(n.to_bytes(sz, byteorder = 'little'))

def write_vector(dest, elt_sz, v):
    for n in v:
        write_int(dest, elt_sz, n)

def mktests(op, xs, nargs, bits):
    # FIXME: Refactor this.
    if nargs == 1:
        yield zip(*[op(x, bits) for x in xs])
    elif nargs == 2:
        ys = deque(xs)
        for i in range(len(xs)):
            yield zip(*[op(x, y, bits) for x, y in zip(xs, ys)])
            ys.rotate(1)
    elif nargs == 3:
        ys = deque(xs)
        zs = deque(xs)
        for _ in range(len(xs)):
            for _ in range(len(xs)):
                yield list(zip(*[op(x, y, z, bits) for x, y, z in zip(xs, ys, zs)]))
                zs.rotate(1)
            ys.rotate(1)
    elif nargs == 4:
        ys = deque(xs)
        zs = deque(xs)
        ws = deque(xs)
        for _ in range(len(xs)):
            for _ in range(len(xs)):
                for _ in range(len(xs)):
                    yield list(zip(*[op(x, y, z, w, bits) for x, y, z, w in zip(xs, ys, zs, ws)]))
                    ws.rotate(1)
                zs.rotate(1)
            ys.rotate(1)
    else:
        raise NotImplementedError()

def write_tests(fname, arg):
    op, xs, nargs, nres, bits = arg
    vec_len = len(xs)
    ntests = vec_len**nargs
    t = timer()
    print('Writing {} tests into "{}"... '.format(ntests, fname), end='', flush=True)
    with open(fname, 'wb') as f:
        fixnum_bytes = bits >> 3
        write_int(f, 4, fixnum_bytes)
        write_int(f, 4, vec_len)
        write_int(f, 4, nres)
        write_vector(f, fixnum_bytes, xs)
        for v in mktests(op, xs, nargs, bits):
            v = list(v)
            assert len(v) == nres, 'bad result length; expected {}, got {}'.format(nres, len(v))
            for res in v:
                write_vector(f, fixnum_bytes, res)
    t = timer() - t
    print('done ({:.2f}s).'.format(t))
    return fname

def add_cy(x, y, bits):
    return [(x + y) & ((1<<bits) - 1), (x + y) >> bits]

def sub_br(x, y, bits):
    return [(x - y) & ((1<<bits) - 1), int(x < y)]

def mul_wide(x, y, bits):
    return [(x * y) & ((1<<bits) - 1), (x * y) >> bits]

def sqr_wide(x, bits):
    return [(x * x) & ((1<<bits) - 1), (x * x) >> bits]

def modexp(x, y, z, bits):
    # FIXME: Handle these cases properly!
    if z % 2 == 0:
        return [0]
    return [pow(x, y, z)]

def paillier_encrypt(p, q, r, m, bits):
    n = p * q
    n2 = n * n
    return [((1 + m * n) * pow(r, n, n2)) % n2]

def test_inputs(nbytes):
    q = nbytes // 4
    res = [0]

    nums = [1, 2, 3];
    nums.extend([2**32 - n for n in nums])

    for i in range(q):
        res.extend(n << 32*i for n in nums)

    lognbits = (32*q).bit_length()
    for i in range(2, lognbits - 1):
        # b = 0xF, 0xFF, 0xFFFF, 0xFFFFFFFF, ...
        e = 1 << i
        b = (1 << e) - 1
        c = sum(b << 2*e*j for j in range(32*q // (2*e)))
        res.extend([c, (1 << 32*q) - c - 1])
    return res

def prev_prime(n):
    # subtract 1 or 2 from n, depending on whether n is even or odd.
    n -= 1 + n%2
    while not (is_prime(n) or n < 3):
        n -= 2
    assert n >= 3, 'Failed to find a prime'
    return n

def test_primes(nbytes):
    ps = [3, 5, 7]
    for i in range(nbytes, nbytes + 1): # i in range(1, ...):
        p = prev_prime(1 << (8*i))
        ps.append(p)
        p = prev_prime(p)
        ps.append(p)
        p = prev_prime(p)
        ps.append(p)
    return ps

def generate_tests(nbytes, tests):
    assert nbytes >= 4 and (nbytes & (nbytes - 1)) == 0, "nbytes must be a binary power at least 4"
    print('Generating input arguments... ', end='', flush=True)
    bits = nbytes * 8

    t = timer()
    xs = test_inputs(nbytes)
    t = timer() - t
    print('done ({:.2f}s). Created {} arguments.'.format(t, len(xs)))

    # ps is only used by paillier_encrypt, which needs primes 1/4 the size of
    # the ciphertext.
    ps = test_primes(nbytes // 4)
    print('primes = ', ps)

    ops = {
        'add_cy': (add_cy, xs, 2, 2, bits),
        'sub_br': (sub_br, xs, 2, 2, bits),
        'mul_wide': (mul_wide, xs, 2, 2, bits),
        'sqr_wide': (sqr_wide, xs, 1, 2, bits),
        'modexp': (modexp, xs, 3, 1, bits),
        'paillier_encrypt' : (paillier_encrypt, ps, 4, 1, bits)
    }
    test_names = ops.keys() & tests if len(tests) > 0 else ops.keys()
    test_fns = { fn: ops[fn] for fn in test_names }
    fnames = map(lambda fn: fn + '_' + str(nbytes), test_fns.keys())
    return list(map(write_tests, fnames, test_fns.values()))


def print_usage(progname):
    print("""Please specify the functions for which you want to generate test cases:

    $ python3 {} <func_1> ... <func_n>

where each <fn_i> is one of 'add_cy', 'sub_br', 'mul_wide', 'sqr_wide', 'modexp', 'paillier_encrypt'.
Specifying no functions will generate all of them (you will need to do this at least once).""".format(progname))


if __name__ == '__main__':
    import sys
    if len(sys.argv[1:]) > 0 and sys.argv[1] == '-h':
        print_usage(sys.argv[0])
    else:
        for i in range(2, 9):
            generate_tests(1 << i, sys.argv[1:])
