r = 21888242871839275222246405745257275088614511777268538073601725287587578984328

a = 7296080957279758407415468581752425029516121466805344781232734728849116493472
b = 16213513238399463127589930181672055621146936592900766180517188641980520820846
p = 21888242871839275222246405745257275088548364400416034343698204186575808495617
Fp = FiniteField(p)

E = EllipticCurve(Fp, [a, b])

zero = E(0, 1, 0)

def read_params(path):
    res = []
    for line in open(path, 'r'):
        q = []
        for pair in line.strip().split(';'):
            p = tuple(int(x) for x in pair.split(','))
            q.append(p)
        res.append(q)
    return res

# uparams = [ 8 * E.random_element() for i in range(512) ]
# params4 = [ (p, 2*p, 3*p, 4*p) for p in params]
# print [ (a.xy(), b.xy(), c.xy(), d.xy()) for (a,b,c,d) in params4 ]

def digest(triples):
    res = zero
    for i, (s0, s1, sign_bit) in enumerate(triples):
        term = params[i][int(s0) + 2 * int(s1)]
        if sign_bit:
            term = -term
        res = res + term
    return res
