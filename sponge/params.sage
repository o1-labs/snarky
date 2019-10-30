import hashlib


MNT4298_r = 475922286169261325753349249653048451545124878552823515553267735739164647307408490559963137
MNT4753_r = 41898490967918953402344214791240637128170709919953949071783502921025352812571106773058893763790338921418070971888458477323173057491593855069696241854796396165721416325350064441470418137846398469611935719059908164220784476160001

BN128_r = 21888242871839275222246405745257275088548364400416034343698204186575808495617

def random_value(F, prefix, i):
    return F(int(hashlib.sha256('%s%d' % (prefix, i)).hexdigest(), 16))

m = 3
rounds = 100

prefix = 'CodaRescue'

def round_constants(F):
    name = prefix + 'RoundConstants'
    return [ [ random_value(F, name, r * m + i) for i in xrange(m) ]
            for r in xrange( rounds ) ]

def matrix_str(rows):
    return '[|' + ';'.join('[|' + ';'.join('"{}"'.format(str(x)) for x in row) + '|]' for row in rows) + '|]'

def mds(F):
    name = prefix + 'MDS'
    for attempt in xrange(100):
        x_values = [random_value(F, name + 'x', attempt * m + i)
                    for i in xrange(m)]
        y_values = [random_value(F, name + 'y', attempt * m + i)
                    for i in xrange(m)]
# Make sure the values are distinct.
        assert len(set(x_values + y_values)) == 2 * m, \
            'The values of x_values and y_values are not distinct'
        mds = matrix([[1 / (x_values[i] - y_values[j]) for j in xrange(m)]
                        for i in xrange(m)])
# Sanity check: check the determinant of the matrix.
        x_prod = product(
            [x_values[i] - x_values[j] for i in xrange(m) for j in xrange(i)])
        y_prod = product(
            [y_values[i] - y_values[j] for i in xrange(m) for j in xrange(i)])
        xy_prod = product(
            [x_values[i] - y_values[j] for i in xrange(m) for j in xrange(m)])
        expected_det = (1 if m % 4 < 2 else -1) * x_prod * y_prod / xy_prod
        det = mds.determinant()
        assert det != 0
        assert det == expected_det, \
            'Expected determinant %s. Found %s' % (expected_det, det)
        if len(mds.characteristic_polynomial().roots()) == 0:
            # There are no eigenvalues in the field.
            return mds

for name, r in [  ('Bn128', BN128_r), ('Mnt4_298', MNT4298_r), ('Mnt4_753', MNT4753_r) ]:
  F = FiniteField(r)
  print ('let params_{} = '.format(name)
          + '{ Params.mds=' + matrix_str(mds(F)) + ';'
          + 'round_constants= ' + matrix_str(round_constants(F))
          + '}' )
