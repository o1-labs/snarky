import hashlib


MNT4298_r = 475922286169261325753349249653048451545124878552823515553267735739164647307408490559963137
MNT4753_r = 41898490967918953402344214791240637128170709919953949071783502921025352812571106773058893763790338921418070971888458477323173057491593855069696241854796396165721416325350064441470418137846398469611935719059908164220784476160001

MNT6298_r = 475922286169261325753349249653048451545124879242694725395555128576210262817955800483758081
MNT6753_r = 41898490967918953402344214791240637128170709919953949071783502921025352812571106773058893763790338921418070971888253786114353726529584385201591605722013126468931404347949840543007986327743462853720628051692141265303114721689601

BN128_r = 21888242871839275222246405745257275088548364400416034343698204186575808495617

BN382_p = 5543634365110765627805495722742127385843376434033820803590214255538854698464778703795540858859767700241957783601153
BN382_q = 5543634365110765627805495722742127385843376434033820803592568747918351978899288491582778380528407187068941959692289

def random_value(F, prefix, i):
    return F(int(hashlib.sha256('%s%d' % (prefix, i)).hexdigest(), 16))

m = 3
rounds = 100

prefix = 'CodaRescue'

def round_constants(F):
    name = prefix + 'RoundConstants'
    return [ [ random_value(F, name, r * m + i) for i in xrange(m) ]
            for r in xrange( rounds ) ]

def matrix_str(of_string_wrap, rows):
    return '[|' + ';'.join('[|' + ';'.join(of_string_wrap('"{}"'.format(str(x))) for x in row) + '|]' for row in rows) + '|]'

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

curves = [  ('Bn128', BN128_r), ('Mnt4_298', MNT4298_r), ('Mnt4_753', MNT4753_r) ]
if sys.argv[1] == 'coda':
    def module_strs(name, r_small, r_medium):
        wrap = lambda s: 'str {}'.format(s)

        F_small = FiniteField(r_small)
        F_medium = FiniteField(r_medium)
        strs = [
            ('let str x = {}_full.Bigint.R.(to_field (of_decimal_string x))'.format(name)),
            ('''[%%import "../../config.mlh"]'''),
            ('''[%%if curve_size = 298]'''),
            ('let inv_alpha = "432656623790237568866681136048225865041022616866203195957516123399240588461280445963602851"'),
            ('let mds ='),
            (matrix_str(wrap, mds(F_small))),
            ('let round_constants ='),
            (matrix_str(wrap, round_constants(F_small))),
            ('''
            [%%elif
            curve_size = 753]'''),
            ('let inv_alpha = "38089537243562684911222013446582397389246099927230862792530457200932138920519187975508085239809399019470973610807689524839248234083267140972451128958905814696110378477590967674064016488951271336010850653690825603837076796509091"'),
            ('let mds ='),
            (matrix_str(wrap, mds(F_medium))),
            ('let round_constants ='),
            (matrix_str(wrap, round_constants(F_medium))),
            ('''
            [%%else]

            [%%show
            curve_size]

            [%%error
            "invalid value for \\"curve_size\\""]

            [%%endif]''') ]
        return 'module {} = struct {} end'.format(name, '\n'.join(strs))

    print 'open Curve_choice'
    print module_strs('Tick', MNT4298_r, MNT4753_r)
    print module_strs('Tock', MNT6298_r, MNT6753_r)

else:
    for name, r in  [ ('Bn382_p', BN382_p), ('Bn382_q', BN382_q) ]:
        wrap = lambda x: x
        F = FiniteField(r)
        print ('let params_{} = '.format(name)
                + '{ Params.mds=' + matrix_str(wrap, mds(F)) + ';'
                + 'round_constants= ' + matrix_str(wrap, round_constants(F))
                + '}' )
