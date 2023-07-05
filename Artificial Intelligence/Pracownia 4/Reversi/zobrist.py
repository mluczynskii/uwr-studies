# generates zobrist hashes for reversi
from itertools import product
from random import getrandbits
from csv import writer

with open('zobrist.csv', 'w') as out:
    cswriter = writer(out)
    for color in ['B', 'W']:
        for row, col in product(range(8), range(8)):
            n = getrandbits(64)
            label = color + str(row) + str(col)
            cswriter.writerow([label, n])
