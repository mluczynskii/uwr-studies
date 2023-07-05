# creates variable representing a cell at pos (i,j)
def B(i,j):
    return 'B%d_%d' % (i,j)

# sets all cell domains to {0,1}
def domains(bs):
    return [cell + ' in 0..1' for cell in bs]

# returns a list of cells at row i
def get_row(i, C):
    return [B(i, j) for j in range(C)]

# returns a list of cells at column j
def get_col(j, R):
    return [B(i, j) for i in range(R)]

# returns a string in format 'bs_0 + bs_1 + ... + bs_n #= val'
def varsum(bs, val):
    args = '['
    for cell in bs[:-1]:
        args = args + '%s,' % cell 
    args = args + '%s]' % bs[-1]
    return 'sum(%s, #=, %d)' % (args, val)

# returns a list of constraints telling that the number of 
# colored cells must respect the specification for each row
def horizontal(config, R, C):
    return [varsum(get_row(i, C), config[i]) for i in range(R)]

# same as horizontal but for columns
def vertical(config, R, C):
    return [varsum(get_col(j, R), config[j]) for j in range(C)]

# return a constraint representing cells whose values
# are initially provided
def setup(triples):
    return ['%s #= %d' % (B(i, j), val) for i, j, val in triples]

# returns a list of all triples A, B, C representing
# all 1x3 and 3x1 blocks of cells on the board
def get_rectangles(R, C):
    result = []
    for i in range(R):
        row = get_row(i, C)
        # A B C
        result = result + [[row[j], row[j+1], row[j+2]] for j in range(C-2)]
    for j in range(C):
        col = get_col(j, R)
        # A
        # B
        # C
        result = result + [[col[i], col[i+1], col[i+2]] for i in range(R-2)]
    return result 

# returns a list of all quadruplets A, B, C, D representing
# all 2x2 squares of cells on the board
def get_squares(R, C):
    result = []
    for i in range(R-1):
        for j in range(C-1):
            # A B
            # C D
            square = (B(i, j), B(i, j+1), B(i+1, j), B(i+1, j+1))
            result.append(square)
    return result 

# returns a list of constraints related to 1x3 (3x1) rectangles
def rectangles(R, C):
    xs = get_rectangles(R, C)
    args = '['
    for [A, B, C] in xs[:-1]:
        args = args + '[%s, %s, %s],' % (A, B, C)
    [A, B, C] = xs[-1]
    args = args + '[%s, %s, %s]]' % (A, B, C)
    legal = '[[1,0,1], [1,1,0], [0,1,1], [1,0,0], [0,0,1], [1,1,1], [0,0,0]]'
    return ['tuples_in(%s, %s)' % (args, legal)]

# return a list of constraints related to 2x2 squares
def squares(R, C):
    xs = get_squares(R, C)
    args = '['
    for [A, B, C, D] in xs[:-1]:
        args = args + '[%s, %s, %s, %s],' % (A, B, C, D)
    [A, B, C, D] = xs[-1]
    args = args + '[%s, %s, %s, %s]]' % (A, B, C, D)
    empty = '[0,0,0,0]'
    corners = '[1,0,0,0], [0,1,0,0], [0,0,1,0], [0,0,0,1]'
    twos = '[1,1,0,0], [1,0,1,0], [0,1,0,1], [0,0,1,1]'
    full = '[1,1,1,1]'
    legal = f'[{empty}, {twos}, {full}, {corners}]'
    return ['tuples_in(%s, %s)' % (args, legal)]

# stolen from sudoku.py
def print_constraints(Cs, indent, d):
    position = indent
    writeln (indent * ' ', end='')
    for c in Cs:
        writeln (c + ',', end=' ')
        position += len(c)
        if position > d:
            position = indent
            writeln ('')
            writeln (indent * ' ', end='')
 
def storms(rows, cols, triples):
    writeln(':- use_module(library(clpfd)).')
    
    R = len(rows)
    C = len(cols)
    
    bs = [ B(i,j) for i in range(R) for j in range(C)]
    
    writeln('solve([' + ', '.join(bs) + ']) :- ')
    
    cs = domains(bs) + setup(triples) + horizontal(rows, R, C) + vertical(cols, R, C) + squares(R, C) + rectangles(R, C)
    
    print_constraints(cs, 4, 70)
    writeln('')
    writeln('    labeling([ff], [' +  ', '.join(bs) + ']).' )
    writeln('')
    writeln(":- solve(X), write(X), nl.")

def writeln(s, end='\n'):
    output.write(s + end)

txt = open('zad_input.txt').readlines()
output = open('zad_output.txt', 'w')

rows = list(map(int, txt[0].split()))
cols = list(map(int, txt[1].split()))
triples = []

for i in range(2, len(txt)):
    if txt[i].strip():
        triples.append(map(int, txt[i].split()))

storms(rows, cols, triples)            
        

