# Mateusz Łuczyński 331826
class Nonogram:
    def __init__(self, horizontal, vertical):
        self.height = len(horizontal)
        self.width = len(vertical)
        row_domains = []
        col_domains = []
        for row_config in horizontal:
            domain = Nonogram.calculate_domain(row_config, self.width)
            row_domains.append(domain)
        for col_config in vertical:
            domain = Nonogram.calculate_domain(col_config, self.height)
            col_domains.append(domain)
        self.row_domains = row_domains
        self.col_domains = col_domains
        #print(row_domains)
        #print(col_domains)

    @staticmethod
    def calculate_domain(config, size):
        domain = []
        if sum(config) + len(config) - 1 > size:
            return None
        if len(config) == 0:
            domain.append('.' * size) 
            return domain
        d = config[0]
        for i in range(size-d+1):
            pref = '.' * i + '#' * d
            if len(config) == 1:
                domain.append(pref + '.' * (size - len(pref)))
            else:
                xs = Nonogram.calculate_domain(config[1:], size-len(pref)-1)
                if xs == None:
                    if len(domain) == 0:
                        return None 
                    else:
                        return domain
                for suf in xs:
                    if suf == '':
                        domain.append(pref)
                    else:
                        domain.append(pref + '.' + suf)
        return domain
    
    def solve(self): # backtrack algorithm on 
        idy = 0
        backtrack = [(-1, []) for _ in range(self.height+1)]
        while idy < self.height:
            i = backtrack[idy][0] + 1
            if idy == 0:
                col_domains = self.col_domains
            else:
                col_domains = backtrack[idy-1][1]
            minimum = len(min(col_domains, key=len))
            if i >= len(self.row_domains[idy]): # exhausted all options for previous row choices
                #print(f'Failed, reverting to previous row')
                if idy-1 < 0:
                    raise Exception('Unsolvable') # exhausted all options all together
                backtrack[idy] = (-1, []) # revert to previous state
                idy = idy-1
                continue
            row = self.row_domains[idy][i]
            #print(f'Trying row {row} at index {idy}')
            ncol_domains = [] # filter unfitting columns
            for idx, col_domain in enumerate(col_domains):
                ncol_domain = list(filter(lambda col : col[idy] == row[idx], col_domain))
                if len(ncol_domain) < minimum:
                    minimum = len(ncol_domain)
                ncol_domains.append(ncol_domain)
            #print(f'Current col_domains: {ncol_domains}')
            if minimum == 0: # some columns can't be colored with current row choices
                #print(f'Failed, trying other configuration')
                backtrack[idy] = (i, [])
            else: # go to the next row if everything's all right so far
                backtrack[idy] = (i, ncol_domains)
                idy = idy+1 
        s = '' # if got so far, then that means that row choices are correct
        for idy, data in enumerate(backtrack[:-1]):
            (idx, _) = data
            s = s + self.row_domains[idy][idx] + '\n'
        return s
        
def main():
    with open('zad_input.txt', 'r') as inp, open('zad_output.txt', 'w') as out:
        size = inp.readline()
        height, _ = size.split(' ')
        height = int(height)
        horizontal, vertical = [], []
        for idy, line in enumerate(inp):
            params = list(map(int, line.split(' ')))
            if idy < height:
                horizontal.append(params)
            else:
                vertical.append(params)
        nonogram = Nonogram(horizontal, vertical)
        out.write(nonogram.solve())

if __name__ == '__main__':
    main()