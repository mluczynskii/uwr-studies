# Mateusz Łuczyński 331826
from queue import Queue
from enum import Enum

class Relation(Enum):
    ROW_COLUMN = 'RC'
    COLUMN_ROW = 'CR'

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
    
    def revise(self, i, j, relation):
        match relation:
            case Relation.ROW_COLUMN:
                domain = self.row_domains[i]
                counter_domain = self.col_domains[j]
            case Relation.COLUMN_ROW:
                domain = self.col_domains[i]
                counter_domain = self.row_domains[j]
        revised = False
        new_domain = domain
        for x in domain:
            for y in counter_domain:
                if y[i] == x[j]:
                    break 
            else:
                revised = True 
                new_domain = list(filter(lambda cfg : cfg[j] != x[j], new_domain))
        match relation:
            case Relation.ROW_COLUMN:
                self.row_domains[i] = new_domain
            case Relation.COLUMN_ROW:
                self.col_domains[i] = new_domain
        return revised        
    
    def solve(self): # ac-3 algorithm
        q = Queue(maxsize=0)
        for i in range(self.height):
            for j in range(self.width):
                q.put((i, j, Relation.ROW_COLUMN))
                q.put((j, i, Relation.COLUMN_ROW))
        while not q.empty():
            (i, j, relation) = q.get()
            if self.revise(i, j, relation): # assert: the puzzle is solvable
                match relation:
                    case Relation.ROW_COLUMN:
                        for idx in range(self.width):
                            if idx != j:
                                q.put((idx, i, Relation.COLUMN_ROW))
                    case Relation.COLUMN_ROW:
                        for idy in range(self.height):
                            if idy != j:
                                q.put((idy, i, Relation.ROW_COLUMN))
        # after this, all domains should be of length 1
        answer = ''
        for domain in self.row_domains:
            answer = answer + domain[0] + '\n'
        return answer

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