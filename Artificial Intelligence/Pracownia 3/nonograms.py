# Mateusz Łuczyński 331826
from queue import Queue
from enum import Enum 
from dataclasses import dataclass
from typing import *
from collections import defaultdict
from queue import LifoQueue

class Relation(Enum):
    RC = 'RC' # x = row, y = column
    CR = 'CR' # x = column, y = row

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

        # used in self.next_block()
        self.entries = [(i, True) for i in range(self.height)] + [(i, False) for i in range(self.width)]

    # returns a list of row/column configurations that satisfy the suplied demands 
    @staticmethod
    def calculate_domain(config: List[int], size: int) -> List[str]:
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
    
    # dataclass used in ac-3 components, represents a constraint that is used
    # to discard domain values that cannot satisfy that constraint in any way
    @dataclass
    class Arc:
        idx: int 
        idy: int
        relation: Relation  
    
    # find configs in self.*_domains[arc.idx] 
    # that are not compatible with any in self.*_domains[arc.idy] and remove them
    def revise(self, arc: Arc) -> bool: 
        revised = False 
        domain = self.row_domains[arc.idx] if arc.relation == Relation.RC else self.col_domains[arc.idx]
        counter_domain = self.col_domains[arc.idy] if arc.relation == Relation.RC else self.row_domains[arc.idy]
        new_x = domain
        for xconfig in domain:
            for yconfig in counter_domain:
                if xconfig[arc.idy] == yconfig[arc.idx]:
                    break 
            else:
                revised = True 
                new_x = [cfg for cfg in new_x if cfg[arc.idy] != xconfig[arc.idy]]
        if arc.relation == Relation.RC:
            self.row_domains[arc.idx] = new_x
        else:
            self.col_domains[arc.idx] = new_x
        return revised

    # ac-3 algorithm for discarding useless configurations
    def deduct(self) -> None:
        q = Queue(maxsize=0)
        for idx in range(self.width):
            for idy in range(self.height):
                q.put(Nonogram.Arc(idx, idy, Relation.CR))
                q.put(Nonogram.Arc(idy, idx, Relation.RC))
        while not q.empty():
            arc = q.get()
            if self.revise(arc):
                if arc.relation == Relation.RC:
                    for idx in [i for i in range(self.width) if i != arc.idy]:
                        q.put(Nonogram.Arc(idx, arc.idx, Relation.CR))
                else:
                    for idx in [i for i in range(self.height) if i != arc.idy]:
                        q.put(Nonogram.Arc(idx, arc.idx, Relation.RC))

    class Kind(Enum):
        ROW = 'R'
        COLUMN = 'C'

    # used in self.solve(), chooses next row/column to be processed based on domain length
    # (smaller first)
    def next_block(self, used: List[Tuple[int, bool]]):
        unused = [entry for entry in self.entries if entry not in used]
        choice = min(unused, key=lambda entry : len(self.row_domains[entry[0]]) if entry[1] else len(self.col_domains[entry[0]]))
        return choice 
    
    # class used to store informations needed during reverting process during backtracking in self.solve()
    @dataclass
    class State:
        rows: List[List[str]]
        cols: List[List[str]]

    # returns a copy of self.*_domains because fck python and i was sitting for 2h figuring this out
    def get_rows(self):
        return [domain for domain in self.row_domains]
    def get_cols(self):
        return [domain for domain in self.col_domains]

    # backtracking algorithm with run-time deduction
    def solve(self): 
        self.deduct()
        stack = LifoQueue(maxsize=0)
        used = []
        choice = defaultdict(lambda : -1)
        stack.put(Nonogram.State(self.get_rows(), self.get_cols()))
        while True:
            stack.put(Nonogram.State(self.get_rows(), self.get_cols())) # state when coming in

            block = self.next_block(used) # choose next unfilled row/col
            idy, is_row = block
            #print(f'processing {is_row} #{idy}')

            used.append(block)

            choice[block] = choice[block] + 1 # choose next configuration number for current row/col
            domain = self.row_domains[idy] if is_row else self.col_domains[idy]
            if choice[block] >= len(domain):
                choice[block] = -1
                used = used[:-2]
                _ = stack.get()
                revert = stack.get()
                self.row_domains = revert.rows
                self.col_domains = revert.cols
                continue

            config = self.row_domains[idy][choice[block]] if is_row else self.col_domains[idy][choice[block]]
            if is_row:
                self.row_domains[idy] = [config]
            else:
                self.col_domains[idy] = [config]
            self.deduct()

            if len(min(self.row_domains + self.col_domains, key=lambda domain : len(domain))) == 0:
                used = used[:-1]
                revert = stack.get()
                self.row_domains = revert.rows
                self.col_domains = revert.cols
                continue 
            
            if len(used) == self.height + self.width:
                break
        
        solution = ''
        for [config] in self.row_domains:
            solution = solution + config + '\n'
        return solution

 
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