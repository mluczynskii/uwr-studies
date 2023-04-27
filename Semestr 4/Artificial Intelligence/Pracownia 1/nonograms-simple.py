# Mateusz Łuczyński 331826
from opt_dist import opt_dist
from dataclasses import dataclass
from random import choice

class Nonogram:
    def __init__(self, vertical, horizontal, height, width):
        self.vertical = vertical # column requirements
        self.horizontal = horizontal # row requirements
        self.height = height # puzzle size
        self.width = width 
        self.board = [['0' for _ in range(width)] for _ in range(height)] # fresh, empty board

    @dataclass 
    class Info: # info helps identify which column/row we randomly selected during the algorithm
        is_row: bool
        index: int  
        s: str

    def reset(self):
        self.board = [['0' for _ in range(self.width)] for _ in range(self.height)]

    def pp_print(self): # pretty-printer for the finished puzzle
        s = ''
        convert = lambda x : '#' if x == '1' else '.' # '.' -> blank, '#' -> colored
        for row in self.board:
            s = s + ''.join(map(convert, row)) + '\n'
        return s 
    
    def get_columns(self): # self explainatory
        return [''.join([self.board[y][x] for y in range(self.height)]) for x in range(self.width)]
    
    def get_rows(self):
        return [''.join([self.board[y][x] for x in range(self.width)]) for y in range(self.height)]

    def find_unfinished(self):
        res = []
        rows = self.get_rows()
        columns = self.get_columns()
        for idx, row in enumerate(rows): # get unfinished rows
            d = self.horizontal[idx]
            delta = opt_dist(row, d)
            if delta > 0: # delta = 0 means that it's already finished
                info = self.Info(True, idx, row)
                res.append(info)
        for idy, col in enumerate(columns): # get unfinished columns
            d = self.vertical[idy]
            delta = opt_dist(col, d)
            if delta > 0:
                info = self.Info(False, idy, col)
                res.append(info)
        return res

    def pick(self): # choose random unfinished column/row
        unfinished = self.find_unfinished()
        return choice(unfinished)
    
    def test_solved(self): # check end condition
        return self.find_unfinished() == []
    
    @staticmethod
    def flip(s, idx): # negate selected bit in a string and return changed copy
        return s[:idx] + ('1' if s[idx] == '0' else '0') + s[idx+1:]
    
    def flip_real(self, y, x):
        self.board[y][x] = '1' if self.board[y][x] == '0' else '0'

    def solve(self): # main loop
        limit = self.width * self.height * 4 # num of max iterations
        for _ in range(limit):
            columns = self.get_columns()
            rows = self.get_rows()
            if self.test_solved():
                return self.pp_print()
            info = self.pick() # randomly chosen row/column
            best, i = -1, -1
            for index, _ in enumerate(info.s): # try to find the best bit
                s1 = Nonogram.flip(info.s, index)
                if info.is_row:
                    s2 = Nonogram.flip(columns[index], info.index) # column on intersection
                    d1 = opt_dist(info.s, self.horizontal[info.index]) # row before changes
                    d2 = opt_dist(s1, self.horizontal[info.index]) # row after changes
                    d3 = opt_dist(columns[index], self.vertical[index]) # column before changes
                    d4 = opt_dist(s2, self.vertical[index]) # column after changes
                else:
                    s2 = Nonogram.flip(rows[index], info.index) # same stuff but mirrored
                    d1 = opt_dist(info.s, self.vertical[info.index]) 
                    d2 = opt_dist(s1, self.vertical[info.index]) 
                    d3 = opt_dist(rows[index], self.horizontal[index]) 
                    d4 = opt_dist(s2, self.horizontal[index]) 
                delta = d1 - d2 + d3 - d4 
                if delta > best: # we want to maximize delta, because that means that we made the biggest progress
                    best = delta 
                    i = index
            if i == -1: # just in case
                continue
            if info.is_row: # flip the best bit
                self.flip_real(info.index, i)
            else:
                self.flip_real(i, info.index)
        self.reset() # if unsuccessful, start again from scratch
        return self.solve()

def main():
    with open('zad5_input.txt', 'r') as inp, open('zad5_output.txt', 'w') as out:
        height, width = inp.readline().split(None)
        height, width = int(height), int(width)
        horizontal = []
        for _ in range(height):
            param = int(inp.readline().strip())
            horizontal.append(param)
        vertical = []
        for _ in range(width):
            param = int(inp.readline().strip())
            vertical.append(param)
        nonogram = Nonogram(vertical, horizontal, height, width)
        out.write(nonogram.solve())

if __name__ == '__main__':
    main()
