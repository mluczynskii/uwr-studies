# Mateusz Łuczyński 331826
from enum import Enum 
from queue import Queue 
from collections import defaultdict
from math import inf
from queue import PriorityQueue
from itertools import combinations_with_replacement as seq
from random import choice

class Move(Enum):
    UP = 'U'
    DOWN = 'D'
    LEFT = 'L'
    RIGHT = 'R'

class Field(Enum):
    WALL = '#'
    EMPTY = ' '
    START = 'S'
    GOAL = 'G'
    SEMI = 'B'

class Puzzle:
    def __init__(self, labirynth):
        height = len(labirynth)
        width = len(labirynth[0])
        layout = [[Field.EMPTY for _ in range(width)] for _ in range(height)]
        start_points = set()
        end_points = set()
        for idy, row in enumerate(labirynth): # parse labirynth
            for idx, c in enumerate(row):
                token = Puzzle.parse_token(c)
                if token == Field.WALL:
                    layout[idy][idx] = token 
                elif token != Field.EMPTY:
                    pos = (idy, idx)
                    if token == Field.START or token == Field.SEMI:
                        start_points.add(pos)
                    if token == Field.GOAL or token == Field.SEMI:
                        end_points.add(pos)
        self.layout = layout
        self.locations = frozenset(start_points)
        self.end_points = frozenset(end_points)
        self.sequence = ''
        self.height = height 
        self.width = width

        self.idle_count = 0 # desperate attempts to rescue things after uncertainty stops dropping
        lifeline = [None for _ in range(4)] # desperate attempts to rescue things after uncertainty stops dropping
        lifeline[0] = [Move.RIGHT, Move.DOWN] * (width//2) + [Move.RIGHT, Move.UP] * (width//2)
        lifeline[1] = [Move.LEFT, Move.UP] * (height//2) + [Move.RIGHT, Move.UP] * (height//2)
        lifeline[2] = [Move.DOWN, Move.LEFT] * (width//2) + [Move.UP, Move.LEFT] * (width//2) 
        lifeline[3] = [Move.RIGHT, Move.UP] * (height//2) + [Move.LEFT, Move.UP] * (height//2)
        self.lifeline = lifeline

        move_sequences = [] # generate all move sequences of set lengths
        for k in range(1, 14):
            move_sequences.append(seq(Move, k))
        self.move_sequences = move_sequences

        distances = [[inf for _ in range(width)] for _ in range(height)]
        for end in end_points: # calculate actual distances to end points using BFS
            (y, x) = end 

            q = Queue(maxsize=0)
            visited = defaultdict(lambda : False)

            root = (end, 0)
            q.put(root)
            visited[end] = True 

            while not q.empty():
                node = q.get()
                (idy, idx), layer = node
                distances[idy][idx] = min(distances[idy][idx], layer)
                for (y, x) in [(idy+1, idx), (idy-1, idx), (idy, idx-1), (idy, idx+1)]:
                    n_pos = (y, x)
                    if not visited[n_pos] and self.layout[y][x] == Field.EMPTY:
                        visited[n_pos] = True 
                        n_node = (n_pos, layer+1)
                        q.put(n_node)
        self.distances = distances

    def check_end(self, state):
        for pos in state:
            if pos not in self.end_points: # not everyone reached their goals
                return False 
        return True

    @staticmethod
    def parse_token(c):
        match c:
            case ' ': 
                token = Field.EMPTY
            case '#':
                token = Field.WALL
            case 'S':
                token = Field.START
            case 'G':
                token = Field.GOAL
            case 'B':
                token = Field.SEMI
        return token
    
    def is_legal(self, pos):
        (idy, idx) = pos 
        return self.layout[idy][idx] != Field.WALL
    
    def get_new_position(self, pos, direction):
        (y, x) = pos 
        match direction:
            case Move.UP:
                n_pos = (y-1, x)
            case Move.DOWN:
                n_pos = (y+1, x)
            case Move.LEFT:
                n_pos = (y, x-1)
            case Move.RIGHT:
                n_pos = (y, x+1)
        if self.is_legal(n_pos):
            return n_pos 
        return pos # nothing's changed if move illegal
    
    def gen_state(self, state, direction, count):
        n_state = set()
        for pos in state:
            n_pos = pos
            for _ in range(count):
                n_pos = self.get_new_position(n_pos, direction)
            n_state.add(n_pos)
        return frozenset(n_state)
    
    def reduce_uncertainty(self):
        best_seq, best_state = '', self.locations
        for seq_set in self.move_sequences:
            for sequence in seq_set:
                n_state = self.run_simulation(self.locations, sequence)
                if len(n_state) < len(best_state):
                    best_seq, best_state = sequence, n_state
                elif len(n_state) == len(best_state) and len(sequence) < len(best_seq):
                    best_seq, best_state = sequence, n_state
        if len(best_seq) == 0: # no sequence changes anything, we need to mix something up
            #best_seq = self.lifeline[self.idle_count]
            #self.idle_count = (self.idle_count + 1) % len(self.lifeline)
            best_seq = choice(self.lifeline) # completely random sequence
            best_state = self.run_simulation(self.locations, best_seq)
        best_seq = ''.join(map(lambda move : move.value, best_seq))
        self.locations = best_state
        self.sequence = self.sequence + best_seq

    def run_simulation(self, state, sequence): # generate locations after sequence of moves
        for move in sequence:
            state = self.gen_state(state, move, 1)
        return state 
    
    def solve(self): # A* algorithm + greedy uncertainty reduction
        while len(self.locations) > 6:
            self.reduce_uncertainty()
            if self.check_end(self.locations):
                return self.sequence
        
        # node = (state, depth, move, prev)         
        def h(node, eps): # heuristic
            res = -inf
            (state, _, _, _) = node
            for (idy, idx) in state:
                res = max(res, self.distances[idy][idx])
            return (1 + eps) * res
        
        def g(node): # distance from the beginning
            (_, depth, _, _) = node
            return depth
        
        def f(node): # node priority
            return h(node, 0.1) + g(node)
        
        root = 0, (self.locations, 0, '', None) # prio, data
        frontier = PriorityQueue()
        frontier.put(root)
        visited = defaultdict(lambda : False)

        while not frontier.empty():
            _, node = frontier.get()
            (state, depth, move, prev) = node 
            visited[state] = True
            if self.check_end(state):
                p = node 
                seq = ''
                while p != None:
                    (_, _, move, prev) = p 
                    seq = move + seq 
                    p = prev
                return self.sequence + seq
            for move in Move:
                n_state = self.gen_state(state, move, 1)
                if visited[n_state]:
                    continue
                child = (n_state, depth+1, move.value, node)
                child_prio = f(child)
                n_node = child_prio, child 
                frontier.put(n_node)

def main():
    with open('zad_input.txt', 'r') as inp, open('zad_output.txt', 'w') as out:
        labirynth = []
        for line in inp:
            labirynth.append(line[:-1]) # ignore newline
        puzzle = Puzzle(labirynth)
        out.write(puzzle.solve() + '\n')

if __name__ == '__main__':
    main()

# Comparison
# uncertainty reduction + A* + niedopuszczalna heurystyka   vs   uncertainty reduction + BFS  
# 11:   49 in 1.61s                 120 in 1.41s
# 10:   98 in 0.99s                 109 in 0.98s
# 9:    96 in 1.29s                 73 in 1.08s
# 8:    125 in 1.75s                [158, 214, 1189] FAILED
# 7:    84 in 1.55s                 [194, 173, 278] FAILED
# 6:    137 in 1.26s                [152, 230, 176] FAILED
# 5:    85 in 1.06s                 58 in 0.92s
# 4:    52 in 1.01s                 83 in 0.99s
# 3:    137 in 2.5s                 128 in 2.34s
# 2:    53 in 1.17s                 107 in 1.01s
# 1:    8 in 0.15s                  8 in 0.14s
        
        

        

        

    
            
