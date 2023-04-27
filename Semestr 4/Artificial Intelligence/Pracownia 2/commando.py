# Mateusz Łuczyński 331826
from enum import Enum 
from queue import Queue 
from collections import defaultdict
from itertools import combinations_with_replacement as seq
from random import choice

class Field(Enum):
    EMPTY = ' '
    WALL = '#'
    START = 'S'
    GOAL = 'G'
    SEMI = 'B'

class Move(Enum):
    UP = 'U'
    DOWN = 'D'
    LEFT = 'L'
    RIGHT = 'R'

class Node: # node representation for BFS
    def __init__(self, move, prev, state):
        self.move = move 
        self.prev = prev 
        self.state = state 

    def backtrack(self): # reconstruct the sequence of moves
        sequence = ''
        node = self
        while node.prev != None:
            sequence = node.move.value + sequence
            node = node.prev
        return sequence 

class Puzzle:
    def __init__(self, labirynth):
        height = len(labirynth)
        width = len(labirynth[0])
        layout = [[Field.EMPTY for _ in range(width)] for _ in range(height)]
        locations = set()
        end_points = set()
        for idy, row in enumerate(labirynth):
            for idx, c in enumerate(row):
                token = Puzzle.parse_token(c)
                if token == Field.WALL:
                    layout[idy][idx] = token 
                elif token != Field.EMPTY:
                    pos = (idy, idx)
                    if token == Field.START or token == Field.SEMI:
                        locations.add(pos)
                    if token == Field.GOAL or token == Field.SEMI:
                        end_points.add(pos)
        self.locations = frozenset(locations) # possible starting points
        self.end_points = end_points # all goal locations
        self.layout = layout # layout of the labirynth
        self.sequence = ''

        self.init_loc = locations # remember starting locations in case of a reset

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

    def reset(self):
        self.locations = self.init_loc
        self.sequence = ''

    def is_legal(self, pos):
        (idy, idx) = pos 
        if self.layout[idy][idx] == Field.EMPTY: # check if there are no walls in the way
            return True 
        return False
    
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
    
    def run_simulation(self, state, sequence): # generate locations after sequence of moves
        for move in sequence:
            state = self.gen_state(state, move, 1)
        return state 
    
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
                    
    def check_end(self, state):
        for pos in state:
            if pos not in self.end_points: # not everyone has reached the goal
                return False 
        return True
    
    def solve(self):
        # 1st phase
        while len(self.locations) > 2:
            self.reduce_uncertainty()
            if self.check_end(self.locations):
                if len(self.sequence) > 150:
                    self.reset()
                    return self.solve()
                return self.sequence
            
        # 2nd phase
        q = Queue(maxsize=0)
        vis = defaultdict(lambda : False)

        root = Node(None, None, self.locations)
        vis[self.locations] = True 
        q.put(root)
        maks = -1
        while not q.empty():
            maks = max(q.qsize(), maks)
            node = q.get()
            if self.check_end(node.state):
                s = node.backtrack()
                if len(self.sequence + s) > 150:
                    self.reset()
                    #print(maks)
                    return self.solve()
                return self.sequence + s
            for move in Move:
                n_state = self.gen_state(node.state, move, 1)
                if not vis[n_state]:
                    vis[n_state] = True 
                    child = Node(move, node, n_state)
                    q.put(child)
        
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
                token = Field.SEMI # start + end
        return token

def main():
    with open('zad_input.txt', 'r') as inp, open('zad_output.txt', 'w') as out:
        labirynth = []
        for line in inp:
            labirynth.append(line[:-1]) # ignore newline
        puzzle = Puzzle(labirynth)
        out.write(puzzle.solve() + '\n')

if __name__ == '__main__':
    main()