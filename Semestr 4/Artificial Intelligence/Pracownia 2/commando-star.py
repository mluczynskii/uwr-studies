# Mateusz Łuczyński 331826
from enum import Enum 
from queue import Queue 
from collections import defaultdict
from math import inf
from queue import PriorityQueue

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
        self.start_points = frozenset(start_points)
        self.end_points = frozenset(end_points)

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
    
    def gen_state(self, state, move):
        n_locations = set()
        for pos in state:
            (idy, idx) = pos
            match move:
                case Move.UP:
                    n_pos = (idy-1, idx)
                case Move.DOWN:
                    n_pos = (idy+1, idx)
                case Move.RIGHT:
                    n_pos = (idy, idx+1)
                case Move.LEFT:
                    n_pos = (idy, idx-1)
            if self.is_legal(n_pos):
                n_locations.add(n_pos)
            else:
                n_locations.add(pos)
        return frozenset(n_locations)
    
    def solve(self): # A* algorithm   
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
            return h(node, 0) + g(node)
        
        root = 0, (self.start_points, 0, '', None) # prio, data
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
                return seq
            for move in Move:
                n_state = self.gen_state(state, move)
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

# Wyniki eksperymentów z niedopuszczalnymi heurystykami
# <eps>         <optimal>           <average time for passing tests>
# 0             21/21               39/21 = 1.85s
# 0.1           21/21               25/21 = 1.19s
# 0.2           13/21               18/13 = 1.38s 
# 0.3           9/21                18/9 = 2.0s
# 0.4           8/21                18/8 = 2.25s
# 0.5           7/21                19/7 = 2.71s

        
        

        

        

    
            
