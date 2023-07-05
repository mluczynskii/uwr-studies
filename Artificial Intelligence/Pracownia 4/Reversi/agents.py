from lib import Agent, PASS, DIRECTIONS, WHITE, BLACK, shift
from random import choice 
from sys import maxsize as inf
from itertools import product
from math import sqrt, log
from time import time 
from copy import deepcopy
from csv import reader

class Rand(Agent):
    def decide(self):
        actions = self.actions()
        return choice(actions)
    
class Carlos(Agent):
    def __init__(self, state=None, turn=None):
        super().__init__(state, turn)
        self.translate = {}
        with open('-', 'r') as inp:
            csvreader = reader(inp)
            for [label, value] in csvreader:
                self.translate[label] = int(value)
        self.zobrist = self.translate['W34'] ^ self.translate['W43'] ^ self.translate['B33'] ^ self.translate['B44']
        self.tree = {}
        
    def result(self, action):
        super().result(action)
        if action != PASS:
            (row, col), _ = action 
            turn = 'W' if self.turn == BLACK else 'B'
            key = turn + str(row) + str(col)
            update = self.translate[key]
            self.zobrist = self.zobrist ^ update 

    # override
    def revert(self):
        action = self.stack.get()
        move, captures = action 
        if action != PASS:
            row, col = move 

            turn = 'W' if self.turn == WHITE else 'B'
            key = turn + str(row) + str(col)
            update = self.translate[key]
            self.zobrist = self.zobrist ^ update 

            self.state[row][col] = None 
            for row, col in captures:
                self.state[row][col] = self.turn 
        self.turn = not self.turn 

    def reset(self):
        super().reset() 
        self.zobrist = self.translate['W34'] ^ self.translate['W43'] ^ self.translate['B33'] ^ self.translate['B44']
        #self.tree = {}

    class Node:
        def __init__(self, agent, action=None, parent=None):
            self.unvisited = agent.actions() 
            self.parent = parent
            self.action = action 
            self.children = [] # list of nodes
            self.balance = 0 # x_i
            self.visits = 0 # N_t

    def decide(self):
        self.t = 1

        hit = self.tree.get(self.zobrist)
        if hit: root = hit
        else: 
            root = self.Node(self)
            self.tree[self.zobrist] = root 
        root.parent = None 

        color = self.turn
        start = time()

        def ucb(node):
            C = 2.0
            if node.visits == 0: return inf
            return node.balance + C * sqrt(log(self.t)/node.visits)
        
        def best():
            if root.children:
                node = max(root.children, key=lambda n : n.visits)
                return node.action
            actions = self.actions()
            return choice(actions)
        
        def backpropagate(node, result):
            if not node.parent: return 
            node.balance = node.balance + result 
            node.visits = node.visits + 1
            self.revert()
            backpropagate(node.parent, result)

        def expanded(node):
            return len(node.unvisited) == 0
        
        def traverse(node):
            while expanded(node):
                node = max(node.children, key=ucb)
                self.result(node.action)
            if self.terminal(): return node 
            else:
                action = node.unvisited[0]
                self.result(action)
                node.unvisited = node.unvisited[1:]
                child = self.Node(self, action, node)
                self.tree[self.zobrist] = child
                node.children.append(child)
                return child

        def rollout():
            backup = deepcopy(self.state)

            player = Rand(self.state, self.turn)
            while not player.terminal():
                action = player.decide()
                player.result(action)

            result = player.winner() # -1 = BLACK, 0 = DRAW, 1 = WHITE
            if result < 0: result = 1 if color == WHITE else -1 
            elif result > 0: result = -1 if color == BLACK else 1 
            else: result = -1

            self.state = backup
            return result
        
        def mcts():
            while time() - start < 0.5:
                leaf = traverse(root)
                result = rollout()
                self.t = self.t + 1
                backpropagate(leaf, result)
            return best()
        
        return mcts()
 
class MinMax(Agent):
    depth: int = 4
    def __init__(self):
        super().__init__()
        self.grid = [[20, -3, 11, 8, 8, 11, -3, 20],
    	             [-3, -7, -4, 1, 1, -4, -7, -3],
    	             [11, -4, 2, 2, 2, 2, -4, 11],
    	             [8, 1, 2, -3, -3, 2, 1, 8],
    	             [8, 1, 2, -3, -3, 2, 1, 8],
    	             [11, -4, 2, 2, 2, 2, -4, 11],
    	             [-3, -7, -4, 1, 1, -4, -7, -3],
    	             [20, -3, 11, 8, 8, 11, -3, 20]]
    
    def decide(self):
        player = self.turn 
        oponent = not self.turn 

        def utility():
            def mobility():
                good = len(self.actions())
                self.result(PASS)
                bad = len(self.actions())
                self.revert()
                if not good+bad: return 0
                elif good > bad: return 100*good/(good+bad)
                return -100*bad/(good+bad)
             
            def cellval(row, col):
                value = self.grid[row][col]
                return value
            
            def value():
                good = bad = 0
                for row, col in product(range(8), range(8)):
                    if self.state[row][col] == player: good += cellval(row, col)
                    if self.state[row][col] == oponent: bad += cellval(row, col)
                if not good+bad: return 0
                return 100*good/(good+bad) if good > bad else -100*bad/(good+bad)
            
            def closeness():
                good = bad = 0
                for row, col in [(0,0), (0,7), (7,0), (7,7)]:
                    if self.state[row][col] != None:
                        continue
                    for direction in DIRECTIONS:
                        nrow, ncol = shift((row, col), direction)
                        if 0 <= nrow < 8 and 0 <= ncol < 8:
                            if self.state[nrow][ncol] == player: bad += 1
                            elif self.state[nrow][ncol] == oponent: good += 1
                return good-bad
            
            def material():
                good = bad = 0
                for row, col in product(range(8), range(8)):
                    if self.state[row][col] == player: good += 1
                    elif self.state[row][col] == oponent: bad += 1
                if good == bad: return 0
                elif good > bad: return good/(good+bad)
                return -bad/(good+bad)
            
            def corners():
                good = bad = 0
                for row, col in [(0,0), (0,7), (7,0), (7,7)]:
                    if self.state[row][col] == player: good += 1
                    elif self.state[row][col] == oponent: bad += 1
                return good-bad
            
            if self.terminal():
                diff = material()
                if diff < 0: return -inf 
                elif diff > 0: return inf
                return 0
            return 10*material() + 20025*corners() + 10*value() + 79*mobility() + 4584*closeness()
        
        def comp(action):
            self.result(action)
            value = utility()
            self.revert()
            return value

        def maxvalue(depth, alpha, beta):
            if not depth or self.terminal(): return utility(), PASS 
            value = -inf
            actions = self.actions()
            if actions == [PASS]: return utility(), PASS
            if depth >= self.depth: actions.sort(key=comp, reverse=True)
            for action in actions:
                self.result(action)
                val, _ = minvalue(depth-1, alpha, beta)
                self.revert()
                if val >= value:
                    value, best = val, action 
                if value > beta: return value, best 
                alpha = max(alpha, value)
            return value, best

        def minvalue(depth, alpha, beta):
            if not depth or self.terminal(): return utility(), PASS  
            value = inf 
            actions = self.actions()
            if actions == [PASS]: return utility(), PASS
            for action in actions:
                self.result(action)
                val, _ = maxvalue(depth-1, alpha, beta)
                self.revert()
                if val <= value:
                    value, best = val, action
                if value < alpha: return value, best 
                beta = min(beta, value)
            return value, best
        
        _, action = maxvalue(self.depth, -inf, inf)
        return action
