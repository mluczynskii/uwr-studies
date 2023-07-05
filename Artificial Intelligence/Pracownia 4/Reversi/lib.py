from abc import ABC, abstractmethod
from typing import Tuple
from queue import LifoQueue
from itertools import product
from csv import reader

Color = bool 
COLORS = [WHITE, BLACK] = [True, False]

Direction = Tuple[int,int]
DIRECTIONS = [UP, DOWN, LEFT, RIGHT, UPLEFT, UPRIGHT, DOWNLEFT, DOWNRIGHT] = [(-1,0), (1,0), (0,-1), (0,1), (-1,-1), (-1,1), (1,-1), (1,1)]

PASS = (-1,-1), []

def shift(position, direction):
    return (position[0] + direction[0], position[1] + direction[1])

class Agent(ABC):
    def __init__(self, state=None, turn=None):
        if state and turn: 
            self.state = state
            self.turn = turn
        else: 
            self.state = Agent.initial()
            self.turn = WHITE 
        self.stack = LifoQueue(maxsize=0)

    @staticmethod
    def initial():
        board = [[None for _ in range(8)] for _ in range(8)]
        board[3][4] = board[4][3] = WHITE 
        board[3][3] = board[4][4] = BLACK 
        return board 
    
    def captures(self, move):
        def accumulate(direction):
            dest, acc = move, []
            while True:
                dest = (row, col) = shift(dest, direction)
                if not 0 <= row < 8 or not 0 <= col < 8 or self.state[row][col] == None:
                    return [] 
                if self.state[row][col] == self.turn:
                    return acc  
                acc.append(dest)
        result = []
        for direction in DIRECTIONS:
            result = result + accumulate(direction)
        return result
    
    def result(self, action):
        move, captures = action 
        if action != PASS:
            row, col = move 
            self.state[row][col] = self.turn
            for row, col in captures:
                self.state[row][col] = self.turn  
        self.stack.put(action)
        self.turn = not self.turn 

    def revert(self):
        action = self.stack.get()
        move, captures = action 
        if action != PASS:
            row, col = move 
            self.state[row][col] = None 
            for row, col in captures:
                self.state[row][col] = self.turn 
        self.turn = not self.turn 
    
    def actions(self):
        result = []
        for row, col in product(range(8), range(8)):
            if self.state[row][col] == None:
                move = row, col 
                captures = self.captures(move)
                if captures: 
                    action = move, captures 
                    result.append(action)
        if not result:
            return [PASS]
        return result
    
    def terminal(self):
        actions = self.actions()
        if actions == [PASS]:
            self.turn = not self.turn 
            actions = self.actions()
            self.turn = not self.turn 
            if actions == [PASS]: return True
        return False 
    
    def winner(self):
        white = black = 0
        for row, col in product(range(8), range(8)):
            if self.state[row][col] == WHITE: white = white + 1
            elif self.state[row][col] == BLACK: black = black + 1
        if white > black: return 1
        elif black > white: return -1
        return 0
    
    def reset(self):
        self.state = Agent.initial()
        self.turn = WHITE 
        self.stack = LifoQueue(maxsize=0)
    
    @abstractmethod
    def decide(self):
        pass 
