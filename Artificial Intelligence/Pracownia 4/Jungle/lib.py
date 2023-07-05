from enum import Enum 
from dataclasses import dataclass, astuple
from abc import ABC, abstractmethod
from itertools import product
from sys import maxsize as inf
from random import choice, shuffle

class Field(Enum):
    MEADOW = '.'
    TRAP = '#'
    CAVE = '*'
    POND = '~'
    @staticmethod
    def fromstr(c: str):
        for field in Field:
            if field.value == c: return field 

class Piece(Enum):
    RAT = 1, 'R'
    CAT = 2, 'C'
    DOG = 3, 'D'
    WOLF = 4, 'W'
    PANTHER = 5, 'J'
    TIGER = 6, 'T'
    LION = 7, 'L'
    ELEPHANT = 8, 'E'
    @staticmethod
    def fromstr(c):
        for piece in Piece:
            _, code = piece.value 
            if c.upper() == code: return piece 

class Player(Enum):
    BLUE = 0
    RED = 1

class Direction(Enum):
    UP = (-1, 0)
    DOWN = (1, 0)
    LEFT = (0, -1)
    RIGHT = (0, 1)
    @staticmethod
    def shift(pos, direction):
        offset = direction.value
        return (pos[0] + offset[0], pos[1] + offset[1])

@dataclass 
class Cell():
    field: Field 
    occupied: bool 
    player: Player = None 
    piece: Piece = None

class Agent(ABC):
    player: Player = None
    def __init__(self, state=None):
        self.state = state if state != None else Agent.initial()
        
    @staticmethod
    def beats(a, b):
        if a == Piece.RAT and b == Piece.ELEPHANT: return True 
        elif a == Piece.ELEPHANT and b == Piece.RAT: return False 
        (aval, _), (bval, _) = a.value, b.value
        return aval >= bval 
    
    @staticmethod
    def initial():
        xs = ['L.#*#.T',
              '.D.#.C.',
              'R.J.W.E',
              '.~~.~~.',
              '.~~.~~.',
              '.~~.~~.',
              'e.w.j.r',
              '.c.#.d.',
              't.#*#.l',]
        positions = {player : {} for player in Player}
        board = [[None for _ in range(7)] for _ in range(9)]
        for row, col in product(range(9), range(7)):
            c = xs[row][col]
            if c in ('.', '*', '#', '~'):
                field = Field.fromstr(c)
                board[row][col] = Cell(field, False)
            else:
                player = Player.BLUE if c.isupper() else Player.RED
                piece = Piece.fromstr(c)
                positions[player][piece] = row, col
                board[row][col] = Cell(Field.MEADOW, True, player, piece)
        board[0][3].player = Player.BLUE
        board[8][3].player = Player.RED
        state = board, positions 
        return state 
    
    @staticmethod
    def legal(board, src, direction, piece, player):
        row, col = src 
        dest = (nrow, ncol) = Direction.shift(src, direction)
        if not 0 <= nrow < 9 or not 0 <= ncol < 7:
            return False
        cell = board[nrow][ncol]
        if cell.field == Field.POND and piece not in (Piece.RAT, Piece.LION, Piece.TIGER): 
            return False
        if piece == Piece.RAT:
            if board[row][col].field == Field.POND and cell.occupied and cell.field != Field.POND: return False
        elif piece in (Piece.TIGER, Piece.LION):
            while cell.field == Field.POND:
                dest = (nrow, ncol) = Direction.shift(dest, direction)
                cell = board[nrow][ncol]
                if cell.occupied and cell.player != player: return False 
        if cell.field == Field.CAVE and cell.player == player: return False
        if not cell.occupied or (cell.field == Field.TRAP and cell.player != player): return True 
        if cell.player == player: return False 
        return Agent.beats(piece, cell.piece)
    
    @staticmethod 
    def destination(board, piece, src, direction):
        dest = (nrow, ncol) = Direction.shift(src, direction)
        cell = board[nrow][ncol]
        if piece in (Piece.TIGER, Piece.LION):
            while cell.field == Field.POND:
                dest = (nrow, ncol) = Direction.shift(dest, direction)
                cell = board[nrow][ncol]
        return dest
    
    @staticmethod
    def actions(state, player):
        acc = []
        board, positions = state 
        for piece in positions[player].keys():
            src = positions[player][piece]
            for direction in Direction:
                if Agent.legal(board, src, direction, piece, player):
                    dest = Agent.destination(board, piece, src, direction)
                    action = (src, dest)
                    acc.append(action)
        if not acc: return None 
        shuffle(acc)
        return acc
    
    def reset(self):
        self.player = None
        self.state = Agent.initial()
    
    # Modifies state
    @staticmethod 
    def result(state, action):
        if not action: return None 
        board, positions = state 
        (row, col), (nrow, ncol) = action 
        start = board[row][col]
        end = board[nrow][ncol]

        snapshot = astuple(start), astuple(end)

        if end.occupied:
            del positions[end.player][end.piece]
        positions[start.player][start.piece] = nrow, ncol

        board[nrow][ncol] = Cell(end.field, True, start.player, start.piece) 
        board[row][col] = Cell(start.field, False)
        return snapshot
    
    # Modifies state
    @staticmethod
    def revert(state, action, snapshot):
        if not action or not snapshot: return 
        board, positions = state 
        (row, col), (nrow, ncol) = action
        (sfield, socc, splay, spiece), (efield, eocc, eplay, epiece) = snapshot 

        if eocc: positions[eplay][epiece] = nrow, ncol
        positions[splay][spiece] = row, col

        board[row][col] = Cell(sfield, socc, splay, spiece)
        board[nrow][ncol] = Cell(efield, eocc, eplay, epiece)

    # Modifies state
    @staticmethod 
    def rebuild(state, recap):
        for info in reversed(recap):
            action, snapshot = info 
            Agent.revert(state, action, snapshot)

    @staticmethod
    def terminal(state):
        board, positions = state
        if not positions[Player.RED] or not positions[Player.BLUE]: 
            return True  
        blue, red = board[0][3], board[8][3]
        if red.occupied or blue.occupied: 
            return True
        return False

    @abstractmethod
    def decide(self, state):
        pass # return action

class Arbiter():
    def __init__(self, blue, red, state=None, turn=None, limit=inf, verbose=False):
        self.limit = limit
        blue.player, red.player = Player.BLUE, Player.RED
        blue.state = red.state = self.state = Agent.initial() if not state else state 

        if turn != None: self.turn = turn  
        else: self.turn = choice([Player.BLUE,Player.RED]) 

        self.agents = (blue, red) 
        if verbose: print(Arbiter.draw(self.state))
        self.verbose = verbose

    def game(self):
        moves, recap = 0, []
        while True:
            player = self.agents[self.turn.value]
            oponent = self.agents[not self.turn.value] 

            action = player.decide(self.state)
            if not action:
                point = self.turn != Player.BLUE
                if self.verbose: print(f'P1 {"wins" if point else "loses"} by forfeit ({moves} actions total)')
                return 2*point-1, self.limit, recap
            
            moves = moves + 1
            self.limit = self.limit - 1
            snapshot = Agent.result(self.state, action)
            player.state = oponent.state = self.state 
            if snapshot:
                info = (action, snapshot)
                recap.append(info)
            if self.verbose: print(Arbiter.draw(self.state))

            if Agent.terminal(self.state):
                point = self.turn == Player.BLUE 
                if self.verbose: print(f'P1 {"wins" if point else "loses"} ({moves} actions total)')
                return 2*point-1, self.limit, recap 
            
            if not self.limit: # draw
                if self.verbose: print(f'Draw ({moves} actions total)')
                return 0, 0, recap 
            
            self.turn = Player.BLUE if self.turn == Player.RED else Player.RED

    @staticmethod 
    def draw(state):
        result = ''
        board, _ = state 
        for row in range(9):
            for col in range(7):
                cell = board[row][col]
                if (piece := cell.piece) != None:
                    _, c = piece.value
                    if cell.player == Player.RED: c = c.lower()
                else: c = cell.field.value
                result += c 
            result += '\n'
        return result 

    
 