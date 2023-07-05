from abc import ABC, abstractmethod
import chess 

class Agent(ABC):
    color = None 
    def __init__(self):
        self.state = chess.Board()

    def actions(self):
        return list(self.state.legal_moves)
    
    def result(self, action):
        self.state.push(action)

    def revert(self):
        _ = self.state.pop()

    def reset(self):
        self.state = chess.Board()

    @abstractmethod
    def decide(self):
        pass 