# simple MinMax algorithm
from lib import Agent, Player, Piece
from sys import maxsize as inf

class Miles(Agent):
    depth: int = 3

    def value(self, piece):
        val, _ = piece.value 
        return val if piece != Piece.RAT else 5

    def utility(self, state, player, oponent):

        def army():
            _, positions = state 
            y, x = (0,3) if player == Player.RED else (8,3)
            good = 0
            for piece in positions[player].keys():
                row, col = positions[player][piece]
                good = good + (abs(row - y) + abs(col - x))
            y, x = (8,3) if player == Player.RED else (0,3)
            bad = 0
            for piece in positions[oponent].keys():
                row, col = positions[oponent][piece]
                bad = bad + (abs(row - y) + abs(col - x))
            if good < bad: value = 100*(12*8-good)/(12*8)
            elif good > bad: value = -100*(12*8-bad)/(12*8)
            else: value = 0
            return value

        def nearest():
            _, positions = state 
            y, x = (0,3) if player == Player.RED else (8,3)
            good = inf 
            for piece in positions[player].keys():
                row, col = positions[player][piece]
                dist = abs(row - y) + abs(col - x)
                good = min(good, dist)
            y, x = (8,3) if player == Player.RED else (0,3)
            bad = inf 
            for piece in positions[oponent].keys():
                row, col = positions[oponent][piece]
                dist = abs(row - y) + abs(col - x)
                bad = min(bad, dist)
            if bad == 0: return -10000
            elif good == 0: return 10000
            if good < bad: value = 100*(12-good)/12
            elif good > bad: value = -100*(12-bad)/12
            else: value = 0
            return value
            
        def material():
            good = bad = 0
            _, positions = state 
            for piece in positions[player].keys():
                good = good + self.value(piece)
            for piece in positions[oponent].keys():
                bad = bad + self.value(piece)
            if good == 0: return -10000
            elif bad == 0: return 10000
            if good > bad: value = 100*good/(good+bad) 
            elif bad < good: value = -100*bad/(good+bad)
            else: value = 0
            return value 
        
        return army() + material() + nearest()
    
    def speculate(self, state, action, player, oponent):
        snapshot = Agent.result(state, action)
        value = self.utility(state, player, oponent)
        Agent.revert(state, action, snapshot)
        return value
    
    def decide(self, state):
        player = self.player 
        oponent = Player.RED if player == Player.BLUE else Player.BLUE
        
        def maxvalue(state, depth, alpha, beta):
            if Agent.terminal(state) or not depth: return None, self.utility(state, player, oponent)
            value = -inf
            best = None 
            actions = Agent.actions(state, player)
            if not actions:
                return best, value
            if depth == self.depth: actions.sort(key=lambda a : self.speculate(state, a, player, oponent), reverse=True)
            for action in actions:
                snapshot = Agent.result(state, action)
                _, result = minvalue(state, depth-1, alpha, beta)
                if result >= value:
                    best, value = action, result  
                Agent.revert(state, action, snapshot)
                if value > beta:
                    return best, value
                alpha = max(alpha, value)
            return best, value 
        
        def minvalue(state, depth, alpha, beta):
            if Agent.terminal(state) or not depth: return None, self.utility(state, player, oponent)
            value = inf
            best = None 
            actions = Agent.actions(state, oponent)
            if not actions:
                return best, value
            for action in actions:
                snapshot = Agent.result(state, action)
                _, result = maxvalue(state, depth-1, alpha, beta)
                if result < value:
                    best, value = action, result  
                Agent.revert(state, action, snapshot)
                if value <= alpha:
                    return best, value 
                beta = min(beta, value)
            return best, value 
        
        best, _ = maxvalue(state, self.depth, -inf, inf)
        return best