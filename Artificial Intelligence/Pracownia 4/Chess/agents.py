from lib import Agent 
from random import choice 
from sys import maxsize as inf
import chess
import chess.polyglot
import chess.gaviota

class Random(Agent):
    def decide(self):
        actions = self.actions()
        return choice(actions)
    
class Magnus(Agent):
    depth: int = 3
    def __init__(self):
        super().__init__()
        self.openings = chess.polyglot.open_reader('')
        self.endings = chess.gaviota.open_tablebase('')

    def decide(self):
        player = self.color
        oponent = chess.BLACK if self.color else chess.WHITE
        board = self.state

        def utility():
            def material():
                good = board.occupied_co[player]
                bad = board.occupied_co[oponent]
                pawns = chess.popcount(good & board.pawns) - chess.popcount(bad & board.pawns)
                knights = chess.popcount(good & board.knights) - chess.popcount(bad & board.knights)
                bishops = chess.popcount(good & board.bishops) - chess.popcount(bad & board.bishops)
                rooks = chess.popcount(good & board.rooks) - chess.popcount(bad & board.rooks)
                queens = chess.popcount(good & board.queens) - chess.popcount(bad & board.queens)
                return pawns + 3*knights + 3*bishops + 5*rooks + 9*queens

            def mobility():
                return len(self.actions()) 
            
            def castling():
                good = int(board.has_kingside_castling_rights(player)) + int(board.has_queenside_castling_rights(player))
                bad = int(board.has_kingside_castling_rights(oponent)) + int(board.has_queenside_castling_rights(oponent))
                return good - bad 
            
            def safety():
                bad = 0
                col, row = chess.square_file(board.king(player)), chess.square_rank(board.king(player))
                if player == chess.WHITE: bad = -row 
                else: bad = 7-row
                adjacent = [(i, j) for i in (col-1, col, col+1) for j in (row-1, row, row+1)]
                for i, j in adjacent:
                    attackers = len(board.attackers(oponent, chess.square(i, j)))
                    bad -= attackers 
                return bad

            def checks():
                value = len(board.checkers())
                if board.turn == player: return -value 
                else: return value
                
            if self.state.outcome():
                winner = self.state.outcome().winner
                if winner == oponent: return -inf 
                elif winner == player: return inf 
                else: return 0
            else:
                return safety() + material() + mobility() + castling() + checks()
            
        def comp(action):
            self.result(action)
            value = utility()
            self.revert()
            return value 

        def maxvalue(depth, alpha, beta):
            if not depth or self.state.outcome(): return utility(), None
            value = -inf
            actions = self.actions()
            if depth >= self.depth: actions.sort(key=comp, reverse=True)
            for move in actions:
                self.result(move)
                val, _ = minvalue(depth-1, alpha, beta)
                if val >= value:
                    value, best = val, move 
                self.revert()
                if value > beta: return value, best  
                alpha = max(value, alpha)
            return value, best 
        
        def minvalue(depth, alpha, beta):
            if not depth or self.state.outcome(): return utility(), None
            value = inf 
            for move in self.actions():
                self.result(move)
                val, _ = maxvalue(depth-1, alpha, beta)
                if val <= value:
                    value, best = val, move 
                self.revert()
                if value < alpha: return value, best 
                beta = min(value, beta)
            return value, best 
        
        op_entry = self.openings.get(board)
        wdl, dtm = self.endings.get_wdl(board), self.endings.get_dtm(board)
        if op_entry: 
            return op_entry.move
        elif wdl and dtm:
            for move in self.actions():
                self.result(move)
                nwdl, ndtm = self.endings.get_wdl(board), self.endings.get_dtm(board)
                self.revert()
                if nwdl and ndtm and ndtm < ndtm:
                    return move
        else:
            _, best = maxvalue(self.depth, -inf, inf)
            return best 
        
        
        
        
