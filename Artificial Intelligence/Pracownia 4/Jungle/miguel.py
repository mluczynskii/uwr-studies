# Agent utilizing P4Z3 algorithm
from lib import Agent, Player, Arbiter
from gwen import Gwen
from sys import maxsize as inf
from time import time

class Miguel(Agent):
    N: int = 10000
    def decide(self, state):
        t1 = time()
        player = self.player 
        oponent = Player.BLUE if player == Player.RED else Player.RED
        actions = Agent.actions(state, player)
        if not actions: return None
        K = self.N//len(actions)
        stats = {a : 0 for a in actions}
        for action in actions:
            snapshot = Agent.result(state, action)
            remaining = K 
            if not Agent.terminal(state):
                result = 0
                while remaining:
                    p1, p2 = Gwen(state), Gwen(state)
                    arbiter = Arbiter(p1, p2, state, oponent, K)
                    point, remaining, recap = arbiter.game()
                    Agent.rebuild(state, recap)
                    result = result + point 
                if player == Player.RED: result = -result 
                stats[action] = result
            else: stats[action] = inf
            Agent.revert(state, action, snapshot)
        t2 = time()
        #print(f'{t2-t1}s')
        return max(stats, key=stats.get)
