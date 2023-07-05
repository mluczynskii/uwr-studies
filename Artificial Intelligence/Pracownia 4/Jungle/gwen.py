from lib import Agent
from random import choice

class Gwen(Agent):
    def decide(self, state):
        actions = Agent.actions(state, self.player)
        if not actions: return None
        return choice(actions)