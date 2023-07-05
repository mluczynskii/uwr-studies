from itertools import product
from copy import copy
from time import time
from rich import print

def intersects(A, B):
    n, (ay1, ax1) = A
    (ay2, ax2) = (ay1+n-1, ax1+n-1)
    m, (by1, bx1) = B 
    (by2, bx2) = (by1+m-1, bx1+m-1)
    if ay1 <= by2 and ax1 <= bx2 and ax2 >= bx1 and ay2 >= by1:
        return True 
    return False 

def legal(state, action):
    n, (row, col) = action 
    if not 0 <= row < 70 or not 0 <= col < 70: return False 
    elif not 0 <= row+n-1 < 70 or not 0 <= col+n-1 < 70: return False 
    for square in state.items():
        if intersects(action, square): return False 
    return True 

# Modifies state, assumes action is legal
def result(state, action):
    n, anchor = action 
    state[n] = anchor 

# Modifies state
def revert(state, action):
    n, _ = action 
    del state[n]

colors = ["green3", "spring_green3", "green_yellow", 
          "dark_turquoise", "turquoise2", "green1", 
          "spring_green2", "deep_pink4", "medium_spring_green", 
          "orange_red1", "cyan1", "orange1", "purple3", 
          "blue_violet", "gold3", "medium_purple4", 
          "slate_blue3", "royal_blue1", "chartreuse4",
          "bright_magenta", "steel_blue", "steel_blue3", 
          "cornflower_blue", "dark_sea_green4"]

def draw(state):
    board = [['.' for _ in range(70)] for _ in range(70)]
    for square in state.items():
        n, (row, col) = square
        char = chr(n + 64) 
        for y, x in product(range(row,row+n), range(col, col+n)):
            board[y][x] = char
    for row in range(70):
        for col in range(70):
            char = board[row][col]
            if char == '.': 
                print('.', end='')
                continue
            index = ord(char)-65
            color = colors[index]
            print(f'[{color}]{char}', end='')
        print()


def value(state):
    area = 70**2
    for n in state.keys():
        area = area - n**2
    return area 

def shift(anchor):
    row, col = anchor 
    if col+1 < 70: return row, col+1
    elif row+1 < 70: return row+1, 0
    return None 

def greedy():
    optimal, state = {}, {24 : (0, -1)}
    n = 24
    begin = time()
    while True:
        if time()-begin > 10: break
        if n == 24: 
            anchor = shift(state[24])
            if not anchor: break 
        else: anchor = (0, 0)
        action = n, anchor
        while anchor and not legal(state, action):
            anchor = shift(anchor)
            action = n, anchor
        if not anchor:
            n = n - 1
            continue 
        result(state, action)
        n = n - 1
        if not n:
            if value(state) < value(optimal):
                optimal = copy(state)
            n = 24
            state = {24 : state[24]}
    print(value(optimal))
    draw(optimal)

if __name__ == '__main__':
    greedy()


