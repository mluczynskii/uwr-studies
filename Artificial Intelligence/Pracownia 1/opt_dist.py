# Mateusz Łuczyński 331826
from math import inf 

def opt_dist(s, d): # O(n) where n is the length of s
    xs = [int(c) for c in s]
    pop_count = sum(xs) # number of 1's
    if d == 0:
        return pop_count # just turn off all 1's
    current = xs[0] # number of 1's inside the window
    l, r = 0, 1 # window [l, r)
    while r < d: # init window
        current = current + xs[r]
        r = r + 1
    res = inf # result accumulator
    while r <= len(xs):
        cost = d + pop_count - 2 * current # fixing the window + fixing everything outside
        if cost < res:
            res = cost
        if r < len(xs): 
            current = current - xs[l] + xs[r]
        l, r = l + 1, r + 1
    return res

def main():
    with open('zad4_input.txt', 'r') as inp, open('zad4_output.txt', 'w') as out:
        for line in inp:
            line = line.split(None)
            ans = opt_dist(line[0], int(line[1]))
            out.write(str(ans) + '\n')

if __name__ == "__main__":
    main()