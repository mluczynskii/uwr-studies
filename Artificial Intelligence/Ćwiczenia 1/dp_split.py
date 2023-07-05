# Mateusz Łuczyński 331826
def d(s: str) -> int:
    n = len(s)
    return n * n 

def alg(s: str, dict, maxS) -> int:
    n = len(s)
    dp = [(0, None)] + [(-1, None) for x in range(n-1)]
    for x in range(n):
        y = max(x - maxS + 1, 0)
        while x - y + 1 > 0:
            sub = s[y : (x+1)]
            if sub in dict:
                weight = d(sub)
                if y == 0:
                    pref = 0
                    val = weight 
                else:
                    pref = dp[y-1][0]
                    val = dp[y-1][0] + weight 
                if pref != -1 and val > dp[x][0]:
                        dp[x] = (val, y-1)
            y += 1
    res = ""
    backtrack = dp[n-1][1]
    end = n
    while True:
        if backtrack == None:
            return None
        sub = s[(backtrack+1) : end]
        res = sub + " " + res
        if backtrack == -1:
            break 
        end = backtrack+1
        backtrack = dp[backtrack][1]
    return res

def main():
    f = open("polish_words.txt", "r", encoding="utf-8")
    maxS = -1
    dict = {}
    for word in f:
        word = word.strip()
        n = len(word)
        if n > maxS:
            maxS = n 
        dict[word] = True
    f.close()
    inp = open("zad2_input.txt", "r", encoding="utf-8")
    out = open("zad2_output.txt", "w", encoding="utf-8")
    for line in inp:
        line = line.strip()
        s = alg(line, maxS, dict)
        out.write(s + '\n')
    inp.close()
    out.close()

if __name__ == "__main__":
    main()

