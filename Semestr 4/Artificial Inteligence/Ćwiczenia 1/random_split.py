from random import shuffle

def random_split(line, dictionary, maxS):
    # recursion base 
    if line == '':
        return ''

    # find max prefix length
    n = len(line)
    jump = min(n+1, maxS)

    # get possible prefixes
    possible = []
    for i in range(1, jump):
        s = line[0:i]
        if s in dictionary:
            possible.append(s)
    
    # if impossible return None 
    if possible == []:
        return None

    # if possible, shuffle prefixes and try to find a good one
    shuffle(possible)
    for pref in possible:
        n = len(pref)
        # recursive call on suffix
        suf = random_split(line[n:], dictionary, maxS)
        # if suffix can be split, return
        if suf != None:
            if suf == '':
                return pref
            return pref + ' ' + suf 
        # otherwise continue with other prefixes
        
    # if all options exhausted, return None
    return None
    

def main():
    # init dictionary
    words = open("words.txt", "r", encoding="utf-8")
    dictionary = {}
    maxS = -1
    for word in words:
        word = word.strip()
        dictionary[word] = True
        if len(word) > maxS:
            maxS = len(word)
    
    # split process
    text = open("pan_tadeusz.txt", "r", encoding="utf-8")
    rand_output = open("rand_output.txt", "w")
    for line in text:
        line = line.strip()
        out = random_split(line, dictionary, maxS)
        rand_output.write(out + '\n')

    # cleanup
    words.close()
    text.close()
    rand_output.close()

# launch main
if __name__ == '__main__':
    main()
