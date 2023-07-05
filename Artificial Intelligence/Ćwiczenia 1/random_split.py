from random import shuffle
from dp_split import alg as dp

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

    # preprocess source
    source = open("pan-tadeusz.txt", "r", encoding="utf-8")
    output = open("pan-tadeusz-processed.txt", "w", encoding="utf-8")
    output_spaceless = open("pan-tadeusz-spaceless.txt", "w", encoding="utf-8")
    whitelist = set('aąbcćdeęfghijklłmnńoópqrsśtuvwxyzźż ')
    for line in source:
        line = line.lower()
        line = ''.join(filter(whitelist.__contains__, line))
        line = line.strip()
        if line != '':
            output.write(' '.join(line.split()) + '\n')
            output_spaceless.write(''.join(line.split()) + '\n')
    source.close()
    output_spaceless.close()
    output.close()
    
    # split process
    text = open("pan-tadeusz-spaceless.txt", "r", encoding="utf-8")
    rand_output = open("rand-output.txt", "w", encoding="utf-8")
    dp_output = open("dp-output.txt", "w", encoding="utf-8")
    for line in text:
        line = line.strip()
        out_rand = random_split(line, dictionary, maxS)
        out_dp = dp(line, dictionary, maxS)
        if out_rand != None and out_dp != None:
            rand_output.write(out_rand + '\n')
            dp_output.write(out_dp + '\n')
        else:
            rand_output.write('\n')
            dp_output.write('\n')
    text.close()
    rand_output.close()
    dp_output.close()

    # calculate % of correctly recreated lines
    output = open("pan-tadeusz-processed.txt", "r", encoding="utf-8")
    rand_output = open("rand-output.txt", "r", encoding="utf-8")
    dp_output = open("dp-output.txt", "r", encoding="utf-8")
    count = 0
    rand_count = 0
    dp_count = 0
    for (og, rand, dpek) in zip(output, rand_output, dp_output):
        og = og.strip()
        rand = rand.strip()
        dpek = dpek.strip()
        count += 1
        if rand == og:
            rand_count += 1
        if dpek == og:
            dp_count += 1

    # print results
    rand_result = "{:.2f}".format(rand_count/count * 100)
    dp_result = "{:.2f}".format(dp_count/count * 100)
    print(f'Result for random algorithm: {rand_result}%')
    print(f'Result for dp algorithm: {dp_result}%')

    # cleanup
    words.close()
    output.close()
    rand_output.close()
    dp_output.close()

# launch main
if __name__ == '__main__':
    main()
