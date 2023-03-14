from itertools import combinations
from enum import Enum
from collections import defaultdict

class Hierarchy(Enum):
    HIGH_CARD = 1
    ONE_PAIR = 2
    TWO_PAIRS = 3
    THREE_OF_A_KIND = 4
    STRAIGHT = 5
    FLUSH = 6
    FULL_HOUSE = 7
    FOUR_OF_A_KIND = 8
    STRAIGHT_FLUSH = 9

class Color(Enum):
    CLUB = 1
    HEART = 2
    DIAMOND = 3
    SPADE = 4

class Face(Enum):
    TWO = 2
    THREE = 3
    FOUR = 4
    FIVE = 5
    SIX = 6
    SEVEN = 7
    EIGHT = 8
    NINE = 9
    TEN = 10
    JACK = 11
    QUEEN = 12
    KING = 13
    ACE = 14

def hierarchy(cards):
    cards = sorted(cards, reverse=True, key=lambda card : card[0].value)
    colors = defaultdict(lambda: 0)
    order = [1 for i in range(5)]
    values = defaultdict(lambda: 0)

    prev = cards[0]
    colors[prev[1]] += 1
    values[prev[0]] += 1
    index = 1
    for card in cards[1:]:
        colors[card[1]] += 1
        if card[0].value + 1 == prev[0].value:
            order[index] = order[index - 1] + 1
        values[card[0]] += 1
        prev = card
        index += 1

    val_count = values.values()
    color_count = colors.values()
    c, o, v = max(color_count), max(order), max(val_count)

    if c == 5 and o == 5:
        return Hierarchy.STRAIGHT_FLUSH
    
    if v == 4:
        return Hierarchy.FOUR_OF_A_KIND
    
    if (2 in val_count) and (3 in val_count):
        return Hierarchy.FULL_HOUSE
    
    if c == 5:
        return Hierarchy.FLUSH
    
    if o == 5:
        return Hierarchy.STRAIGHT
    
    if 3 in val_count:
        return Hierarchy.THREE_OF_A_KIND
    
    pair_count = 0
    for val in val_count:
        if val == 2:
            pair_count += 1
    if pair_count == 2:
        return Hierarchy.TWO_PAIRS
    
    if 2 in val_count:
        return Hierarchy.ONE_PAIR
    
    return Hierarchy.HIGH_CARD

def main():
    print('Blotkarz: ')
    b_deck = [(face, color) for face in Face if face.value <= 10 for color in Color]
    b_hands = list(combinations(b_deck, 5))
    b_dictionary = defaultdict(lambda : 0)
    print(f'Wszystkich możliwości jest: {len(b_hands)}')
    for hand in b_hands:
        x = hierarchy(hand)
        b_dictionary[x] += 1
    for name in Hierarchy:
        print(f'{name}: {b_dictionary[name]}')
    
    print('\nFigurant')
    f_deck = [(face, color) for face in Face if face.value >= 11 for color in Color]
    f_hands = list(combinations(f_deck, 5))
    f_dictionary = defaultdict(lambda : 0)
    print(f'Wszystkich możliwości jest: {len(f_hands)}')
    for hand in f_hands:
        x = hierarchy(hand)
        f_dictionary[x] += 1
    for name in Hierarchy:
        print(f'{name}: {f_dictionary[name]}')
        
    mianownik = len(b_hands) * len(f_hands)
    f_sums = [0 for _ in range(len(Hierarchy))]
    for name in [x for x in Hierarchy if x.value >= 2]:
        f_sums[name.value-1] = f_sums[name.value-2] + f_dictionary[name]
    licznik = 0
    for name in [x for x in Hierarchy if x.value >= 2]:
        licznik += b_dictionary[name] * f_sums[name.value-2]
    print(f'Prawdopodobieństwo wynosi: {licznik}/{mianownik}')

if __name__ == "__main__":
    main()
