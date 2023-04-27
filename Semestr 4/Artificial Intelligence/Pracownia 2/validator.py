#!/usr/bin/env python
# -*- coding: UTF-8 -*-
'''
Prosta sprawdzarka. Przykłady użycia:

1. Uruchomienie wszystkich testów dla danego zadania:
  `python validator.py zad1 python rozwiazanie.py`

2. Uruchomienie wybranych testów
  `python validator.py --cases 1,3-5 zad1 a.out`

3. Urochomienie na innych testach
  `python validator.py --testset large_tests.yaml zad1 python rozwiazanie.py`

4. Wypisanie przykładowego wejścia/wyjścia:
  `python validator.py --show_example zad1`

5. Wypisanie informacji o rozwiązaniu:
  `python validator.py --verbose zad1 python rozwiazanie.py`

6. Wymuszenie użycia STDIN/STDOUT do komunikacji:
  `python validator.py --stdio zad1 python rozwiazanie.py`

7. Ustawienie mnożnika dla limitów czasowych:
  `python validator.py --timeout-multiplier 2.5 zad1 python rozwiazanie.py`


'''


import argparse
import os
import signal
import subprocess
import sys
import threading
import time
import math
import timeit

import numpy as np


import time

def time_consuming_function(N): 
    d = {} 
    r = [] 
    for i in range(N): 
        s = str(i) 
        d[s] = 4 * s 
        r.append(s) 
    res1 = max(d) 
    res2 = max(d, key=lambda x:d[x]) 
    res3 = s.find('999999') 
    return res1 and res2 and res3 

t0 = time.time()
r = time_consuming_function(500_000)

VERBOSE = False

TIME_MULTIPLIER = (time.time() - t0) / 0.4

print ('Estimated computer speed=', 1 / TIME_MULTIPLIER)
TIME_MULTIPLIER *= 1.1 # SAFETY BONUS :)



# Tests embedded into the validator.

DEFAULT_TESTSET = {'zad1': {'cases': [{'inp': '5 5\n'
                            '5\n'
                            '1 1 1\n'
                            '3\n'
                            '2 2\n'
                            '5\n'
                            '2 2\n'
                            '1 3\n'
                            '3 1\n'
                            '1 3\n'
                            '2 2\n',
                     'out': '#####\n#.#.#\n.###.\n##.##\n#####\n'},
                    {'inp': '9 9\n'
                            '1 1 1\n'
                            '5 1\n'
                            '1 1 1 1\n'
                            '5 1\n'
                            '6 1\n'
                            '7\n'
                            '6\n'
                            '1 3\n'
                            '2 4\n'
                            '4\n'
                            '1 2 1\n'
                            '8\n'
                            '1 4\n'
                            '7 1\n'
                            '5\n'
                            '5\n'
                            '4\n'
                            '6\n',
                     'out': '#...#...#\n'
                            '#####...#\n'
                            '#.#.#...#\n'
                            '#####...#\n'
                            '.######.#\n'
                            '..#######\n'
                            '..######.\n'
                            '..#..###.\n'
                            '.##.####.\n'},
                    {'inp': '6 10\n'
                            '3\n'
                            '1 1\n'
                            '4\n'
                            '1 3\n'
                            '6\n'
                            '6\n'
                            '1\n'
                            '3\n'
                            '1 1\n'
                            '5\n'
                            '2\n'
                            '3\n'
                            '3\n'
                            '3\n'
                            '2\n'
                            '1\n',
                     'out': '.###......\n'
                            '.#.#......\n'
                            '####......\n'
                            '...#.###..\n'
                            '...######.\n'
                            '....######\n'},
                    {'inp': '10 10\n'
                            '3\n'
                            '3\n'
                            '1\n'
                            '3\n'
                            '6\n'
                            '3\n'
                            '3\n'
                            '3 3\n'
                            '2 2\n'
                            '2 1\n'
                            '1\n'
                            '1 2\n'
                            '1 2\n'
                            '1 1\n'
                            '2 5\n'
                            '7\n'
                            '2 5\n'
                            '1\n'
                            '2\n'
                            '2\n',
                     'out': '....###...\n'
                            '....###...\n'
                            '.....#....\n'
                            '....###...\n'
                            '.######...\n'
                            '....###...\n'
                            '....###...\n'
                            '..###.###.\n'
                            '.##.....##\n'
                            '##.......#\n'},
                    {'inp': '10 10\n'
                            '4\n'
                            '6\n'
                            '3 4\n'
                            '4 5\n'
                            '4 5\n'
                            '5 4\n'
                            '5 2\n'
                            '6\n'
                            '6\n'
                            '2 2\n'
                            '3\n'
                            '5\n'
                            '9\n'
                            '10\n'
                            '2 4\n'
                            '5 3\n'
                            '6 3\n'
                            '9\n'
                            '5\n'
                            '3\n',
                     'out': '...####...\n'
                            '..######..\n'
                            '.###.####.\n'
                            '####.#####\n'
                            '####.#####\n'
                            '#####.####\n'
                            '.#####.##.\n'
                            '..######..\n'
                            '..######..\n'
                            '..##..##..\n'},
                    {'inp': '10 10\n'
                            '3 3\n'
                            '2 4 2\n'
                            '1 2 1\n'
                            '1 1\n'
                            '2 2\n'
                            '3 3\n'
                            '3 3\n'
                            '6\n'
                            '4\n'
                            '2\n'
                            '5\n'
                            '2 3\n'
                            '1 3\n'
                            '2 3\n'
                            '2 3\n'
                            '2 3\n'
                            '2 3\n'
                            '1 3\n'
                            '2 3\n'
                            '5\n',
                     'out': '.###..###.\n'
                            '##.####.##\n'
                            '#...##...#\n'
                            '#........#\n'
                            '##......##\n'
                            '###....###\n'
                            '.###..###.\n'
                            '..######..\n'
                            '...####...\n'
                            '....##....\n'},
                    {'inp': '14 10\n'
                            '4\n'
                            '1 1\n'
                            '2\n'
                            '1 2 1\n'
                            '10\n'
                            '1 2 1\n'
                            '2\n'
                            '2\n'
                            '2\n'
                            '2 2 2\n'
                            '2 2 2\n'
                            '8\n'
                            '6\n'
                            '2\n'
                            '1 2\n'
                            '3 3\n'
                            '1 2\n'
                            '2 1 2\n'
                            '1 12\n'
                            '1 12\n'
                            '2 1 2\n'
                            '1 2\n'
                            '3 3\n'
                            '1 2\n',
                     'out': '...####...\n'
                            '...#..#...\n'
                            '....##....\n'
                            '.#..##..#.\n'
                            '##########\n'
                            '.#..##..#.\n'
                            '....##....\n'
                            '....##....\n'
                            '....##....\n'
                            '##..##..##\n'
                            '##..##..##\n'
                            '.########.\n'
                            '..######..\n'
                            '....##....\n'},
                    {'inp': '10 15\n'
                            '4\n'
                            '1 1 6\n'
                            '1 1 6\n'
                            '1 1 6\n'
                            '4 9\n'
                            '1 1\n'
                            '1 1\n'
                            '2 7 2\n'
                            '1 1 1 1\n'
                            '2 2\n'
                            '4\n'
                            '1 2\n'
                            '1 1\n'
                            '5 1\n'
                            '1 2\n'
                            '1 1\n'
                            '5 1\n'
                            '1 1\n'
                            '4 1\n'
                            '4 1\n'
                            '4 2\n'
                            '4 1\n'
                            '4 1\n'
                            '4 2\n'
                            '4\n',
                     'out': '...####........\n'
                            '...#..#.######.\n'
                            '...#..#.######.\n'
                            '...#..#.######.\n'
                            '####..#########\n'
                            '#.............#\n'
                            '#.............#\n'
                            '##..#######..##\n'
                            '.#..#.....#..#.\n'
                            '..##.......##..\n'},
                    {'inp': '15 15\n'
                            '5\n'
                            '9\n'
                            '5 5\n'
                            '13\n'
                            '3 5 3\n'
                            '15\n'
                            '1 5 5 1\n'
                            '15\n'
                            '2 2\n'
                            '2 2\n'
                            '1 1\n'
                            '1 1\n'
                            '1 1\n'
                            '2 2\n'
                            '5\n'
                            '3\n'
                            '3 1\n'
                            '6\n'
                            '7\n'
                            '3 3 5\n'
                            '10 2\n'
                            '9 1\n'
                            '2 3 1 1\n'
                            '9 1\n'
                            '10 2\n'
                            '3 3 5\n'
                            '7\n'
                            '6\n'
                            '3 1\n'
                            '3\n',
                     'out': '.....#####.....\n'
                            '...#########...\n'
                            '..#####.#####..\n'
                            '.#############.\n'
                            '.###.#####.###.\n'
                            '###############\n'
                            '#.#####.#####.#\n'
                            '###############\n'
                            '.....##.##.....\n'
                            '....##...##....\n'
                            '....#.....#....\n'
                            '....#.....#....\n'
                            '....#.....#....\n'
                            '....##...##....\n'
                            '.....#####.....\n'},
                    {'inp': '15 15\n'
                            '5\n'
                            '2 2\n'
                            '1 1\n'
                            '1 1\n'
                            '4 4\n'
                            '2 2 1 2\n'
                            '1 3 1\n'
                            '1 1 1 1\n'
                            '2 7 2\n'
                            '4 1 5\n'
                            '2 1 1\n'
                            '1 1 2\n'
                            '1 1 1\n'
                            '2 5 2\n'
                            '3 4\n'
                            '4\n'
                            '2 2\n'
                            '1 5\n'
                            '1 2 2\n'
                            '5 2 1\n'
                            '2 1 1 2\n'
                            '1 3 1\n'
                            '1 1 6\n'
                            '1 3 1\n'
                            '2 1 2 2\n'
                            '4 2 1\n'
                            '1 1 1\n'
                            '1 3 2\n'
                            '2 2 3\n'
                            '4\n',
                     'out': '.....#####.....\n'
                            '....##...##....\n'
                            '....#.....#....\n'
                            '....#.....#....\n'
                            '.####.....####.\n'
                            '##..##...#...##\n'
                            '#.....###.....#\n'
                            '#.....#.#.....#\n'
                            '##..#######..##\n'
                            '.####..#.#####.\n'
                            '..##...#....#..\n'
                            '..#....#....##.\n'
                            '..#....#.....#.\n'
                            '..##.#####..##.\n'
                            '...###...####..\n'},
                    {'inp': '15 15\n'
                            '4\n'
                            '2 2\n'
                            '2 2\n'
                            '2 4 2\n'
                            '2 1 1 2\n'
                            '2 4 2\n'
                            '1 2\n'
                            '4 4 4\n'
                            '1 1 1 1 1 1\n'
                            '4 1 1 4\n'
                            '1 1 1\n'
                            '1 1 3\n'
                            '10\n'
                            '2 1\n'
                            '4 1\n'
                            '5 1\n'
                            '2 1 1 1\n'
                            '2 1 1 2\n'
                            '2 3 3\n'
                            '2 1\n'
                            '2 3 6\n'
                            '1 1 1 1 1\n'
                            '1 1 1 1 1\n'
                            '2 3 6\n'
                            '2 1\n'
                            '2 3 1\n'
                            '2 1 1 1\n'
                            '2 1 1 4\n'
                            '7\n'
                            '1 1\n',
                     'out': '.....####......\n'
                            '....##..##.....\n'
                            '...##....##....\n'
                            '..##.####.##...\n'
                            '.##..#..#..##..\n'
                            '##...####...##.\n'
                            '#............##\n'
                            '####.####.####.\n'
                            '#..#.#..#.#..#.\n'
                            '####.#..#.####.\n'
                            '.....#..#....#.\n'
                            '.....#..#...###\n'
                            '...##########..\n'
                            '..##........#..\n'
                            '####........#..\n'},
                    {'inp': '15 15\n'
                            '5\n'
                            '9\n'
                            '5 5\n'
                            '13\n'
                            '3 5 3\n'
                            '15\n'
                            '1 5 5 1\n'
                            '15\n'
                            '2 2\n'
                            '2 2\n'
                            '1 1\n'
                            '1 1\n'
                            '1 1\n'
                            '2 2\n'
                            '5\n'
                            '3\n'
                            '3 1\n'
                            '6\n'
                            '7\n'
                            '3 3 5\n'
                            '10 2\n'
                            '9 1\n'
                            '2 3 1 1\n'
                            '9 1\n'
                            '10 2\n'
                            '3 3 5\n'
                            '7\n'
                            '6\n'
                            '3 1\n'
                            '3\n',
                     'out': '.....#####.....\n'
                            '...#########...\n'
                            '..#####.#####..\n'
                            '.#############.\n'
                            '.###.#####.###.\n'
                            '###############\n'
                            '#.#####.#####.#\n'
                            '###############\n'
                            '.....##.##.....\n'
                            '....##...##....\n'
                            '....#.....#....\n'
                            '....#.....#....\n'
                            '....#.....#....\n'
                            '....##...##....\n'
                            '.....#####.....\n'}],
          'defaults': {'input_file': 'zad_input.txt',
                       'output_file': 'zad_output.txt',
                       'timeout': 60},
          'validator': 'perlines_validator'},
 'zad2': {'cases': [{'inp': 'WWWWWW\n'
                            'W.GWWW\n'
                            'W..WWW\n'
                            'W*K..W\n'
                            'W..B.W\n'
                            'W..WWW\n'
                            'WWWWWW\n',
                     'out': 33},
                    {'inp': 'WWWWWW\n'
                            'W....W\n'
                            'W.WK.W\n'
                            'W.B*.W\n'
                            'W.G*.W\n'
                            'W....W\n'
                            'WWWWWW\n',
                     'out': 16},
                    {'inp': 'WWWWWWWWW\n'
                            'WWW..WWWW\n'
                            'W.....B.W\n'
                            'W.W..WB.W\n'
                            'W.G.GWK.W\n'
                            'WWWWWWWWW\n',
                     'out': 41},
                    {'inp': 'WWWWWWWW\n'
                            'W......W\n'
                            'W.G**BKW\n'
                            'W......W\n'
                            'WWWWW..W\n'
                            'WWWWWWWW\n',
                     'out': 23},
                    {'inp': 'WWWWWWWWWWWW\n'
                            'WWWWWW.WWWWW\n'
                            'W....WWW...W\n'
                            'W.BB.....WKW\n'
                            'W.B.WGGG...W\n'
                            'W...WWWWWWWW\n'
                            'WWWWWWWWWWWW\n',
                     'out': 107},
                    {'inp': 'WWWWWWWW\n'
                            'WWW.GGKW\n'
                            'WWW.BB.W\n'
                            'WWWW.WWW\n'
                            'WWWW.WWW\n'
                            'WWWW.WWW\n'
                            'WWWW.WWW\n'
                            'W....WWW\n'
                            'W.W...WW\n'
                            'W...W.WW\n'
                            'WWW...WW\n'
                            'WWWWWWWW\n',
                     'out': 97},
                    {'inp': 'WWWWWW\n'
                            'WG..WW\n'
                            'WKBB.W\n'
                            'WW...W\n'
                            'WWW..W\n'
                            'WWWWGW\n'
                            'WWWWWW\n',
                     'out': 30},
                    {'inp': 'WWWWWWWWWWW\n'
                            'WWWWWWWG..W\n'
                            'WWWWWWWGW.W\n'
                            'WWWWWWWGW.W\n'
                            'W.K.B.B.B.W\n'
                            'W.W.W.W.WWW\n'
                            'W.......WWW\n'
                            'WWWWWWWWWWW\n',
                     'out': 89},
                    {'inp': 'WWWWWWWWW\n'
                            'WWW....WW\n'
                            'WWW.WWKWW\n'
                            'WWW.W.B.W\n'
                            'W.GGW.B.W\n'
                            'W.......W\n'
                            'W..WWWWWW\n'
                            'WWWWWWWWW\n',
                     'out': 78},
                    {'inp': 'WWWWWWWW\n'
                            'W.....WW\n'
                            'WKBBB.WW\n'
                            'W..WGGGW\n'
                            'WW....WW\n'
                            'WWWWWWWW\n',
                     'out': 50}],
          'defaults': {'input_file': 'zad_input.txt',
                       'output_file': 'zad_output.txt',
                       'timeout': 10},
          'validator': 'sokoban_validator'},
 'zad3': {'cases': [{'inp': 'WWWWWWW\n'
                            'W.....W\n'
                            'W.GBG.W\n'
                            'W.BGB.W\n'
                            'W.GBG.W\n'
                            'W.BGB.W\n'
                            'W..K..W\n'
                            'WWWWWWW\n',
                     'out': 1000000},
                    {'inp': 'WWWWWWWWW\n'
                            'WWW..WWWW\n'
                            'W.......W\n'
                            'WKB***G.W\n'
                            'W.......W\n'
                            'WWWWWWWWW\n',
                     'out': 1000000},
                    {'inp': 'WWWWWWW\n'
                            'WWW..WW\n'
                            'WWG.BWW\n'
                            'WWGB.WW\n'
                            'WWGB.WW\n'
                            'WWGB.WW\n'
                            'WWG.BWW\n'
                            'WW...KW\n'
                            'WWW...W\n'
                            'WWWWWWW\n',
                     'out': 1000000},
                    {'inp': 'WWWWWWWWWWWW\n'
                            'WWWWWWW....W\n'
                            'WWWWWWW.G..W\n'
                            'WWW..WWWG..W\n'
                            'W.B..B..G.WW\n'
                            'W.KBB.W.G.WW\n'
                            'WW....WWWWWW\n'
                            'WWWWWWWWWWWW\n',
                     'out': 1000000},
                    {'inp': 'WWWWWWWWWWWWW\n'
                            'W..........WW\n'
                            'W.WWWWWWW.KWW\n'
                            'W.W.........W\n'
                            'W.W..B...W..W\n'
                            'W.BB.WWWWW..W\n'
                            'WWW..W.W.GGGW\n'
                            'WWWWWW.W....W\n'
                            'WWWWWWWWWWWWW\n',
                     'out': 1000000},
                    {'inp': 'WWWWWWWWWWWWW\n'
                            'WWWW....WW..W\n'
                            'W..BBBGGGGBKW\n'
                            'W......WWW..W\n'
                            'W...WWWW.WWWW\n'
                            'WWWWWWWWWWWWW\n',
                     'out': 1000000},
                    {'inp': 'WWWWWWWWWW\n'
                            'WW....WWWW\n'
                            'W...B.WWWW\n'
                            'W..BB.WWWW\n'
                            'WWW.GWWWWW\n'
                            'WWWWGW.K.W\n'
                            'WWWWG..B.W\n'
                            'WWWWG.WWWW\n'
                            'WWWWWWWWWW\n',
                     'out': 1000000},
                    {'inp': 'WWWWWWWWW\n'
                            'WWW....WW\n'
                            'WWW..B.WW\n'
                            'WWWWWB.WW\n'
                            'WW.B.B.WW\n'
                            'WGGGGW.WW\n'
                            'W.....K.W\n'
                            'WW..W...W\n'
                            'WWWWWWWWW\n',
                     'out': 1000000},
                    {'inp': 'WWWWWWWWW\n'
                            'WWWWKWWWW\n'
                            'WWWWBWWWW\n'
                            'WW..G..WW\n'
                            'W..W.W..W\n'
                            'W.W...W.W\n'
                            'W.W...W.W\n'
                            'W.W...W.W\n'
                            'W..W.W..W\n'
                            'WW.B.B.WW\n'
                            'WWWG.GWWW\n'
                            'WWW...WWW\n'
                            'WWW...WWW\n'
                            'WWWWWWWWW\n',
                     'out': 1000000},
                    {'inp': 'WWWWWWWWWWWWWWW\n'
                            'WWWWWW.WWWWWWWW\n'
                            'W.....W....WWWW\n'
                            'WGWW..WBWW..WWW\n'
                            'W...W.....W..WW\n'
                            'WB..W.WWW..W..W\n'
                            'W.W......W..W.W\n'
                            'W.W.WWWW..W.W.W\n'
                            'WG.K....B.*.G.W\n'
                            'WWWWWWWWWWWWWWW\n',
                     'out': 1000000}],
          'defaults': {'input_file': 'zad_input.txt',
                       'output_file': 'zad_output.txt',
                       'timeout': 20},
          'validator': 'sokoban_validator'},
 'zad4': {'cases': [{'inp': '###########\n#BSSSSSSSS#\n###########\n',
                     'out': 150},
                    {'inp': '######################\n'
                            '#SSSSSSSS#SSS##SSSSSS#\n'
                            '#SSSSSSSSSSSS##SSSSSS#\n'
                            '#SSSSSS###SSSSSSSSS#B#\n'
                            '#SSSSSS###SSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#####SSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '######################\n',
                     'out': 150},
                    {'inp': '######################\n'
                            '#SSSSSSSS#SSSSSSSSSBS#\n'
                            '#SSSSSSSSSSSS##SSSSSS#\n'
                            '#SSSSSSSS#############\n'
                            '#SSSSSS###SSSSSSSSSSS#\n'
                            '#SSSSSS###SSSSSSSSSSS#\n'
                            '#SSSSSSSS#SSSSSSSSSSS#\n'
                            '##S#######SSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '######################\n',
                     'out': 150},
                    {'inp': '######################\n'
                            '#SSSSSSSS#SSS##SSSSSS#\n'
                            '#SSSSSSSSSSSS##SSSSSS#\n'
                            '#SSSSSS###SSSSSSSSS#B#\n'
                            '#SSSSSS###SSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#####SSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '######################\n',
                     'out': 150},
                    {'inp': '######################\n'
                            '#SSSSSSSS#SSS##SSSSBS#\n'
                            '#SSSSSSSS#SSS##SSSSSS#\n'
                            '#SSSSSS###SSSSSSSSSSS#\n'
                            '#SSSSSS###SSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#####SSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '######################\n',
                     'out': 150},
                    {'inp': '######################\n'
                            '#SSSSSSSSSSSSSSS#SSBS#\n'
                            '#SSBSSSSBSSSSSSS#SSSS#\n'
                            '#########S#######S####\n'
                            '#SSSSS#SSSSSSSSSSSSSB#\n'
                            '##SSS##SSSS###########\n'
                            '#SSSS#SSSSSSSSSSSSSSS#\n'
                            '#S##S###########SSSSS#\n'
                            '#SSSS#SSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSS###SS####SSS#\n'
                            '######################\n',
                     'out': 150},
                    {'inp': '######################\n'
                            '#SSSSSSSS#SSSSSSSSSBS#\n'
                            '#SSSSSSSSSSSS##SSSSSS#\n'
                            '#SSSSSSSS#############\n'
                            '###SSSSSS###SSSSSSSSB#\n'
                            '###SSSSSS#S#SSSSSSSSS#\n'
                            '#SSSSSSSS#SSSSSSSSSSS#\n'
                            '##S############SSS####\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '######################\n',
                     'out': 150},
                    {'inp': '######################\n'
                            '#SSSSSSSS#SSSSSSSSSBS#\n'
                            '#SSSSSSSSSSSS##SSSSSS#\n'
                            '#SSSSSSSS#############\n'
                            '#SSSSSS###SSSSSSSSSSS#\n'
                            '#SSSSSS###SSS#SSSSSSS#\n'
                            '#SSSSSSSS#SSS#SSSSSSS#\n'
                            '##S#######SSS####SSSS#\n'
                            '#SSSSSSSSSSSSSSS#SSSS#\n'
                            '#SSSSSSSSSSSSSSS##SSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSS#################\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSS#\n'
                            '######################\n',
                     'out': 150},
                    {'inp': '######################\n'
                            '#SSSSSSSSSSSSSSSSSSBS#\n'
                            '#SSBSSSSBSSSSSSSSSSSS#\n'
                            '#########S#######S####\n'
                            '#SSSSS#SSSSSSSSSSSSSB#\n'
                            '##SSS##SSSS###########\n'
                            '#SSSS#SSSSSSSSSSSSSSS#\n'
                            '#S##S###########SSSSS#\n'
                            '#SSSS#SSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSSS####SSSSSS#\n'
                            '######################\n',
                     'out': 150},
                    {'inp': '##########################\n'
                            '#SSSSSSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSS############S###\n'
                            '#SSSSSSSSSSSSSSSSS#SSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSS#SSSBSS#\n'
                            '##########SSSSSSSS#SBSSSS#\n'
                            '#SSSSSSSSSSSSSSSSS#SSSBSS#\n'
                            '##########################\n',
                     'out': 150},
                    {'inp': '#######################\n'
                            '#SSSSSSS#BSSSS#BSS#SSB#\n'
                            '#SSSSSSSSSSSSSSSSSSSSS#\n'
                            '#SSSSSSSSS#####SSSSSSS#\n'
                            '#SSSSSSSSS#####SSSSSSS#\n'
                            '#SSSSSSSSS#####SSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSSS#\n'
                            '#SS##########SSSSSSSSS#\n'
                            '#SS##########SSSSSSSSS#\n'
                            '#SS##########SSSSSSSSS#\n'
                            '#SSSSSSSSSSSSSSSSSSSSS#\n'
                            '#######################\n',
                     'out': 150}],
          'defaults': {'input_file': 'zad_input.txt',
                       'output_file': 'zad_output.txt',
                       'timeout': 10},
          'validator': 'komandos_validator'},
 'zad5': {'cases': [{'inp': '#####\n'
                            '#G  #\n'
                            '#   #\n'
                            '#  G#\n'
                            '#S# #\n'
                            '#   #\n'
                            '#SSS#\n'
                            '#####\n',
                     'out': 5},
                    {'inp': '#####\n'
                            '#G# #\n'
                            '# S #\n'
                            '#  G#\n'
                            '#S# #\n'
                            '#   #\n'
                            '#SSS#\n'
                            '#####\n',
                     'out': 7},
                    {'inp': '#####\n'
                            '#B#S#\n'
                            '#SSS#\n'
                            '#SSB#\n'
                            '#S#S#\n'
                            '#SSS#\n'
                            '#SSS#\n'
                            '#####\n',
                     'out': 9},
                    {'inp': '############\n'
                            '#   SSS#   #\n'
                            '#  #####   #\n'
                            '#    #   G #\n'
                            '#   SSS    #\n'
                            '############\n',
                     'out': 17},
                    {'inp': '############\n'
                            '#   SSS    #\n'
                            '########## #\n'
                            '#    G     #\n'
                            '# ##########\n'
                            '#   SSS    #\n'
                            '############\n',
                     'out': 23},
                    {'inp': '######################\n'
                            '#        #   ##S     #\n'
                            '#            ##      #\n'
                            '#      ###         #G#\n'
                            '#      ###           #\n'
                            '#                    #\n'
                            '#####         S      #\n'
                            '#                    #\n'
                            '######################\n',
                     'out': 12},
                    {'inp': '######################\n'
                            '#        #   ##S     #\n'
                            '#            ##      #\n'
                            '#      ###         #G#\n'
                            '#S     ###           #\n'
                            '#                    #\n'
                            '#####         S      #\n'
                            '# S                  #\n'
                            '######################\n',
                     'out': 26},
                    {'inp': '######################\n'
                            '#        #         G #\n'
                            '#            ##      #\n'
                            '#       S#############\n'
                            '#      ###           #\n'
                            '#      ###           #\n'
                            '#        #           #\n'
                            '## #######           #\n'
                            '#   S                #\n'
                            '#                    #\n'
                            '#                    #\n'
                            '#                    #\n'
                            '#                    #\n'
                            '#                    #\n'
                            '#                  S #\n'
                            '######################\n',
                     'out': 50},
                    {'inp': '######################\n'
                            '#        #   ##S     #\n'
                            '# S          ##      #\n'
                            '#    S ###         #G#\n'
                            '#      ###           #\n'
                            '#                    #\n'
                            '#####        SS      #\n'
                            '#S                   #\n'
                            '######################\n',
                     'out': 28},
                    {'inp': '######################\n'
                            '#        # SS##    G #\n'
                            '#        # SS##      #\n'
                            '#      ###           #\n'
                            '#      ###           #\n'
                            '#                    #\n'
                            '#####                #\n'
                            '#S                   #\n'
                            '######################\n',
                     'out': 26},
                    {'inp': '######################\n'
                            '#     S         #  G #\n'
                            '#  G    G       #    #\n'
                            '######### ####### ####\n'
                            '#     #             G#\n'
                            '##   ##    ###########\n'
                            '#    #               #\n'
                            '# ## ###########     #\n'
                            '#    #    S          #\n'
                            '#        ###  ####   #\n'
                            '######################\n',
                     'out': 22},
                    {'inp': '######################\n'
                            '#                  G #\n'
                            '#  G    G            #\n'
                            '######### ####### ####\n'
                            '#     #             G#\n'
                            '##   ##    ###########\n'
                            '#    #   S           #\n'
                            '# ## ###########     #\n'
                            '#    #    S          #\n'
                            '#S         ####      #\n'
                            '######################\n',
                     'out': 34},
                    {'inp': '######################\n'
                            '#                  G #\n'
                            '#  G    G            #\n'
                            '######### ####### ####\n'
                            '#     #   S         G#\n'
                            '##   ##    ###########\n'
                            '#    #   S           #\n'
                            '# ## ###########     #\n'
                            '#    #    S          #\n'
                            '#S         ####      #\n'
                            '######################\n',
                     'out': 34},
                    {'inp': '######################\n'
                            '#        #         G #\n'
                            '#            ##      #\n'
                            '#       S#############\n'
                            '#      ###          G#\n'
                            '#      ###           #\n'
                            '#        #           #\n'
                            '## #######           #\n'
                            '#   S                #\n'
                            '#                    #\n'
                            '#                  S #\n'
                            '######################\n',
                     'out': 22},
                    {'inp': '######################\n'
                            '#        #         G #\n'
                            '#            ##      #\n'
                            '#       S#############\n'
                            '#      ###           #\n'
                            '#      ###   #       #\n'
                            '#        #   #       #\n'
                            '## #######   ####S   #\n'
                            '#   S           #    #\n'
                            '#               ##   #\n'
                            '#                    #\n'
                            '#    #################\n'
                            '#                    #\n'
                            '#                    #\n'
                            '#                  S #\n'
                            '######################\n',
                     'out': 55},
                    {'inp': '######################\n'
                            '#  SS              G #\n'
                            '#  G    G            #\n'
                            '######### ####### ####\n'
                            '#     #             G#\n'
                            '##   ##    ###########\n'
                            '#    #   S           #\n'
                            '# ## ###########     #\n'
                            '#    #    S          #\n'
                            '#S         ####      #\n'
                            '######################\n',
                     'out': 38},
                    {'inp': '######################\n'
                            '#  SS              G #\n'
                            '#  G    G            #\n'
                            '######### ####### ####\n'
                            '#     #             G#\n'
                            '##   ##    ###########\n'
                            '#    #   S           #\n'
                            '# ## ###########     #\n'
                            '#    #    S      SS  #\n'
                            '#S         ####      #\n'
                            '######################\n',
                     'out': 39},
                    {'inp': '############\n'
                            '#SSSSSS#SSS#\n'
                            '#SS#####SSS#\n'
                            '#SSSS#SSSBS#\n'
                            '#SSSSSSSSSS#\n'
                            '############\n',
                     'out': 18},
                    {'inp': '############\n'
                            '#SSSSSS#SSS#\n'
                            '#SS#####SSS#\n'
                            '#SSSS#SSSBS#\n'
                            '#SSSSSS#SSS#\n'
                            '#SS##SSSSSS#\n'
                            '############\n',
                     'out': 20},
                    {'inp': '##################\n'
                            '#SSSSSSSSSSSSSSSS#\n'
                            '######SSSS###SSSS#\n'
                            '#SSSSSSSSS###SSSS#\n'
                            '#SSS###SSSSSSSSSS#\n'
                            '#SSS###SSSS#####S#\n'
                            '#SS####SSSS#SSSSS#\n'
                            '#SSSSSSSSSS#SSSSB#\n'
                            '#SSSSSSSSSS#SSSSB#\n'
                            '##################\n',
                     'out': 28},
                    {'inp': '############\n'
                            '#SSSSSS#SSS#\n'
                            '#SS#####SSS#\n'
                            '#SSSS#SSSBS#\n'
                            '#BBSSSS#SSS#\n'
                            '#SS##SSSSSS#\n'
                            '#SS##SSSSSS#\n'
                            '############\n',
                     'out': 18,
                     'timeout': 200}],
          'defaults': {'input_file': 'zad_input.txt',
                       'output_file': 'zad_output.txt',
                       'timeout': 20},
          'validator': 'komandos_validator'}}

# Custom comparison functions

# Sokoban logic
class Sokoban(object):
    EMPTY = 0
    GOAL = 1
    WALL = 2
    KEEPER = 3
    BOX = 4
    BOX_ON_GOAL = 5
    KEEPER_ON_GOAL = 6
    char2id = {
        '.': EMPTY,
        'G': GOAL,
        'W': WALL,
        'K': KEEPER,
        'B': BOX,
        '*': BOX_ON_GOAL,
        '+': KEEPER_ON_GOAL
        }
    id2char = {v: k for k, v in char2id.items()}

    MOVES = {
        "U": (-1, 0),
        "D": (1, 0),
        "L": (0, -1),
        "R": (0, 1),
        }

    @staticmethod
    def read_map(lines):
        map = []
        for line in lines:
            map.append([Sokoban.char2id[c] for c in line.strip()])
        map = np.array(map)
        b_locs = set(zip(*(map == Sokoban.BOX_ON_GOAL).nonzero()))
        for br, bc in b_locs:
            map[br, bc] = Sokoban.GOAL
        b_locs.update(zip(*(map == Sokoban.BOX).nonzero()))
        b_locs = frozenset(b_locs)

        k_loc = tuple(zip(*(map == Sokoban.KEEPER_ON_GOAL).nonzero()))
        if k_loc:
            assert not tuple(zip(*(map == Sokoban.KEEPER).nonzero()))
            map[k_loc[0][0], k_loc[0][1]] = Sokoban.GOAL
        else:
            k_loc = zip(*(map == Sokoban.KEEPER).nonzero())
        k_loc, = k_loc

        map[map > Sokoban.WALL] = Sokoban.EMPTY
        return map, (k_loc, b_locs)

    @staticmethod
    def map_to_string(empty_map, state):
        k_loc, b_locs = state
        strings = [[Sokoban.id2char[i] for i in r] for r in empty_map]
        if empty_map[k_loc[0], k_loc[1]] == Sokoban.GOAL:
            strings[
                k_loc[0]][k_loc[1]] = Sokoban.id2char[Sokoban.KEEPER_ON_GOAL]
        else:
            strings[k_loc[0]][k_loc[1]] = Sokoban.id2char[Sokoban.KEEPER]
        for br, bc in b_locs:
            if empty_map[br, bc] == Sokoban.GOAL:
                strings[br][bc] = Sokoban.id2char[Sokoban.BOX_ON_GOAL]
            else:
                strings[br][bc] = Sokoban.id2char[Sokoban.BOX]
        return '\n'.join([''.join(r) for r in strings])

    @staticmethod
    def keeper_moves(empty_map, state, moves="UDLR"):
        k_loc, b_locs = state
        kr, kc = k_loc
        for m in moves:
            dr, dc = Sokoban.MOVES[m]
            nl = (kr + dr, kc + dc)
            n = empty_map[nl[0], nl[1]]
            if n != Sokoban.WALL:
                if nl not in b_locs:  # move to GOAL/EMPTY
                    yield m, (nl, b_locs)
                else:  # maybe move BOX
                    nbl = (nl[0] + dr, nl[1] + dc)
                    if empty_map[nbl[0], nbl[1]] <= Sokoban.GOAL:
                        nb_locs = set(b_locs)
                        nb_locs.remove(nl)
                        nb_locs.add(nbl)
                        yield m, (nl, frozenset(nb_locs))

    @staticmethod
    def moves_to_strings(empty_map, state, k_moves):
        if VERBOSE:
            print(Sokoban.map_to_string(empty_map, state))
        for m in k_moves:
            possible_moves = Sokoban.keeper_moves(empty_map, state, m)
            possible_moves = tuple(possible_moves)
            if not possible_moves:
                fail("Keeper move %s is illegal!" % (m,))
            (_, state), = possible_moves
            if VERBOSE:
                print("Keeper move %s" % (m,))
                print(Sokoban.map_to_string(empty_map, state))
        return state


def sokoban_validator(case, process_out, message=""):
    k_moves = whitespace_normalize(process_out)
    max_num_moves = int(whitespace_normalize(case['out']))

    empty_map, state = Sokoban.read_map(case['inp'].strip().split('\n'))
    state = Sokoban.moves_to_strings(
        empty_map, state, k_moves)

    g_locs = set(zip(*(empty_map == Sokoban.GOAL).nonzero()))
    solved = g_locs == state[1]
    if solved:
        if len(k_moves) > max_num_moves:
            fail("Level solved, but path is too long!")
        else:
            if VERBOSE:
                print(message + "Level solved!")
            return {'num_steps': len(k_moves)}
    else:
        fail("All moves were legal, but puzzle not solved")


# Komandos

class Maze:
    _dirs = {'U': (0, -1), 'D': (0, 1), 'R': (1, 0), 'L': (-1, 0)}

    def __init__(self, maze_str):
        self.m = []
        self.goals = set()
        self.starts = set()
        self.states = set()

        for x in maze_str.split('\n'):
            x = x.strip()
            if x:
                self.m.append(list(x))

        for y in range(len(self.m)):
            raw = self.m[y]
            for x in range(len(raw)):
                if self.m[y][x] == 'S':
                    self.start = (x, y)
                    self.starts.add((x, y))
                if self.m[y][x] == 'G':
                    self.goals.add((x, y))
                if self.m[y][x] == 'B':
                    self.start = (x, y)
                    self.starts.add((x, y))
                    self.goals.add((x, y))
                if self.m[y][x] != '#':
                    self.states.add((x, y))

    def to_str(self, s):
        lines = []
        for y, line in enumerate(self.m):
            cs = []
            for x, c in enumerate(line):
                if (x, y) in s:
                    if c in 'BG':
                        c = 'B'
                    else:
                        c = 'S'
                else:
                    if c in 'S':
                        c = ' '
                cs.append(c)
            lines.append(''.join(cs))
        return '\n'.join(lines)

    def do(self, state, action):
        dx, dy = Maze._dirs[action]
        x, y = state
        if self.m[y+dy][x+dx] != '#':
            return (x+dx, y+dy)
        else:
            return state

    def do_belief(self, states, action):
        return {self.do(s, action) for s in states}


def komandos_validator(case, process_out, message=""):
    k_moves = whitespace_normalize(process_out)
    max_num_moves = int(whitespace_normalize(case['out']))

    maze = Maze(case['inp'])

    states = maze.starts

    if VERBOSE:
        print(maze.to_str(states))

    for c in k_moves:
        states = maze.do_belief(states, c)
        if VERBOSE:
            print(maze.to_str(states))

    solved_fraction = len(states & maze.goals) / len(states)

    if solved_fraction == 1:
        if len(k_moves) > max_num_moves:
            fail(message + "Level solved, but path is too long!")
        else:
            if VERBOSE:
                print("Level solved!")
            return {'num_moves': len(k_moves)}
    else:
        fail("%sLevel solved in %f%% only!" % (
             message, solved_fraction * 100.0))


# Comparison functions

class ValidatorException(Exception):
    pass


def fail(message):
    raise ValidatorException(message)


def compare(returned, expected, message="Contents"):
    if returned != expected:
        fail('%s differ. Got: "%s", expceted: "%s"' % (
             message, returned, expected))


def whitespace_relaxed_validator(case, process_out):
    """
    Compare two strings ignoring whitespaces and trailing newlines.
    """
    ref_out = whitespace_normalize(case['out'])
    process_out = whitespace_normalize(process_out)
    return compare(process_out, ref_out, "Outputs")


def perlines_validator(case, process_out, line_compare_fun=compare):
    """
    Compare two strings line by line, ignoring whitespaces.
    """
    ref_lines = whitespace_normalize(case['out']).split('\n')
    process_lines = whitespace_normalize(process_out).split('\n')
    compare(len(process_lines), len(ref_lines), "Number of lines")
    for lnum, (proc_line, ref_line) in enumerate(
            zip(process_lines, ref_lines)):
        line_compare_fun(proc_line, ref_line, "Line %d contents" % (lnum + 1,))


# Comparison function utils
def ensure_unicode(obj):
    if sys.version_info[0] == 3:
        if isinstance(obj, str):
            return obj
        elif isinstance(obj, bytes):
            return obj.decode('utf8')
        else:
            return str(obj)
    else:
        if isinstance(obj, unicode):
            return obj
        elif isinstance(obj, str):
            return obj.decode('utf8')
        else:
            return unicode(obj)
    return obj


def whitespace_normalize(obj):
    """
    Optionally convert to string and normalize newline and space characters.
    """
    string = ensure_unicode(obj)
    lines = string.replace('\r', '').strip().split('\n')
    lines = [' '.join(l.strip().split()) for l in lines]
    return '\n'.join(lines)


# Subprocess handling utils
try:  # py3
    from shlex import quote as shellquote
except ImportError:  # py2
    from pipes import quote as shellquote


if os.name == 'nt':
    def shellquote(arg):
        return subprocess.list2cmdline([arg])

    def kill_proc(process):
        if process.poll() is None:
            print('Killing subprocess.')
            subprocess.call(['taskkill', '/F', '/T', '/PID', str(process.pid)])
else:
    def kill_proc(process):
        if process.poll() is None:
            print('Killing subprocess.')
            os.killpg(os.getpgid(process.pid), signal.SIGTERM)


def run_and_score_case(program, defaults, case_def, validator, timeout_multiplier):
    opts = dict(defaults)
    opts.update(case_def)
    opts['timeout'] *= timeout_multiplier
    process_out, elapsed_time = run_case(program, **opts)
    if VERBOSE:
        print("Got output:")
        print(process_out)
    measurements = validator(opts, process_out)
    measurements = measurements or {}
    measurements['time'] = elapsed_time
    return measurements


def run_case(program, inp, out=None,
             input_file='<stdin>', output_file='<stdout>',
             timeout=1.0):
    del out  # unused
    inp = ensure_unicode(inp)
    if inp[-1] != '\n':
        inp += '\n'
    inp = inp.encode('utf8')

    if input_file != '<stdin>':
        with open(input_file, 'wb') as in_f:
            in_f.write(inp)
        inp = None
    try:
        if output_file != '<stdout>':
            os.remove(output_file)
    except:
        pass

    stdin = subprocess.PIPE if input_file == '<stdin>' else None
    stdout = subprocess.PIPE if output_file == '<stdout>' else None
    process_out = ''
    process = None

    try:
        if os.name == 'nt':
            kwargs = {}
        else:
            kwargs = {'preexec_fn': os.setpgrp}

        process = subprocess.Popen(
            program, shell=True, stdin=stdin, stdout=stdout, **kwargs)
        start = time.time()
        if timeout > 0:
            timer = threading.Timer(timeout * TIME_MULTIPLIER, kill_proc, [process])
            timer.start()

        process_out, _ = process.communicate(inp)
        elapsed = time.time() - start
    except Exception as e:
        fail(str(e))
    finally:
        if process:
            kill_proc(process)
        if timeout > 0:
            timer.cancel()
    if process.poll() != 0:
        fail("Bad process exit status: %d" % (process.poll(),))

    if output_file != '<stdout>':
        if not os.path.isfile(output_file):
            fail("Output file %s does not exist" % (output_file, ))
        with open(output_file, 'rb') as out_f:
            process_out = out_f.read()
    process_out = process_out.decode('utf8')

    return process_out, elapsed


def ensure_newline_string(obj):
    obj = ensure_unicode(obj)
    if obj[-1] != '\n':
        obj += '\n'
    return obj


def show_example(defaults, case_def):
    opts = dict(defaults)
    opts.update(case_def)
    print("Input is passed using %s and contains:" % (opts['input_file'],))
    print(ensure_newline_string(opts["inp"]))
    print("Output is expected in %s with contents:" % (opts['output_file'],))
    print(ensure_newline_string(opts["out"]))


def get_argparser():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--cases', default='',
        help='Comma-separated list of test cases to run, e.g. 1,2,3-6.')
    parser.add_argument(
        '--testset', default='',
        help='Path to a YAML test set definition.')
    parser.add_argument(
        '--show_example', default=False, action='store_true',
        help='Print a sample input/output pair.')
    parser.add_argument(
        '--timeout-multiplier', '-tm',
        help='Multiply timeout by provided amount, e.g. 2.13')
    parser.add_argument(
        '--verbose', default=False, action='store_true',
        help='Print more information about solutions.')
    parser.add_argument(
        '--stdio', default=False, action='store_true',
        help='Use stdin/stdout for communication.')
    parser.add_argument(
        'problem',
        help='Problem form this homework, one of: %s.' %
        (', '.join(sorted(DEFAULT_TESTSET.keys())),))
    parser.add_argument(
        'program', nargs=argparse.REMAINDER,
        help='Program to execute, e.g. python solution.py.')
    return parser


def get_program(args):
    return ' '.join([shellquote(a) for a in args])


def get_cases(problem_def, cases):
    problem_cases = problem_def['cases']
    if cases == '':
        for case in enumerate(problem_cases, 1):
            yield case
        return
    cases = cases.strip().split(',')
    for case in cases:
        if '-' not in case:
            case = int(case) - 1
            if case < 0:
                raise Exception('Bad case number: %d' % (case + 1,))
            yield case + 1, problem_cases[case]
        else:
            low, high = case.split('-')
            low = int(low) - 1
            high = int(high)
            if low < 0 or high > len(problem_cases):
                raise Exception('Bad case range: %s' % (case,))
            for case in range(low, high):
                yield case + 1, problem_cases[case]


def simple_benchmark():
    product = 1.0
    for counter in range(1, 1000, 1):
        for dex in list(range(1, 360, 1)):
            angle = math.radians(dex)
            product *= math.sin(angle)**2 + math.cos(angle)**2

    sys.stdout.write('.')
    sys.stdout.flush()
    return product


def start_benchmark():
        print('Executing CPU benchmark. It may take some time ...')
        print('0%', '.'*96, '100%')
        sys.stdout.write('|')
        sys.stdout.flush()

        result = timeit.repeat('validator2.simple_benchmark()', setup='import validator2', number=10, repeat=10)
        result = list(sorted(result))
        result = sum(result[:3])/3.0
        return (result - 1.0) / 1.5 + 1.0  # some tweaks


if __name__ == '__main__':
    """
    benchmark_file = '.benchmark_result'
    benchmark_result = 1.0
    if not os.path.isfile(benchmark_file):
        benchmark_result = start_benchmark()
        print('|\nResult = ', benchmark_result)
        with open(benchmark_file, 'w') as outFile:
            outFile.write(str(benchmark_result))
    else:
        with open(benchmark_file) as inputFile:
            benchmark_result = float(inputFile.readline())
    """
    
    parser = get_argparser()
    args = parser.parse_args()
    VERBOSE = args.verbose

    if args.testset:
        with open(args.testset) as testset_f:
            testset = yaml.load(testset_f)
    else:
        testset = DEFAULT_TESTSET
    if args.problem not in testset:
        print('Problem not known: %s. Choose one of %s.' %
              (args.problem, ', '.join(sorted(testset.keys()))))

    problem_def = testset[args.problem]
    problem_validator = eval(problem_def['validator'])
    problem_cases = get_cases(problem_def, args.cases)
    program = get_program(args.program)

    if args.show_example:
        show_example(problem_def['defaults'], next(problem_cases)[1])
        sys.exit()

    failed_cases = []
    ok_cases = []
    for case_num, case_def in problem_cases:
        print('Running case %d... ' % (case_num,), end='')
        try:
            timeout_multiplier = float(args.timeout_multiplier) if args.timeout_multiplier and float(args.timeout_multiplier) > 1 else 1
            if args.stdio:
                case_def['input_file'] = '<stdin>'
                case_def['output_file'] = '<stdout>'
            case_meas = run_and_score_case(
                program, problem_def['defaults'], case_def, problem_validator, timeout_multiplier * TIME_MULTIPLIER)
            ok_cases.append((case_num, case_meas))
            print('OK!')
        except ValidatorException as e:
            failed_cases.append(case_num)
            print('Failed:')
            print(str(e))

    print('\nValidation result: %d/%d cases pass.\n' % (
        len(ok_cases), len(ok_cases) + len(failed_cases)))

    tot_meas = {}
    for nc, meas in ok_cases:
        for k, v in meas.items():
            tot_meas[k] = tot_meas.get(k, 0) + v
    for k, v in tot_meas.items():
        print("For passing cases total %s: %s" % (k, v))

    if failed_cases:
        print('\nSome test cases have failed. '
              'To rerun the failing cases execute:')
        misc_opts = ''
        if args.verbose:
            misc_opts = ' --verbose'
        if args.timeout_multiplier:
            misc_opts += ' --timeout-multiplier ' + args.timeout_multiplier
        if args.testset:
            misc_opts = '%s --testset %s' % (
                misc_opts, shellquote(args.testset),)
        cases_opt = '--cases ' + ','.join([str(fc) for fc in failed_cases])
        print('python validator2.py%s %s %s %s' %
              (misc_opts, cases_opt, args.problem, program))

