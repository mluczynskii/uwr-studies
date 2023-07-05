#!/usr/bin/env python
# -*- coding: UTF-8 -*-
'''
Prosta sprawdzarka. Przykady użycia:

1. uruchomienie wszystkich testów dla danego zadania:
python validator.py zad1 python rozwiazanie.py

2. uruchomienie wybranych testów
python validator.py --cases 1,3-5 zad1 a.out

3. Wypisanie przykadowego wejścia/wyjścia:
python validator.py --show_example zad1
'''


import argparse
import os
import signal
import subprocess
import threading
import gzip
import sys
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


TIME_MULTIPLIER = (time.time() - t0) / 0.4

print ('Estimated computer speed=', 1 / TIME_MULTIPLIER)
TIME_MULTIPLIER *= 1.1 # SAFETY BONUS :)


# Tests embedded into the validator.

DEFAULT_TESTSET = {'zad1': {'cases': [{'inp': 'black g8 h1 c4', 'out': 10},
                    {'inp': 'black b4 f3 e8\n', 'out': '6\n'},
                    {'inp': 'white a1 e3 b7', 'out': 9},
                    {'inp': 'black h7 a2 f2', 'out': 6},
                    {'inp': 'black a2 e4 a4', 'out': 8}],
          'defaults': {'input_file': 'zad1_input.txt',
                       'output_file': 'zad1_output.txt',
                       'timeout': 10},
          'validator': 'whitespace_relaxed_validator'},
 'zad2': {'cases': [{'inp': 'księgapierwsza\n'
                            'gospodarstwo\n'
                            'powrótpaniczaspotkaniesiępierwszewpokoikudrugieustołuważnasędziegonaukaogrzecznościpodkomorzegouwagipolitycznenadmodamipocząteksporuokusegoisokołażalewojskiegoostatniwoźnytrybunałurzutokanaówczesnystanpolitycznylitwyieuropy\n'
                            'litwoojczyznomojatyjesteśjakzdrowie\n'
                            'ileciętrzebacenićtentylkosiędowie\n'
                            'ktocięstraciłdziśpięknośćtwąwcałejozdobie\n'
                            'widzęiopisujębotęskniępotobie\n'
                            'pannoświętacojasnejbroniszczęstochowy\n'
                            'iwostrejświeciszbramietycogródzamkowy\n'
                            'nowogródzkiochraniaszzjegowiernymludem\n'
                            'jakmniedzieckodozdrowiapowróciłaścudem\n'
                            'gdyodpłaczącejmatkipodtwojąopiekę\n'
                            'ofiarowanymartwąpodniosłempowiekę\n'
                            'izarazmogłempieszodotwychświątyńprogu\n'
                            'iśćzawróconeżyciepodziękowaćbogu\n'
                            'taknaspowróciszcudemnaojczyznyłono\n'
                            'tymczasemprzenośmojąduszęutęsknioną\n'
                            'dotychpagórkówleśnychdotychłąkzielonych\n'
                            'szerokonadbłękitnymniemnemrozciągnionych\n'
                            'dotychpólmalowanychzbożemrozmaitem\n'
                            'wyzłacanychpszenicąposrebrzanychżytem\n'
                            'gdziebursztynowyświerzopgrykajakśniegbiała\n'
                            'gdziepanieńskimrumieńcemdzięcielinapała\n'
                            'awszystkoprzepasanejakbywstęgąmiedzą\n'
                            'zielonąnaniejzrzadkacichegruszesiedzą\n'
                            'śródtakichpólprzedlatynadbrzegiemruczaju\n'
                            'napagórkuniewielkimwebrzozowymgaju\n'
                            'stałdwórszlacheckizdrzewaleczpodmurowany\n'
                            'świeciłysięzdalekapobielaneściany\n'
                            'tymbielszeżeodbiteodciemnejzieleni\n',
                     'out': 'księga pierwsza\n'
                            'gospodarstwo\n'
                            'powrót panicza spotkanie się pierwsze w pokoiku '
                            'drugie u stołu ważna sędziego nauka o grzeczności '
                            'podkomorzego uwagi polityczne nad modami początek '
                            'sporu o kusego i sokoła żale wojskiego ostatni '
                            'woźny trybunału rzut oka na ówczesny stan '
                            'polityczny litwy i europy\n'
                            'litwo ojczyznom o jaty jesteś jak zdrowie\n'
                            'ile cię trzeba cenić ten tylko się dowie\n'
                            'kto cię stracił dziś piękność twą w całej '
                            'ozdobie\n'
                            'widzę i opisuję bo tęsknię po tobie\n'
                            'panno święta co jasnej bronisz częstochowy\n'
                            'i w ostrej świecisz bramie ty co gród zamkowy\n'
                            'nowogródzki ochraniasz z jego wiernym ludem\n'
                            'jak mnie dziecko do zdrowia powróciłaś cudem\n'
                            'gdy od płaczącej matki pod twoją opiekę\n'
                            'ofiarowany martwą podniosłem powiekę\n'
                            'i zaraz mogłem pieszo do twych świątyń progu\n'
                            'iść zawrócone życie podziękować bogu\n'
                            'tak nas powrócisz cudem na ojczyzny łono\n'
                            'tymczasem przenoś moją duszę utęsknioną\n'
                            'do tych pagórków leśnych do tych łąk zielonych\n'
                            'szeroko nad błękitnym niemnem rozciągnionych\n'
                            'do tych pól malowanych zbożem rozmaitem\n'
                            'wyzłacanych pszenicą posrebrzanych żytem\n'
                            'gdzie bursztynowy świerzop gry kajak śnieg biała\n'
                            'gdzie panieńskim rumieńcem dzięcielina pała\n'
                            'a wszystko przepasane jakby wstęgą miedzą\n'
                            'zieloną na niej z rzadka ciche grusze siedzą\n'
                            'śród takich pól przed laty nad brzegiem ruczaju\n'
                            'na pagórku niewielkim we brzozowym gaju\n'
                            'stał dwór szlachecki z drzewa lecz podmurowany\n'
                            'świeciły się z daleka pobielane ściany\n'
                            'tym bielsze że odbite od ciemnej zieleni\n'}],
          'defaults': {'input_file': 'zad2_input.txt',
                       'output_file': 'zad2_output.txt',
                       'timeout': 60},
          'validator': 'lambda opts, out: perlines_validator(opts, out, '
                       'zad2_line_compare)'},
 'zad4': {'cases': [{'inp': '0010001000 5\n'
                            '0010001000 4\n'
                            '0010001000 3\n'
                            '0010001000 2\n'
                            '0010001000 1\n'
                            '0010001000 0\n'
                            '0010101000 5\n'
                            '0010101000 4\n'
                            '0010101000 3\n'
                            '0010101000 2\n'
                            '0010101000 1\n'
                            '0010101000 0\n',
                     'out': '3\n4\n3\n2\n1\n2\n2\n3\n2\n3\n2\n3\n'},
                    {'inp': '0000000001 1\n'
                            '0000000010 1\n'
                            '1000000000 1\n'
                            '0100000000 1\n',
                     'out': '0\n0\n0\n0\n'}],
          'defaults': {'input_file': 'zad4_input.txt',
                       'output_file': 'zad4_output.txt',
                       'timeout': 10},
          'validator': 'perlines_validator'},
 'zad5': {'cases': [{'inp': '7 7\n7\n7\n7\n7\n7\n7\n7\n7\n7\n7\n7\n7\n7\n7\n',
                     'out': '#######\n'
                            '#######\n'
                            '#######\n'
                            '#######\n'
                            '#######\n'
                            '#######\n'
                            '#######\n'},
                    {'inp': '7 7\n2\n2\n7\n7\n2\n2\n2\n2\n2\n7\n7\n2\n2\n2\n',
                     'out': '..##...\n'
                            '..##...\n'
                            '#######\n'
                            '#######\n'
                            '..##...\n'
                            '..##...\n'
                            '..##...\n'},
                    {'inp': '7 7\n2\n2\n7\n7\n2\n2\n2\n4\n4\n2\n2\n2\n5\n5\n',
                     'out': '##.....\n'
                            '##.....\n'
                            '#######\n'
                            '#######\n'
                            '.....##\n'
                            '.....##\n'
                            '.....##\n'},
                    {'inp': '7 7\n7\n6\n5\n4\n3\n2\n1\n1\n2\n3\n4\n5\n6\n7\n',
                     'out': '#######\n'
                            '.######\n'
                            '..#####\n'
                            '...####\n'
                            '....###\n'
                            '.....##\n'
                            '......#\n'},
                    {'inp': '7 7\n7\n5\n3\n1\n1\n1\n1\n1\n2\n3\n7\n3\n2\n1\n',
                     'out': '#######\n'
                            '.#####.\n'
                            '..###..\n'
                            '...#...\n'
                            '...#...\n'
                            '...#...\n'
                            '...#...\n'}],
          'defaults': {'input_file': 'zad5_input.txt',
                       'output_file': 'zad5_output.txt',
                       'timeout': 30},
          'validator': 'perlines_validator'}}



# Custom comparison functions
zad2_words = None
def load_word_list():
    global zad2_words
    if zad2_words:
        return zad2_words
    print("Loading wordlist...", end='')
    zad2_words = set()
    with gzip.open('zad2_words.txt.gz', 'rb') as gf:
        for line in gf:
            line = line.strip()
            if line:
                zad2_words.add(line.decode('utf8'))
    return zad2_words


def zad2_line_compare(returned, expected, message="Contents"):
    ret = compare(returned.replace(' ', ''), expected.replace(' ', ''),
                  message + ' string without spaces differs!')
    if ret:
        return ret
    expected_score = sum([len(w)**2 for w in expected.split()])
    returned_words = returned.split()
    returned_score = sum([len(w)**2 for w in returned_words])
    if returned_score < expected_score:
        return message + " split is suboptimal!"
    bad_words = set(returned_words) - load_word_list()
    if bad_words:
        return message + " has unknown words: " + str(bad_words)
    return None


# Comparison functions
def compare(returned, expected, message="Contents"):
    if returned != expected:
        return '%s differ. Got: "%s", expceted: "%s"' % (
            message, returned, expected)
    return None


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
    ret = compare(len(process_lines), len(ref_lines), "Number of lines")
    if ret:
        return ret
    for lnum, (proc_line, ref_line) in enumerate(
            zip(process_lines, ref_lines)):
        ret = line_compare_fun(proc_line, ref_line,
                               "Line %d contents" % (lnum + 1,))
        if ret:
            return ret
    return None


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


def run_and_score_case(program, defaults, case_def, validator):
    opts = dict(defaults)
    opts.update(case_def)
    process_out = run_case(program, **opts)
    return validator(opts, process_out)


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
        if timeout > 0:
            timer = threading.Timer(timeout * TIME_MULTIPLIER, kill_proc, [process])
            timer.start()

        process_out, _ = process.communicate(inp)
    except Exception as e:
        raise
        return str(e)
    finally:
        if process:
            kill_proc(process)
        if timeout > 0:
            timer.cancel()
    if process.poll() != 0:
        return "Bad process exit status: %d" % (process.poll(),)

    if output_file != '<stdout>':
        with open(output_file, 'rb') as out_f:
            process_out = out_f.read()
    process_out = process_out.decode('utf8')

    return process_out


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
        '--show_example', default=False, action='store_true',
        help='Print a sample input/output pair.')
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


if __name__ == '__main__':
    parser = get_argparser()
    args = parser.parse_args()

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
    for case_num, case_def in problem_cases:
        print('Running case %d... ' % (case_num,), end='')
        case_ret = run_and_score_case(
            program, problem_def['defaults'], case_def, problem_validator)
        if case_ret is None:
            print('OK!')
        else:
            failed_cases.append(case_num)
            print('Failed:')
            print(case_ret)

    if failed_cases:
        print('\nSome test cases have failed. '
              'To rerun the failing cases execute:')
        testset_opt = ''
        cases_opt = '--cases ' + ','.join([str(fc) for fc in failed_cases])
        print('python validator.py%s %s %s %s' %
              (testset_opt, cases_opt, args.problem, program))
