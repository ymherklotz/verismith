#!/usr/bin/env python3

import os
import re
from pathlib import Path
import itertools
import subprocess

def iterdir(currdir):
    return [x for x in currdir.iterdir() if x.is_dir()]

def identify_vivado(current, name):
    v2018 = re.compile(".*2018.2")
    v2017 = re.compile(".*2017.4")
    v20161 = re.compile(".*2016.1")
    v20162 = re.compile(".*2016.2")

    if v2018.match(name):
        current[3] = 1
    elif v2017.match(name):
        current[2] = 1
    elif v20162.match(name):
        current[1] = 1
    elif v20161.match(name):
        current[0] = 1

    return current

def identify_general(current, name):
    yosys = re.compile(".*yosys")
    vivado = re.compile(".*vivado")
    xst = re.compile(".*xst")
    quartus = re.compile(".*quartus")

    if yosys.match(name):
        current[3] = 1
    elif vivado.match(name):
        current[2] = 1
    elif xst.match(name):
        current[1] = 1
    elif quartus.match(name):
        current[0] = 1

    return current

def get_group(val):
    return val[0]

def get_freq(val):
    return val[1]

def timeout_present(directory):
    return subprocess.run([ "grep", "-r", "--include", "symbiyosys.log"
                   , "-m", "1", "-q", "Keyboard interrupt"
                   , directory.as_posix()
    ]).returncode == 0

def find_reduce_dirs(start_dir=".", prefix="reduce"):
    matcher = re.compile(prefix + ".*")
    fuzzmatch = re.compile("fuzz.*")
    initdir = Path(start_dir)

    sets = []
    for dirlevel1 in iterdir(initdir):
        for dirlevel2 in iterdir(dirlevel1):
            current_set = [0, 0, 0, 0]
            update = True
            for dirlevel3 in iterdir(dirlevel2):
                if matcher.match(dirlevel3.name):
                    if timeout_present(dirlevel2):
                        current_set = [2, 2, 2, 2]
                        continue
                    current_set = identify_vivado(
                        current_set, dirlevel3.name)
                elif fuzzmatch.match(dirlevel3.name):
                    current_set = [0, 0, 0, 0]
                    for dirlevel4 in iterdir(dirlevel3):
                        if timeout_present(dirlevel3):
                            current_set = [2, 2, 2, 2]
                            break
                        if matcher.match(dirlevel4.name):
                            current_set = identify_vivado(
                                current_set, dirlevel4.name)
                    sets.append((current_set, dirlevel3))
                    update = False
            if update:
                sets.append((current_set, dirlevel2))
    freqs = [(x, len(list(y))) for x, y in
             itertools.groupby(sorted(sets, key=get_group),
                               get_group)]
    print(sorted(freqs, key=get_freq))

def main():
    find_reduce_dirs(".", "reduce")

if __name__ == "__main__":
    main()
