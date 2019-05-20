#!/usr/bin/env python3

import subprocess
import statistics
import argparse
import os

def file_len(fname):
    with open(fname) as f:
        for i, l in enumerate(f):
            pass
    return i + 1

def main(c, n):
    l = []
    for x in range(0, n):
        subprocess.call(["verifuzz", "generate", "-o", "main.v", "-c", c])
        l.append(file_len("main.v"))
    os.remove("main.v")
    print("mean: ", statistics.mean(l))
    print("median: ", statistics.median(l))
    print("stdev: ", statistics.stdev(l))
    print("variance: ", statistics.variance(l))

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Check the average size of a generated program.')
    parser.add_argument('config', metavar='CONFIG', help='The config file to test.')
    parser.add_argument('-n', '--num', help='How many iterations to run.', default=10, type=int)
    args = parser.parse_args()
    main(args.config, args.num)
