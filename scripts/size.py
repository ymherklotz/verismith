#!/usr/bin/env python3

import subprocess
import statistics

def file_len(fname):
    with open(fname) as f:
        for i, l in enumerate(f):
            pass
    return i + 1

def main():
    l = []
    for x in range(0, 10):
        subprocess.call(["verifuzz", "generate", "-o", "main.v", "-c", "config.toml"])
        l.append(file_len("main.v"))
    print("mean: ", statistics.mean(l))
    print("median: ", statistics.median(l))
    print("stdev: ", statistics.stdev(l))
    print("variance: ", statistics.variance(l))

if __name__ == '__main__':
    main()
