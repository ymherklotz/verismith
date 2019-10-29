#!/usr/bin/env python3

import subprocess
import os
import sys
import datetime

def main(run_id):
    i = 0
    name = "medium_{}_".format(run_id)
    config = "config.toml"
    iterations = 100
    directory = "yosys_all"
    try:
        os.makedirs(directory)
    except IOError:
        pass
    while True:
        output_directory = directory + "/" + name + str(i)
        print("{} :: {}".format(datetime.datetime.now(), output_directory))
        with open(output_directory + ".log", "w") as f:
            subprocess.call(["cabal", "run", "-O2", "verismith", "--", "fuzz"
                             , "-o", output_directory
                             , "-c", config
                             , "-n", str(iterations)], stdout=f)
        i += 1

if __name__ == '__main__':
    main(sys.argv[1])
