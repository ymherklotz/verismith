#!/usr/bin/env python3

import subprocess
import os

def main():
        i = 0
        name = "mediumB"
        config = "experiments/config_yosys.toml"
        iterations = 50
        directory = "yosys_all"
        if not os.path.exists(directory):
                os.makedirs(directory)
        while True:
                subprocess.call(["verismith", "fuzz"
                                 , "-o", directory + "/" + name + str(i)
                                 , "-c", config
                                 , "-n", str(iterations)])
                i += 1

if __name__ == '__main__':
        main()
