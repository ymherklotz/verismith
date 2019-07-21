#!/usr/bin/env python3

import subprocess
import os

def main():
        i = 0
        name = "medium"
        config = "experiments/config_medium.toml"
        iterations = 20
        directory = "swarm"
        if not os.path.exists(directory):
                os.makedirs(directory)
        while True:
                subprocess.call(["verifuzz", "config"
                                 , "-c", config
                                 , "-o", directory + "/config_medium_random.toml"
                                 , "--randomise"])
                subprocess.call([ "verifuzz", "fuzz"
                                 , "-o", directory + "/" + name + str(i)
                                 , "-c", directory + "/config_medium_random.toml"
                                 , "-n", str(iterations)])
                i += 1

if __name__ == '__main__':
        main()
