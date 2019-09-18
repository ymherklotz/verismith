#!/usr/bin/env python3

import csv
import sys
import random

def main(filename, output_file):
    with open(filename, "r") as f:
        reader = list(csv.reader(f))
    newreader = []
    for row in reader:
        try:
            if float(row[4]) > 900:
                row[4] = "900"
            if float(row[3]) > 900:
                row[3] = "900"
            if random.random() < 0.25:
                newreader.append(row)
        except:
            newreader.append(row)
    with open(output_file, "w") as f:
        writer = csv.writer(f)
        writer.writerows(newreader)

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
