#!/usr/bin/env python3

import sys
from bs4 import BeautifulSoup
import csv
import re

def main(file_, output):
    with open(file_, "r") as f:
        file_contents = f.read()

    sec = re.compile(r"([0-9.]+)s")

    soup = BeautifulSoup(file_contents, "html.parser")
    table = soup.select_one("table.table")
    headers = [th.text for th in table.select("tr th")]
    vals = [[td.text for td in row.find_all("td")] for row in table.select("tr + tr")][:-2]

    headers = map(lambda x: "Size" if x == "Size (loc)" else x, headers)

    vals = map(lambda l: map(lambda s: sec.sub(r"\1", s), l), vals)
    vals = map(lambda l: map(lambda s: re.sub(r"Failed", r"1", s), l), vals)
    vals = map(lambda l: map(lambda s: re.sub(r"Passed", r"0", s), l), vals)

    with open(output, "w") as f:
        wr = csv.writer(f)
        wr.writerow(headers)
        wr.writerows(vals)

if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
