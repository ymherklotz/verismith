#!/usr/bin/env python3

import sys
from bs4 import BeautifulSoup
import csv

def main(file_):
    with open(file_, "r") as f:
        file_contents = f.read()

    soup = BeautifulSoup(file_contents)
    table = soup.select_one("table")
    headers = [th.text.encode("utf-8") for th in table.select("tr th")]

    with open("out.csv", "w") as f:
        wr = csv.writer(f)
        wr.writerow(headers)
        wr.writerows([[td.text.encode("utf-8") for td in row.find_all("td")] for row in table.select("tr + tr")])

if __name__ == '__main__':
    main(sys.argv[1])
