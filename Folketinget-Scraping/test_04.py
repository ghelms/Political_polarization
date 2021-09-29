import pandas as pd
import re
import sys
import glob


out_file = "data/parsed_data.csv"
debug = False

# load some prepared data of the folketing members and some titles
# and build a regular expression to match speakers
ft = pd.read_csv("data/folketing_members.csv", ";", header = None)

# construct name variations. ie given name "Malte Lau Petersen",
# also look for "Petersen" and "Malte Petersen" and "Lau Petersen"

extranames = []
for name in ft.iloc[:, 0]:
    namelist = name.split()
    extranames.append(namelist[-1])
    extranames.append(namelist[0] + " " + namelist[-1])
    extranames.append(namelist[-2] + " " + namelist[-1])
allnames = set(ft.iloc[:, 0])
allnames.update(extranames)

members = "(?:" + '|'.join([re.escape(i) for i in allnames]) + ")"
parties = "(?:" + '|'.join([re.escape(str(i)) for i in set(ft.iloc[:, 1])]) + ")"