#!/usr/bin/env python3

# this file reads the txt documents (one per parlaiment session) and
# splits up the session into speeches, extracting timestamps and speakers
# it needs a good date regex and a list of member names and party names
# to work.

# the algorithm is basically this:
# look at a new line
# if it looks like a time, a member name, or a title
# -> store that metadata, start new document
# otherwise, add line to current document

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
with open("folketing_titler.txt") as f:
    titler = [l for l in f.read().split("\n") if l]
    titler = "(?:" + "|".join([re.escape(i) for i in titler] +\
                      [re.escape(i + "en") for i in titler])  + ")"

# match either of these formats:
# name (party)
# title (name)
# title
header_format1 = "^\s*(" + members + ")\s*\(.{0,35}?\)\s*(?=:)"
header_format2 = "^\s*" + titler + "\s*:?\s*[(](" + members + ")[)]?\s*(?=:)"
header_format3 = "^\s*(" + titler + ")\s*:$"
header_format4 = "^\s*\[(.{0,35}?)\.*\][.:]*\s*"
header_format5 = "^(" + members + ")\s*:\s+"
header_format6 = "^(" + titler + ")\s*:\s+"
newline_re = re.compile("\n")


class Rule:
    def __init__(self, regex, name):
        self.regex = re.compile(regex, re.IGNORECASE)
        self.name = name

    def find(self, txt):
        return self.regex.match(txt)


rules = [
    # match time
    Rule("(?:(?:(?:man|tirs|ons|tors|fre|lør|søn)dag "\
         "den\s+)?\d{1,2}\.\s+[A-Za-z]+\s+\d{4}\s+)?"\
         "(((kl(\.)?)\s*?)"\
         "([012]?\d[\.:-]?\d{2})([\.:-][012]\d|(?=[^\.:-]+?)|$))",
         "Time"),
    # match speaker
    Rule(header_format1, "name_party"),
    Rule(header_format2, "title_name"),
    Rule(header_format3, "title_name"),
    Rule(header_format4, "title_name"),
    Rule(header_format5, "title_name"),
    Rule(header_format6, "title_name")
]


def show(i):
    # print a rule and its matche easily
    print(data.reason[i] + "\n" + data.text[i])


data_cols = ['split', 'reason', 'text']
file_list = glob.glob("data/txt/*.txt")

file_list = sys.argv[1:]

fl = len(file_list)


# for filename in ["data/txt/20111_M14_helemoedet.txt"]:
for i, filename in enumerate(file_list):
    # print("[segmenting] " + str(i+1) + " / " + str(fl) + " " + filename)
    # prepare the output data frame
    data = pd.DataFrame([], columns=data_cols)

    # initial values
    prev_split = 0
    prev_reason = ''

    # keep track of where we are in the file
    tally = 0

    with open(filename) as f:
        t = f.read().replace(";", ":")
        orig_t = t

        while '\n' in t:
            for r in rules:
                d = r.find(t[:50])
                if d:
                    # if the rule matched at the location we're currently at
                    # add the data
                    new = {'split': prev_split, 'reason': prev_reason,
                           'text': orig_t[prev_split:tally+d.start()].strip()}
                    data = data.append(new, ignore_index=True)

                    # save the location
                    prev_split = d.end() + tally
                    prev_reason = r.name + ";" + d.group(1).strip()

                    # keep track of how far we are
                    tally += len(t[:d.end()])
                    t = t[d.end():]

                    if debug:
                        print("HIT " + str(prev_split))

                    break  # dont hit the else below

            else:
                # neither rule matched, skip one line ahead

                if debug:
                    print("MISS " + str(prev_split) + ';' + str(tally))

                # try:
                _old, t = newline_re.split(t, 1)
                tally += len(_old)+1
                # except:
                #     pass

    new = {'split': prev_split, 'reason': prev_reason,
           'text': orig_t[prev_split:].strip()}
    data = data.append(new, ignore_index=True)
    # done with this file

    # now split the reason column
    if len(data) > 1:
        data['reason'], data['value'] = data['reason'].str.split(';').str

    data.to_csv(filename.replace("/txt/", "/segmented/"),
                sep=";", index=False)
