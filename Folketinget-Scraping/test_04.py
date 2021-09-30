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

with open("txtfile.txt") as f:
    f = f.read()
    print(f)
    print(f[:50])

#for r in rules:
#    print(r.find(t[:50]))

#self_test = re.compile("t", re.IGNORECASE)
#print(self_test)

#class_test = Rule("t", "Morten")
#print(class_test.find("o"))

#print(newline_re)
#print(members)
#print(parties)
#print(titler)
#print("\nn\nn\n %s \nn\nn\n  %s \n\n\n %s \n\n\n %s \n\n\n %s \n\n\n %s \n\n\n" % (header_format1, header_format2, header_format3, header_format4, header_format5, header_format6))

#print(extranames)
#print(members)