#!/bin/bash

cd data/
# download each pdf from the urls in data/pdf_list
# not cpu bound, so use > NCORES threads

# -j defines how many jobs run in parallel. -c makes it continue if it stops. -P saves it in the pdf folder.
sort pdf_list.txt | parallel -j 10 "wget --no-check-certificate -c -P pdf -i"
