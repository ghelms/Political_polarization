#!/bin/bash

cd data/pdf
# download each pdf from the urls in data/pdf_list
# not cpu bound, so use > NCORES threads
sort ../missing_pdfs.txt | uniq -u |  parallel -v --gnu -j 10 "wget -c"
