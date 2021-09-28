#!/bin/bash

#  use pdf2text from

echo "[ ] Extracting text from pdfs"
cd data/pdf
/bin/ls | parallel --bar --gnu -j 8 "pdftotext -q {} ../txt/{.}.txt"

