#!/bin/bash

#  use pdf2text from
echo "[ ] Extracting text from pdfs"

# Changing the directory to the folder containing all the scraped pdf
cd data/pdf/

# Listing all the filenames and then converting them to txt files
ls | parallel -j 10 "pdftotext"

# Moving all the new txt files to the new folder
mv *.txt ../txt/