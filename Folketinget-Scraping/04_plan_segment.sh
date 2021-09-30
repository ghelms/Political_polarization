#!/bin/bash

echo "[ ] Starting text segmenting"
# cpu bound, so use only NCORES number of threads
/usr/bin/find data/txt/*.txt | parallel -j 2 -X "python3 04_segment.py"
