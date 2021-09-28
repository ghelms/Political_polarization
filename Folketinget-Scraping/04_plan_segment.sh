#!/bin/bash

echo "[ ] Starting text segmenting"
# cpu bound, so use only NCORES number of threads
/usr/bin/find data/txt/*.txt | parallel --gnu -j 4 -X "python3 04_segment.py"
