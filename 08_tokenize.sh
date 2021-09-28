#!/bin/sh

dapipe/udpipe.lin64 --tokenizer=presegmented --tag dapipe/danish-ud-2.0-170801.udpipe $1 --outfile=$2
