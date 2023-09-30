#!/bin/bash
# Authors:
#  - Logan Towne, ltowne@westmont.edu
#  - Landon Amaral, lamaral@westmont.edu
#  - Eli Tiao, jtiao@westmont.edu

# Run any commands necessary to set up your language's runtime environment here.
# If the runtime is expected to be present on Ubuntu by default, then do nothing.
echo "Setting up runtime ..."
sudo apt update
sudo apt install r-base r-base-dev -y
R

# Run your Tokki lexer, passing in the first command line argument directly to the lexer.
# Any output to STDOUT should be directed to a text output file titled "out.txt."
echo "Running Tokki ... output file is out.txt"

# This will run out tokki file and output it on out.txt
Rscript tokki.r
