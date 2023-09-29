#!/bin/bash
# Authors:
#  - Logan Towne, ltowne@westmont.edu
#  - Landon Amaral, lamaral@westmont.edu
#  - Ovenly McEasybake, omceasybake@westmont.edu

# Run any commands necessary to set up your language's runtime environment here.
# If the runtime is expected to be present on Ubuntu by default, then do nothing.
echo "Setting up runtime ..."
sudo apt update
sudo apt install r-base r-base-dev -y
R

# I'm checking Python interpreter's version here just as a placeholder.
python3 --version # TODO: YOUR RUNTIME SETUP HERE.

# Run your Tokki lexer, passing in the first command line argument directly to the lexer.
# Any output to STDOUT should be directed to a text output file titled "out.txt."
echo "Running Tokki ..."

# As an example, I have provided how I would run my tokki.pyc.
python3 tokki.pyc $1 > out.txt # TODO: REPLACE THIS WITH YOUR OWN COMMAND.
