[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/a2fQs4QM)
# Lexer & Parser for the Tokki Language
Westmont College CS 105 Fall 2023
Chapter 4 Assignment A and B

## Author Information
- Logan Towne, ltowne@westmont.edu
- Landon Amaral, lamaral@westmont.edu
- Eli Tiao, jtiao@westmont.edu

## Overview
In this assignment we are going to use our chosen language R to make a syntax analyzer based on the code in the textbook thats in c++. 
The first step is to convert the code one to one, and then try and use our chosen languages features to make it more efficient. In order to distribute the work evenly, each of us took two methods to transcode. After we each felt confident in our work, we began the process of implementing them together which took some time.

We ran into some issues with handling double parentheses and the input ending in a operator. It was reading it as an unknown and not running through our logic again. The reason for that was because of the fact that we had a conditional checking if its the same character, repeat and repeat. However, it was doing that with parentheses as well, counting them as one lexeme. We solved that issue eventually. 

## Design Notes
Some notable design decisions are: 

When converting the switch statements in c++ to R we could use a lookup table or if statements, 
so we chose a lookup table thinking they would be faster and easier to convert over. 

We decided to remove the predefined lexeme list of 100 because R can operate on dynamic list lengths. That allowed us to reduce a lot of list length management.

## Lessons Learned
Lesson #1
    It isn't as straightforward as we expected to be able to transcode logic from C++ to R. Because the two languages were designed with different goals in mind, the functionality and capability greatly impact how the logic operates. 

Lesson #2
    Working with multiple people on a single repository can be challenging. The biggest hurdle is when we all come back together in order to finish the project.

Lesson #3
    There is a differences between 'paste' and 'sprintf'. One adds spaces for you and the other allows more flexible and customizable formatting options.
