# Lexer & Parser for the Tokki Language
Westmont College CS 105 Fall 2023
Chapter 4 Assignment A

## Author Information
- Logan Towne, ltowne@westmont.edu
- Landon Amaral, lamaral@westmont.edu
- **Ovenly McEasybake** omceasybake@westmont.edu

## Overview
In this assignment we are going to use our chosen language R to make a syntax analyzer based on the code in the textbook thats in c++. 
The first step is to convert the code one to one, and then try and use our chosen languages features to make it more efficient. In order to distribute the work evenly, each of us took two methods to transcode. After we each felt confident in our work, we began the process of implementing them together which took some time.

## Design Notes
Some notable design decisions are: 

When converting the switch statements in c++ to R we could use a lookup table or if statements, 
so we chose a lookup table thinking they would be faster and easier to convert over. 

## Lessons Learned
Lesson #1
    It isn't as straightforward as we expected to be able to transcode logic from C++ to R. Because the two languages were designed with different goals in mind, the functionality and capability greatly impact how the logic operates. 

Lesson #2
    Working with multiple people on a single repository can be challenging. The biggest hurdle is when we all come back together in order to finish the project.
