# Global Variables

charClass <- ""
lexeme <- ""
nextChar <- ""
nextToken <- 0
in_fp <- NULL
char_iterator <- NULL

depth <- 0

LETTER <- "LETTER"
DIGIT <- "DIGIT"
UNKNOWN <- "UNKNOWN"
SPACE <- "SPACE"

INT_LIT <- "INT_LIT"
IDENT <- "IDENT"
ASSIGN_OP <- "ASSIGN_OP"
ADD_OP <- "ADD_OP"
SUB_OP <- "SUB_OP"
MULT_OP <- "MULT_OP"
DIV_OP <- "DIV_OP"
LEFT_PAREN <- "LEFT_PAREN"
RIGHT_PAREN <- "RIGHT_PAREN"
EOF <- "EOF"


# This is a helper function that prints all information to terminal 
printData <- function() {
  cat("Values at time of error:\n")
  cat("\tcharClass:", charClass, "\n")
  cat("\tlexeme:", lexeme, "\n")
  cat("\tnextChar:", nextChar, "\n")
  cat("\tnextToken:", nextToken, "\n")
  cat("\tchar_iterator:", char_iterator, "\n")
}



# A function that formats the entering and exiting of <expr>, <term> and <factor>
detailed_print <- function(entering, depth_change) {
  
  # We want to increase if it's a positive change
  if(depth_change > 0){depth <<- depth + depth_change}
  
  for(i in 1:depth){
    if(entering){
      # If it is entering a derivation, increment the indent for formatting and print out the ">"
      cat(">")
    }else{
      # Otherwise, you're leaving a derivation
      cat("<")
    }
  }
  
  # We should decrease only after printing to follow the formatting
  if(depth_change < 0){depth <<- depth + depth_change}
}



# Parses strings in the language generated by the rule:
#     <factor> -> id | int_constant | ( <expr> )
factor <- function(){
  detailed_print(TRUE, 1)
  cat(" factor\n")
  
  # Determine which Right-Hand-Side
  if (nextToken == IDENT || nextToken == INT_LIT){
    
    # Get the next token
    lex()
  
  # If the RHS is ( <expr> ), call lex to pass over the left parenthesis, call expr(), and check for the right parenthesis
  } else {
    if (nextToken == LEFT_PAREN){
      lex()
      expr()
      
      if (nextToken == RIGHT_PAREN){
        lex()
      } else {
        cat("Error: Expected RIGHT_PAREN\tReceived -", nextToken, "\n")
        printData()
        break
      }
    } else {
      cat("Error: Expected - LEFT_PAREN\tReceived -", nextToken, "\n")
      printData()
      break
    }
  }
  
  detailed_print(FALSE, -1)
  cat(" factor\n")
}



# Parses strings in the language generated by the rule:
#     <term> -> <factor> { (* | /) <factor> }
term <- function(){
  detailed_print(TRUE, 1)
  cat(" term\n")
  
  # Parse the first factor
  factor()
  
  # As long as the next token is * or /, get the next token and parse the next factor
  while (nextToken == MULT_OP || nextToken == DIV_OP){
    lex()
    factor()
  }
  
  detailed_print(FALSE, -1)
  cat(" term\n")
}



# Parses strings in the language generated by the rule:
#     <expr> -> <term> { (+ | -) <term> }
expr <- function(){
  
  detailed_print(TRUE, 1)
  cat(" expr\n")
  
  # Parse the first term
  term()
  
  # As long as the next token is + or -, get the next token and parse the next term
  while (nextToken == ADD_OP || nextToken == SUB_OP){
    lex()
    term()
  }
  
  detailed_print(FALSE, -1)
  cat(" expr\n")
}



# A simple lexical analyzer for arithmetic expressions
lex <- function(){
  
  # Switch Case
  switch(charClass,
         
         # Parsing identifiers 
         LETTER = {
           lexeme <<- nextChar
           getChar()
           nextToken <<- IDENT
         },
         
         # Parsing integer literals
         DIGIT = {
           lexeme <<- nextChar
           getChar()
           nextToken <<- INT_LIT
         },
         
         # Parentheses and operators
         UNKNOWN = {
           lexeme <<- nextChar
           lookup(nextChar)
           getChar()
         },
         
         # EOF 
         EOF = {
           nextToken <<- EOF
           lexeme <<- "EOF"
         })
  
  # This is also for formatting the output
  if(depth != 0){ for(i in 1:depth) {cat("=")} }
  cat(sprintf(" %s [ %s ]\n", nextToken, lexeme))
  return(nextToken)
}



# A function to get the next character of input and determine its character class
# If it is a number or a variable name (ex: 24 or total), it will combine those into one "char"
getChar <- function(){

  # Checking if the input string is empty (FYI we're at the end)
  if (length(char_iterator) > 0) {
    
    # This removes a space in the front
    if(grepl("^\\s*$", char_iterator[1])){
      char_iterator <<- char_iterator[-1]
    }
    
    # Gets the next character in the input string
    nextChar <<- char_iterator[1]
    # Removes that character from the input string
    char_iterator <<- char_iterator[-1]
    
    
    # This combines characters into a single lexeme by checking if the next class is the same as the previous
    while(whatClass(nextChar) != UNKNOWN && whatClass(nextChar) == whatClass(char_iterator[1])) {
      nextChar <<- paste0(nextChar, char_iterator[1])
      char_iterator <<- char_iterator[-1]
      if (length(char_iterator) == 0) break
    }
    
    # This assigns charClass depending on the value of nextChar
    if (grepl("\\d+", nextChar)) {
      charClass <<- DIGIT
    } else if (grepl("[A-Za-z]", nextChar)) {
      charClass <<- LETTER
    } else {
      charClass <<- UNKNOWN
    }
    
  # Otherwise, we're at the end
  } else {
    charClass <<- EOF
    nextChar <<- ""
  }
}



# A function that returns what character class 'x' is
whatClass <- function(x) {
  if (grepl("^\\s*$", x)) {
    return(SPACE)
  }else if (grepl("\\d+", x)) {
    return(DIGIT)
  } else if (grepl("[A-Za-z]", x)) {
    return(LETTER)
  } else {
    return(UNKNOWN)
  }
}



# A function to add nextChar to lexeme
addChar <- function(){
  
  # This combines characters into a single lexeme by checking if the next class is the same as the previous
  while(whatClass(nextChar) != UNKNOWN && whatClass(nextChar) == whatClass(char_iterator[1])) {
    nextChar <<- paste0(nextChar, char_iterator[1])
    char_iterator <<- char_iterator[-1]
    if (length(char_iterator) == 0) break
  }
  
  #This finally
  lexeme <<- nextChar 
}



# A function to look up operators and parentheses and return the respective token
lookup <- function(ch) {
  
  if(ch == "("){nextToken <<- LEFT_PAREN}
  else if(ch == ")"){nextToken <<- RIGHT_PAREN}
  else if(ch == "+"){nextToken <<- ADD_OP}
  else if(ch == "-"){nextToken <<- SUB_OP}
  else if(ch == "*"){nextToken <<- MULT_OP}
  else if(ch == "/"){nextToken <<- DIV_OP}
  else if(ch == "="){nextToken <<- ASSIGN_OP}
  else {nextToken <<- EOF}
  
  return(nextToken)
}



# MAIN part
args <- commandArgs(trailingOnly = TRUE)

# If the file exists
if (file.exists(args[1])) {
  in_fp <- file(args, "r")
  
  # If the file is empty, return EOF token; Otherwise, we can begin parsing
  if (file.size(args[1]) == 0) {
    cat("EOF [ EOF ]\n> expr\n>> term\n>>> factor\nError - invalid tokki")
  } else {
    # Getting the all characters in the file and splitting them
    char_iterator <<- readLines(in_fp, warn = FALSE)
    char_iterator <<- strsplit(char_iterator, "")[[1]]
    char_iter_len <<- length(char_iterator)
    
    # Start the parsing
    getChar() # Initial assignment of char
    lex() # Initial assignment of token
    expr()
  }
  
  close(in_fp)
} else {
  cat("ERROR - cannot open file:\t", args[1], "\n")
}
