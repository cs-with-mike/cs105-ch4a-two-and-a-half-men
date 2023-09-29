#/* front.c - a lexical analyzer system for simple arithmetic expressions */ 

#/* Global declarations */ 

# Variables
charClass <- NULL  # Initialize to NULL
lexeme <- character(100)  # Initialize as a character vector of length 100
nextChar <- NULL  # Initialize to NULL
lexLen <- NULL  # Initialize to NULL
token <- NULL  # Initialize to NULL
nextToken <- NULL  # Initialize to NULL
in_fp <- NULL  # Initialize to NULL

# Character classes
LETTER <- 0
DIGIT <- 1
UNKNOWN <- 99

# Token codes
INT_LIT <- 10
IDENT <- 11
ASSIGN_OP <- 20
ADD_OP <- 21
SUB_OP <- 22
MULT_OP <- 23
DIV_OP <- 24
LEFT_PAREN <- 25
RIGHT_PAREN <- 26
EOF <- -1

#/******************************************************/ 
#/* main driver */ 

#/* Open the input data file and process its contents */ 

#/*****************************************************/ 
#/* lookup - a function to lookup operators and parentheses and return the token */ 

# Look up table
# Returns null if not here meaning the file is empty
lookup <- function(ch) {
  switch(ch,
         "(" = {
           print("addChar...")
           nextToken <<- LEFT_PAREN
         },
         ")" = {
           print("addChar...")
           nextToken <<- RIGHT_PAREN
         },
         "+" = {
           print("addChar...")
           nextToken <<- ADD_OP
         },
         "-" = {
           print("addChar...")
           nextToken <<- SUB_OP
         },
         "*" = {
           print("addChar...")
           nextToken <<- MULT_OP
         },
         "/" = {
           print("addChar...")
           nextToken <<- DIV_OP
         },
         default = {
           print("addChar...")
           nextToken <<- EOF
         }
  )
  return(nextToken)
}

#/*****************************************************/ 
#/* addChar - a function to add nextChar to lexeme */

#/*****************************************************/ 
#/* getChar - a function to get the next character of 

#/*****************************************************/ 
#getNonBlank - a function to call getChar until it returns a non-whitespace character
getNonBlank <- function() {
  while (charClass == "SPACE") {
    getChar()
  }
}
#/ *****************************************************/ 
#lex - a simple lexical analyzer for arithmetic expressions
lex <- function() {
  lexLen <<- 0
  getNonBlank()
  switch(charClass,
         LETTER = {
           addChar()
           getChar()
           while (charClass == "LETTER" || charClass == "DIGIT") {
             addChar()
             getChar()
           }
           nextToken <<- "IDENT"
         },
         DIGIT = {
           addChar()
           getChar()
           while (charClass == "DIGIT") {
             addChar()
             getChar()
           }
           nextToken <<- "INT_LIT"
         },
         UNKNOWN = {
           lookup(nextChar)
           getChar()
         },
         EOF = {
           nextToken <<- "EOF"
           lexeme <<- c("E", "O", "F")
         }
  )
  
  cat(paste("Next token is:", nextToken, ", Next lexeme is ", paste(lexeme, collapse = ""), "\n"))
  return(nextToken)
}



