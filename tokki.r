#front.c - a lexical analyzer system for simple arithmetic expressions

#Global declarations

# Variables
charClass <- "" 
lexeme <- character(100)  # Initialize as a character vector of length 100
nextChar <- ""  
lexLen <- 0
token <- 0
nextToken <- 0  
in_fp <- NULL  

# Character classes
LETTER <- "LETTER"
DIGIT <- "DIGIT"
UNKNOWN <- "UNKNOWN"

# Token codes
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

#/lookup - a function to lookup operators and parentheses and return the token

# Look up table WORKS
lookup <- function(ch) {
  switch(ch,
         '(' = {
           print("addChar...")
           nextToken <<- LEFT_PAREN
         },
         ')' = {
           print("addChar...")
           nextToken <<- RIGHT_PAREN
         },
         '+' = {
           print("addChar...")
           nextToken <<- ADD_OP
         },
         '-' = {
           print("addChar...")
           nextToken <<- SUB_OP
         },
         '*' = {
           print("addChar...")
           nextToken <<- MULT_OP
         },
         '/' = {
           print("addChar...")
           nextToken <<- DIV_OP
         },
         '=' = {
           print("addChar...")
           nextToken <<- ASSIGN_OP
         },
         default = {
           print("addChar...")
           nextToken <<- EOF
         }
  )
  return(nextToken)
}

#addChar - a function to add nextChar to lexeme WORKS
addChar <- function() {
  if (lexLen <= 98) {
    lexeme[lexLen + 1] <- nextChar
    lexLen <<- lexLen + 1
    lexeme[lexLen + 1] <- ""
  } else {
    stop("Error - lexeme is too long")
  }
}

#getChar - a function to get the next character of input and determine its character class WORKS
getChar <- function() {
  # Open the file connection if it's not already open
  if (is.null(in_fp)) {
    in_fp <<- file("sample.tk", "r")
  }
  
  # Read a single character from the file connection
  nextChar <<- readChar(in_fp, 1)
  
  if (nextChar != "") {
    if (nextChar %in% c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) {
      charClass <<- DIGIT
    } else if (grepl("[A-Za-z]", nextChar)) {
      charClass <<- LETTER
    } else {
      charClass <<- UNKNOWN
    }
  } else {
    charClass <<- EOF
  }
}

#getNonBlank - a function to call getChar until it returns a non-whitespace character SHOULDWORK
getNonBlank <- function() {
  while (grepl("\\s", nextChar)) {
    getChar()
  }
}

#lex - a simple lexical analyzer for arithmetic expressions
lex <- function() {
  lexLen <<- 0
  lexeme <<- character(100)  # Reset lexeme
  
  getNonBlank()
  
  switch(charClass,
         LETTER = {
           addChar()
           getChar()
           while (charClass == LETTER || charClass == DIGIT) {
             addChar()
             getChar()
           }
           nextToken <<- IDENT
         },
         DIGIT = {
           addChar()
           getChar()
           while (charClass == DIGIT) {
             addChar()
             getChar()
           }
           nextToken <<- INT_LIT
         },
         UNKNOWN = {
           lookup(nextChar)
           getChar()
         },
         EOF = {
           nextToken <<- EOF
           lexeme[1] <<- "EOF"
         }
  )
  
  cat(paste("Next token is:", nextToken, ", Next lexeme is ", paste(lexeme, collapse = ""), "\n"))
  return(nextToken)
}


# main driver *
#Open the input data file and process its contents 
if (file.exists("sample.tk")) {
  in_fp <- file("sample.tk", "r")
  getChar()
  repeat {
    if (lex() == EOF) {
      break
    }
  }
  close(in_fp)
} else {
  cat("ERROR - cannot open front.in\n")
}



