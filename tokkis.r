charClass <- "" # the class of the current character
lexeme <- c() # stores character of current lexeme
nextChar <- "" # stores the next character in the input
token <- 0 # current token
nextToken <- 0 # next token (duh)
in_fp <- NULL # pointer for input file
depth <- 0 # depth of derivation

# constants to represent different character classes and tokens as strings

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

# Part B Functions
# NEW STUFF

# This is a function meant to print out the depth and derivation of each line
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


# factor
factor <- function() {
  
  detailed_print(TRUE, 1)
  cat(" factor\n")
  
  if (nextToken == "IDENT" || nextToken == "INT_LIT") {
    # Get the next token
    lex()
  } else {
    if (nextToken == "LEFT_PAREN") {
      lex()
      expr()
      if (nextToken == "RIGHT_PAREN") {
        lex()
      } else {
        cat("Error: Expected right parenthesis\n")
      }
    } else {
      cat("Error: Invalid token\n")
    }
  }
  
  detailed_print(FALSE, -1)
  
  cat(" factor\n")
  
}

# term
term <- function() {
  
  detailed_print(TRUE, 1)
  cat(" term\n")
  
  # First Factor
  factor()
  
  while (nextToken == "MULT_OP" || nextToken == "DIV_OP") {
    lex()
    factor()
  }
  
  detailed_print(FALSE, -1)
  cat(" term\n")
}

# expr
expr <- function() {
  
  detailed_print(TRUE, 1)
  cat(" expr\n")
  
  # First term
  term()
  
  while (nextToken == "ADD_OP" || nextToken == "SUB_OP") {
    lex()
    term()
  }
  
  detailed_print(FALSE, -1)
  cat(" expr\n")
}

# Part A

# if the lexeme is an UNKNOWN token, it passes it through to lookup() for it to be categorized as an arithmetic token or EOF (End of File)

lookup <- function(ch) {
  switch(ch,
    "(" = {
      nextToken <<- LEFT_PAREN
    },
    ")" = {
      nextToken <<- RIGHT_PAREN
    },
    "+" = {
      nextToken <<- ADD_OP
    },
    "-" = {
      nextToken <<- SUB_OP
    },
    "*" = {
      nextToken <<- MULT_OP
    },
    "/" = {
      nextToken <<- DIV_OP
    },
    "=" = {
      nextToken <<- ASSIGN_OP
    },
    default = {
      nextToken <<- EOF
    }
  )
  return(nextToken)
}

whatClass <- function(x) {
  if (grepl("^\\s*$", x)) {
    return(SPACE)
  } else if (grepl("\\d+", x)) {
    return(DIGIT)
  } else if (grepl("[A-Za-z]", x)) {
    return(LETTER)
  } else {
    return(UNKNOWN)
  }
}

# Sets charClass, which is essentially the token, unless UNKNOWN which then needs lookup()

# set current charClass and nextChar, recursively set nextChar as long as class matches?
getChar <- function() {
  if (length(char_iterator) > 0) {
    if (grepl("^\\s*$", char_iterator[1])) {
      char_iterator <<- char_iterator[-1]
    }

    nextChar <<- char_iterator[1]
    char_iterator <<- char_iterator[-1]


    # go through iterator and combine until all similar are same thing
    while (whatClass(nextChar) != UNKNOWN && whatClass(nextChar) == whatClass(char_iterator[1])) {
      nextChar <<- paste0(nextChar, char_iterator[1])
      char_iterator <<- char_iterator[-1]
      if (length(char_iterator) == 0) break
    }

    if (grepl("\\d+", nextChar)) {
      charClass <<- DIGIT
    } else if (grepl("[A-Za-z]", nextChar)) {
      charClass <<- LETTER
    } else {
      charClass <<- UNKNOWN
    }
  } else {
    charClass <<- EOF
    nextChar <<- ""
  }
}


# Loop that iterates through string to set token and lexeme of characters, then prints as statement.

lex <- function() {
  switch(charClass,
    LETTER = {
      lexeme[1] <<- nextChar
      nextToken <<- IDENT
      getChar()
    },
    DIGIT = {
      lexeme[1] <<- nextChar
      nextToken <<- INT_LIT
      getChar()
    },
    UNKNOWN = {
      lookup(nextChar)
      lexeme[1] <<- nextChar
      getChar()
    },
    EOF = {
      nextToken <<- EOF
      lexeme[1] <<- "EOF"
    }
  
  )

  token_name <- switch(nextToken,
    LEFT_PAREN = "LEFT_PAREN",
    RIGHT_PAREN = "RIGHT_PAREN",
    ADD_OP = "ADD_OP",
    SUB_OP = "SUB_OP",
    MULT_OP = "MULT_OP",
    DIV_OP = "DIV_OP",
    ASSIGN_OP = "ASSIGN_OP",
    IDENT = "IDENT",
    INT_LIT = "INT_LIT",
    EOF = "EOF",
    UNKNOWN = "UNKNOWN"
  )
  
  # This is also for formatting the output
  if(depth != 0){
    for(i in 1:depth){cat("=")}
  }
  # Printing out the tokens
  cat(sprintf(" %s [ %s ]\n", token_name, paste(lexeme, collapse = "")))
  expr()
  return(nextToken)
}

# MAIN part
args <- commandArgs(trailingOnly = TRUE)

if (file.exists(args[1])) {
  in_fp <- file(args, "r")

  # Checking if the file has information in it
  if (file.size(args[1]) == 0) {
    cat("Next token is: EOF | Next lexeme is EOF")

    # Run the lexer if it does
  } else {
    char_iterator <- readLines(in_fp, warn = FALSE)
    char_iterator <- strsplit(char_iterator, "")[[1]]

    getChar()
    repeat {
      if (lex() == EOF) {
        break
      }
    }
  }

  close(in_fp)
} else {
  cat("ERROR - cannot open file:\t", args[1], "\n")
}
