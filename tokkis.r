# note, completely redid length system to make more readable in R

charClass <- ""
lexeme <- c()
nextChar <- ""
token <- 0
nextToken <- 0
in_fp <- NULL

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


# factor
factor <- function() {
  cat("Enter <factor>\n")
  
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
        error("Error: Expected right parenthesis")
      }
    } else {
      error("Error: Invalid token")
    }
  }
  
  cat("Exit <factor>\n")
}

# term
term <- function() {
  cat("Enter <term>\n")
  
  # First Factor
  factor()
  
  while (nextToken == "MULT_OP" || nextToken == "DIV_OP") {
    lex()
    factor()
  }
  
  cat("Exit <term>\n")
}

# expr
expr <- function() {
  cat("Enter <expr>\n")
  
  # First term
  term()
  
  while (nextToken == "ADD_OP" || nextToken == "SUB_OP") {
    lex()
    term()
  }
  
  cat("Exit <expr>\n")
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
  cat(paste("Next token is:", token_name, "| Next lexeme is", paste(lexeme, collapse = ""), "\n"))
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
