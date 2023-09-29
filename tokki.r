#note, completely redid length system to make more readable in R

charClass <- ""
lexeme <- c() 
nextChar <- ""
token <- 0
nextToken <- 0
in_fp <- NULL  

LETTER <- "LETTER"
DIGIT <- "DIGIT"
UNKNOWN <- "UNKNOWN"

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

lookup <- function(ch) {
  switch(ch,
         '(' = {
           nextToken <<- LEFT_PAREN
         },
         ')' = {
           nextToken <<- RIGHT_PAREN
         },
         '+' = {
           nextToken <<- ADD_OP
         },
         '-' = {
           nextToken <<- SUB_OP
         },
         '*' = {
           nextToken <<- MULT_OP
         },
         '/' = {
           nextToken <<- DIV_OP
         },
         '=' = {
           nextToken <<- ASSIGN_OP
         },
         default = {
           nextToken <<- EOF
         }
  )
  return(nextToken)
}

addChar <- function() {
  lexeme[(length(lexeme)) + 1] <- nextChar
}

getChar <- function() {
  if (length(char_iterator) > 0) {
    nextChar <<- char_iterator[1]
    char_iterator <<- char_iterator[-1]
    if (nextChar %in% c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')) {
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

getNonBlank <- function() {
  while (grepl("\\s", nextChar)) {
    getChar()
  }
}

lex <- function() {
  lexeme <<- c()
  getNonBlank()
  
  
  switch(charClass,
         LETTER = {
           getChar()
           addChar()
           while (charClass == LETTER || charClass == DIGIT) {
             getChar()
             addChar()
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
  
  token_name <- switch(
    nextToken,
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
  return(nextToken)
}


input_string <- "(sum + 47) / total"

#not here
char_iterator <- strsplit(input_string, "")[[1]]

getChar()
repeat {
  if (lex() == EOF) {
    break
  }
}