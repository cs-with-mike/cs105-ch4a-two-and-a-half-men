#/* front.c - a lexical analyzer system for simple arithmetic expressions */ 

#/* Global declarations */ 

#/* Variables */ 


#/* Function declarations */ 


#/* Character classes */ 


#/* Token codes */ 

#/******************************************************/ 
#/* main driver */ 

#/* Open the input data file and process its contents */ 

#/*****************************************************/ 
#/* lookup - a function to lookup operators and parentheses and return the token */ 

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



