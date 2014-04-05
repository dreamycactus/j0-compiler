\section{Lexer}
This module simply contains definitions that Parsec uses to configure its lexer. There 
is not much of note other than some convenience functions defined near the end of the file
which are used in the parser.
\begin{code}
module Lexert where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+","*","-","/",";",",","<","=",">","."]
    names = ["class","static","if","then","else","in"
           , "while","return","print","null","new","int"]
    style = emptyDef {
               Tok.commentLine = "//"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

integer    = Tok.integer lexer
float      = Tok.float lexer
parens     = Tok.parens lexer
braces     = Tok.braces lexer
commaSep   = Tok.commaSep lexer
semiSep    = Tok.semiSep lexer
identifier = Tok.identifier lexer
whitespace = Tok.whiteSpace lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
\end{code}
