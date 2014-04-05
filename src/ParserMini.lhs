\section{Parser}
This module contains the functions for using the combinatoric parsers that the Parsec library
offers. The parser is designed to go top-down.
\begin{code}
module ParserMini where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Perm

import Lexert
import SyntaxMini
\end{code}
\subsection{Types}
Using the ``do" notation, the parsing functions are quite readable. typeInt and typeId
are trivial parsers that parse types, which are part of all expressions and variables.
\begin{code}
typeInt :: Parser Type
typeInt = do
    reserved "int"
    return T_Int

typeId :: Parser Type
typeId = do
    name <- identifier
    return $ T_Id name

\end{code}
The type\_ function wraps all types and attempts to match the input to a previously defined
type. Using the ``try" notation, in the event the parser fails to match, the parser acts as
though no input was consumed, effectively rolling back infinitely.
\begin{code}
type_ :: Parser Type
type_ = do
        try typeInt
    <|> try typeId
\end{code}
\subsection{Expressions}
Binary operators are implemented via a table, which is then passed to Parsec. The reason that
this table is built is due to Parsec being unable to handle left recursive grammars. By creating
this table, Parsec can rewrite the grammar internally to a non-left recursive one.
\begin{code}
binary s f assoc
    = Ex.Infix (reservedOp s >> return (B_Op f)) assoc

binops = [[binary "*" Multiply Ex.AssocLeft
          ,binary "/" Divide Ex.AssocLeft]
         ,[binary "+" Add Ex.AssocLeft
          ,binary "-" Subtract Ex.AssocLeft]
         ,[binary "<" LessThan Ex.AssocLeft
          ,binary ">" GreaterThan Ex.AssocLeft]]


\end{code}
The \textless?\textgreater  operator is used to provide more meaningful error messages in case
invald syntax is detected during parsing. In this case, if an expression is expected during
parsing, but is not matched, the parser will state that an expression is expected.
\begin{code}
expr :: Parser Exp
expr =  Ex.buildExpressionParser binops factor
    <?> ("Expression")

\end{code}
For each type constructor defined for the Exp data type, a corresponding expression 
parser must exist. Each parser must return the data type specified in the Parser type constructor
in the function signature definition. The ``return" at the end of each parser returns a complete
data type that is not modified any more in the parsing phase.
\begin{code}
exprInt :: Parser Exp
exprInt = do
    n <- integer
    return $ E_Int (fromInteger n)

\end{code}
This parser attempts to match an optional class identifier followed be a ``." before a
variable identifier. This is needed for class member referencing.
\begin{code}
exprVar :: Parser Exp
exprVar = do
    classId <- option "" (try (do
        classId <- identifier
        reservedOp "."
        return classId))
    n <- identifier
    return $ E_Id classId n

call :: Parser Exp
call = do
    classId <- option "" (try (do
        classId <- identifier
        reservedOp "."
        return classId))
    name <- identifier
    args <- parens $ commaSep expr
    return $ Call classId (E_Id classId "") name args --todo fix


returnStatement :: Parser Exp
returnStatement = do
    reserved "return"
    value <- expr
    reservedOp ";"
    return value

\end{code}
Only the types of expressions referenced by the factor function are accepted currently. 
returnStatement is not included here as it is explicitly used in the methodDeclaration.

\begin{code}
factor :: Parser Exp
factor = try exprInt
      <|> try call
      <|> try exprVar
      <|> (parens expr)

\end{code}
\subsection{Statements}
Statements are handled by these functions. All statements are followed by the semicolon
reserved operator.
\begin{code}
assign :: Parser Statement
assign = do
    classId <- option "" (try (do
        classId <- identifier
        reservedOp "."
        return classId))
    name <- identifier
    reservedOp "="
    val <- expr
    reservedOp ";"
    return $ S_Assign name classId val

printst :: Parser Statement
printst = do
    reserved "print"
    exp <- parens expr
    reservedOp ";"
    return $ S_Print exp
\end{code}
The void statement is a wrapper for an expression. This allows functions calls to be made
without being part of another statement for example.
\begin{code}
voidst :: Parser Statement
voidst = do
    exp <- expr
    reservedOp ";"
    return $ S_Void exp

ifStatement :: Parser Statement
ifStatement = do
    reserved "if"
    cond <- parens expr
    reserved "then"
    tr <- statement
    reserved "else"
    fl <- statement
    return $ S_If cond tr fl

whileStatement :: Parser Statement
whileStatement = do
    reserved "while"
    cond <- parens expr
    body <- statement
    return $ S_While cond body

block :: Parser Statement
block = do
    s <- braces $ many statement
    return $ S_Block s

statement :: Parser Statement
statement = try block
    <|> try assign
    <|> try printst
    <|> try voidst
    <|> try ifStatement
    <|> try whileStatement
\end{code}
\subsection{Member Declarations}
The variable parser simply matches a type followed by an identifier. It is used by many other
parsers. The fieldDeclaration parser is responsible for matching class member declarations.
Although ``static" is matched against, it holds no special meaning as there is no corresponding
code generation feature for it.
\begin{code}
variable :: Parser VarDecl
variable = do
    t <- type_
    name <- identifier
    return $ VarDecl t name

fieldDeclaration :: Parser VarDecl
fieldDeclaration = do
    optional (reserved "static")
    var <- variable
    reservedOp ";"
    return var <?> ("field declaration")

\end{code}
Method generation parser is slightly more complex than the other parsers due to a method
having more parts to it than regular statements.
\begin{code}
methodDeclaration :: Parser MethodDecl
methodDeclaration = do
    optional (reserved "static")
\end{code}
First we attempt to match a type and a name,
\begin{code}
    t       <- type_
    name    <- identifier
\end{code}
then a comma delimited array of variables surrounded by parenthesis,
\begin{code}
    args    <- parens $ commaSep variable
\end{code}
and finally the method body definition, which starts and ends with curly braces.
\begin{code}
    (vars, stats, ret) <- braces (do {
\end{code}
I experimented with a permutation parser to try and allow arbitrary interspersion of
local variable declarations to little success. In the end I decided to enforce all 
local declarations to be at the top of the method body.
\begin{code}
          vars    <- many $ try ( do { v <- variable;
                                       reservedOp ";";
                                       return v 
                                  })
        ; stats   <- many statement

\end{code}
Every method body must contain exactly one return statement. This makes parsing much easier, 
but may make some function design awkward. Perhaps a ``goto" construct in the language could 
help with this.
\begin{code}
        ; ret     <- returnStatement
        ; return (vars, stats, ret)
    })
    return $ MethodDecl t name args vars stats ret
    <?> ("method declaration")
\end{code}
\subsection{Class Definitions}
The classBody parser attempts to match all the field and method member declarations in a
class definition. It is another instance of failed experimentation with permutation parsers.
Hence, all field declarations must preceed method declarations. 
\begin{code}
classBody :: Parser ([VarDecl], [MethodDecl])
classBody = do
    fd <- option [] (many (try fieldDeclaration))
    md <- option [] (many (try methodDeclaration) )
    return (fd, md)

\end{code}
Every program consists of one or more class declarations.
\begin{code}
classDeclaration :: Parser ClassDecl
classDeclaration = do
    reserved "class"
    name <- identifier
    (fd, md) <- braces classBody
    return $ ClassDecl name Nothing fd md

program :: Parser Program
program = do
    mc <- classDeclaration
    cs <- many classDeclaration
    return $ Program mc cs
\end{code}
\subsection{Top Level}
\begin{code}
defn :: Parser Program
defn = program

\end{code}
This helper function removes starting whitespaces.
\begin{code}
contents :: Parser a -> Parser a
contents p = do
  whitespace
  r <- p
  eof
  return r

\end{code}
These functions are the ones that start the top down parsing execution.
\begin{code}
toplevel :: Parser Program
toplevel = do
    def <- defn
    return def

--This function is not used.
--parseExpr :: String -> Either ParseError Exp
--parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError Program
parseToplevel s = parse (contents toplevel) "<stdin>" s
\end{code}
