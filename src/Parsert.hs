module Parsert where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexert
import Syntaxt

int :: Parser Expr
int = do
  n <- integer
  return $ E_Int (fromInteger n)

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

binops = [[binary "*" Times Ex.AssocLeft,
           binary "/" Divide Ex.AssocLeft]
         ,[binary "+" Plus Ex.AssocLeft,
           binary "-" Minus Ex.AssocLeft]]

expr :: Parser Expr
expr =  Ex.buildExpressionParser binops factor

type_ :: Parser Type
type_ = do
    return T_Int

variable :: Parser VarDecl
variable = do
    t <- type_
    name <- identifier
    return $ VarDecl t name

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name name args --todo fix

classDeclaration :: Parser ClassDecl
classDeclaration = do
    reserved "class"
    name <- identifier
    c <- char '{'
    v <- many fieldDeclaration
    m <- many methodDeclaration
    d <- char '}'
    return $ ClassDecl name v m

fieldDeclaration :: Parser VarDecl
fieldDeclaration = do
    optional (reserved "static")
    var <- variable
    return var

methodDeclaration :: Parser MethodDecl
methodDeclaration = do
    optional (reserved "static")
    t <- type_
    name <- identifier
    args <- parens $ many variable
    op <- char '{'
    vars <- many variable
    stats <- many statement
    ed <- char '}'
    return $ MethodDecl t name args vars stats

statement :: Parser Statement
statement = try block
    <|> try assign
    <|> try ifStatement
    <|> try whileStatement
    <|> try returnStatement

assign :: Parser Statement
assign = do
    name <- identifier
    char '='
    val <- expr
    return $ S_Assign name val


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

returnStatement :: Parser Statement
returnStatement = do
    reserved "return"
    value <- expr
    return $ S_Return value

factor :: Parser Expr
factor = try int
      <|> try call
      <|> (parens expr)

block :: Parser Statement
block = do
    s <- braces $ many statement
    return $ S_Block s

defn :: Parser ClassDecl
defn = try classDeclaration

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [ClassDecl]
toplevel = many $ do
    def <- defn
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [ClassDecl]
parseToplevel s = parse (contents toplevel) "<stdin>" s
