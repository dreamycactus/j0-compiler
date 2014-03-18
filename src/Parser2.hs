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
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

binops = [[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]]

expr :: Parser Expr
expr =  Ex.buildExpressionParser binops factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many identifier
  body <- expr
  return $ Function name args body

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

classDeclaration :: Parser ClassDecl
classDeclaration = do
    reserved "class"
    name <- identifier
    members <- braces $ many (try fieldDeclaration <|> methodDeclaration)

fieldDeclaration :: Parser VarDecl
fieldDeclaration = do
    reserved "static"
    name <- identifier

methodDeclaration :: Parser MethodDecl
methodDeclaration = do
    reserved "static"
    name <- identifier

ifStatement :: Parser Statement
ifStatement = do
    reserved "if"
    cond <- parens expr
    reserved "then"
    tr <- expr
    reserved "else"
    fl <- expr
    return $ S_If cond tr fl

whileStatement :: Parser Statement
whileStatement = do
    reserved "while"
    cond <- parens expr
    body <- expr
    return $ S_WhileS cond body

returnStatement :: Parser Statement
returnStatement = do
    reserved "return"
    val <- expr
    return S_Return val




factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> (parens expr)

defn :: Parser Expr
defn = try classDeclaration
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s


