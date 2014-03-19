module ParserMini where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Perm

import Lexert
import SyntaxMini

typeInt :: Parser Type
typeInt = do
  n <- integer
  return T_Int

typeId :: Parser Type
typeId = do
    name <- identifier
    return $ T_Id name

type_ :: Parser Type
type_ = do
        try typeInt
    <|> try typeId

binary s f assoc = Ex.Infix (reservedOp s >> return (B_Op f)) assoc

binops = [[binary "*" Multiply Ex.AssocLeft,
           binary "/" Divide Ex.AssocLeft]
         ,[binary "+" Add Ex.AssocLeft,
           binary "-" Subtract Ex.AssocLeft]]

expr :: Parser Exp
expr =  Ex.buildExpressionParser binops factor
    <?> ("Expression")

exprInt :: Parser Exp
exprInt = do
    n <- integer
    return $ E_Int (fromInteger n)

exprVar :: Parser Exp
exprVar = do
    n <- identifier
    return $ E_Id n

factor :: Parser Exp
factor = try exprInt
      <|> try call
      <|> try exprVar
      <|> (parens expr)

variable :: Parser VarDecl
variable = do
    t <- type_
    name <- identifier
    return $ VarDecl t name

call :: Parser Exp
call = do
    cn <- option ("this") (do
        className <- identifier
        _         <- char '.'
        return className)
    name <- identifier
    args <- parens $ commaSep expr
    return $ Call cn (E_Id cn) name args --todo fix

classDeclaration :: Parser ClassDecl
classDeclaration = do
    reserved "class"
    name <- identifier
    c <- char '{'
    ms <- many methodDeclaration
    d <- char '}'
    return $ ClassDecl name Nothing [] ms

fieldDeclaration :: Parser VarDecl
fieldDeclaration = do
    optional (reserved "static")
    var <- variable
    reservedOp ";"
    return var <?> ("field declaration")

methodDeclaration :: Parser MethodDecl
methodDeclaration = do
    optional (reserved "static")
    t       <- type_
    name    <- identifier
    args    <- parens $ commaSep variable
    op      <- char '{'
    vars    <- many variable
    stats   <- many statement
    ret     <- returnStatement
    ed      <- char '}'
    return $ MethodDecl t name args vars stats ret
    <?> ("methodDeclaration")

methodBody :: Parser ([Statement], [VarDecl])
methodBody = permute (f <$?> ([], many statement)
                        <|?> ([], many fieldDeclaration) )
        where
            f a b = (a, b)

statement :: Parser Statement
statement = try block
    <|> try assign
    <|> try ifStatement
    <|> try whileStatement

assign :: Parser Statement
assign = do
    name <- identifier
    char '='
    val <- expr
    reservedOp ";"
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

returnStatement :: Parser Exp
returnStatement = do
    reserved "return"
    value <- expr
    reservedOp ";"
    return value

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

parseExpr :: String -> Either ParseError Exp
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [ClassDecl]
parseToplevel s = parse (contents toplevel) "<stdin>" s
