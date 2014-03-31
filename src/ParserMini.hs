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
    reserved "int"
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
--
--operator :: Parser String
--operator = do
--  c <- Tok.opStart emptyDef
--  cs <- many $ Tok.opLetter emptyDef
--  return (c:cs)
--
--op :: Parser String
--op = do
--    whitespace
--    o <- operator
--    whitespace
--    return o

binops = [[binary "*" Multiply Ex.AssocLeft
          ,binary "/" Divide Ex.AssocLeft]
         ,[binary "+" Add Ex.AssocLeft
          ,binary "-" Subtract Ex.AssocLeft]
         ,[binary "<" LessThan Ex.AssocLeft
          ,binary ">" GreaterThan Ex.AssocLeft]]

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
    return $ Call cn (E_Id "") name args --todo fix

program :: Parser Program
program = do
    mc <- classDeclaration
    cs <- many classDeclaration
    return $ Program mc cs

classDeclaration :: Parser ClassDecl
classDeclaration = do
    reserved "class"
    name <- identifier
    (fd, md) <- braces classBody
--    (fd, md) <- braces (do{
----         fd <- try(many (fieldDeclaration))
--       md <- many methodDeclaration
--       ; return ([], md)
--    })
    return $ ClassDecl name Nothing fd md

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
    (vars, stats, ret) <- braces (do {
          vars    <- many variable
        ; stats   <- many statement
        ; ret     <- returnStatement
        ; return (vars, stats, ret)
    })
    return $ MethodDecl t name args vars stats ret
    <?> ("method declaration")

classBody :: Parser ([VarDecl], [MethodDecl])
--classBody = permute ( f
--        <$?> ((many (try (fieldDeclaration)) ) )
--        <|?> ((many (try (methodDeclaration)) )  ))
--    where
--    f a b = (a, b)
classBody = do
    fd <- option [] (many (try fieldDeclaration))
    md <- option [] (many (try methodDeclaration) )
    return (fd, md)

statement :: Parser Statement
statement = try block
    <|> try assign
    <|> try printst
    <|> try voidst
    <|> try ifStatement
    <|> try whileStatement

assign :: Parser Statement
assign = do
    classId <- option "" (do
        classId <- identifier
        reservedOp "."
        return classId)
    name <- identifier
    char '='
    val <- expr
    reservedOp ";"
    return $ S_Assign name classId val

printst :: Parser Statement
printst = do
    reserved "print"
    exp <- parens expr
    reservedOp ";"
    return $ S_Print exp
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

defn :: Parser Program
defn = program

contents :: Parser a -> Parser a
contents p = do
  whitespace
  r <- p
  eof
  return r

toplevel :: Parser Program
toplevel = do
    def <- defn
    return def

parseExpr :: String -> Either ParseError Exp
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError Program
parseToplevel s = parse (contents toplevel) "<stdin>" s
