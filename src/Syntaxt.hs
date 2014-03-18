module Syntaxt where

type Id = String

data Program
    = Program MainClass [ClassDecl]

data MainClass
    = MainClass { mainClassName :: Id
                , parsName      :: Id
                , mainBody      :: Statement }

data ClassDecl
    = ClassDecl { classId :: Id
                , vars :: [VarDecl]
                , methods :: [MethodDecl] }
        deriving Show

data VarDecl
    = VarDecl { varType :: Type, varId :: Id }
    deriving Show

data MethodDecl
    = MethodDecl { methodType :: Type
                 , methodId   :: Id
                 , methodArgs :: [VarDecl]
                 , decls      :: [VarDecl]
                 , body       :: [Statement] }
    deriving Show

data Statement
    = S_Block   [Statement]
    | S_Assign  { var :: Id, value :: Expr }
    | S_If      { cond :: Expr, thenS :: Statement, elseS :: Statement }
    | S_While   { cond :: Expr, whileBody :: Statement }
    | S_Return  { value :: Expr }
    | S_Print   Expr
    deriving Show

data Type
    = T_Int
    | T_Id Id
    deriving Show

data Expr
    = E_Int Int
    | BinOp Op Expr Expr
    | E_false
    | E_true
    | Call { clazz :: Id, method :: Id, args :: [Expr] }
    | E_Id Id
    deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)

