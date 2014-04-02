module SyntaxMini where

data Program
    = Program ClassDecl [ClassDecl]
    deriving (Eq, Show)

progMainClass (Program mc cd) = mc
progClassDecls (Program mc cd) = cd

data ClassDecl
    = ClassDecl { class_name :: Id
             , extends    :: Maybe Id
             , vars       :: [VarDecl]
             , methods    :: [MethodDecl] }
    deriving (Eq, Show)


data VarDecl
    = VarDecl { var_type :: Type, var_id :: Id }
    deriving (Eq, Show)

varName (VarDecl ty id) = id

data MethodDecl
    = MethodDecl { m_type   :: Type
              , m_name   :: Id
              , m_args   :: [VarDecl]
              , decls    :: [VarDecl]
              , body     :: [Statement]
              , m_return :: Exp }
    deriving (Eq, Show)
data Type
    = T_Int
    | T_Id Id
    deriving (Eq, Show)

data Statement
    = S_Block       [Statement]
    | S_If          { cond :: Exp, then_arm :: Statement, else_arm :: Statement }
    | S_While       { cond :: Exp, while_body :: Statement }
    | S_Print       Exp
    | S_Return      Exp
    | S_Void        Exp
    | S_Assign      { var :: Id, classId :: Id, value :: Exp }
    | S_ArrayAssign { var :: Id, arr_index :: Exp, value :: Exp }
    deriving (Eq, Show)

data Op
    = Add | Subtract | Multiply | Divide | LessThan | GreaterThan
    deriving (Eq, Ord, Show)

data Exp
    = B_Op Op Exp Exp
    | E_Index { array_exp, index_exp :: Exp }
    | Length Exp
    | Call { class_ :: Id, callee :: Exp, method :: Id, args :: [Exp] }
    | Function Id [Id] Exp
    | E_Int Int
    | E_false
    | E_true
    | E_Id Id Id
    | E_this
    | E_Not Exp
    deriving (Eq, Ord, Show)
type Id = String
