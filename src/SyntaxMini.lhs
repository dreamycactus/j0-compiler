\section{Syntax}
The Syntax Module contains data structures which implement
attribute grammar for the language. The structure of the grammar encourages top-down parsing.

\begin{code}
module SyntaxMini where
\end{code}

A Program consists of 1+ class declarations, with the first
declaration being considered the main class.

\begin{code}
data Program     
  = Program ClassDecl [ClassDecl]
       deriving(Eq, Show)

progMainClass (Program mc cd) = mc 
progClassDecls (Program mc cd) = cd
\end{code}

The class declaration data structure is designed in such a way
to support inheritance, but in this revision is not used. The
extends field will always be Nothing.

\begin{code}
data ClassDecl
    = ClassDecl { class_name :: Id
             , extends    :: Maybe Id
             , vars       :: [VarDecl]
             , methods    :: [MethodDecl] }
    deriving (Eq, Show)
\end{code}

Variable and Method declaration data structures are what one
might expect of an imperative language. VarDecl is used both
for local declarations in a function body, arguments to
functions, and class member declarations. This makes sense as
the syntax and data are similar for all of these purposes.

The Method declaration is a bit stricter than usual imperative
languages. It mandates that each method must have exactly one
return expression.

\begin{code}
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
\end{code}

There is only one ``first class" type in my implementation,
the integer. All other types are declared as classes.

\begin{code}
type Id = String

data Type
    = T_Int
    | T_Id Id
    deriving (Eq, Show)
\end{code}

Some of the statement constructors are not used. They are
simply there to demonstrate the possibilities of this schema
ofsyntax attributes. All Statements do not have any values
associated with them, in contrast to expressions, which must
represent a value.

\begin{code}
data Statement
    = S_Block       [Statement]
    | S_If          { cond :: Exp, then_arm :: Statement
                    , else_arm :: Statement }
    | S_While       { cond :: Exp, while_body :: Statement }
    | S_Print       Exp
    | S_Return      Exp
    | S_Void        Exp
    | S_Assign      { var :: Id, classId :: Id, value :: Exp }
    | S_ArrayAssign { var :: Id, arr_index :: Exp
                    , value :: Exp}
    deriving (Eq, Show)
\end{code}

A value with a type must be associated with every expression.
Note that in this scheme, a function call is considered an
expression, and hence the only way to call a function (a naked
expression) is through a S\_Void statement, which is designed
exactly for this purpose.

\begin{code}
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
\end{code}
The Op data type covers binary operators. The parser is capable
of parsing these, but in this revision, LessThan and
GreaterThan may not be implemented fully.
\begin{code}
data Op
    = Add | Subtract | Multiply | Divide | LessThan
          | GreaterThan
    deriving (Eq, Ord, Show)
\end{code}
