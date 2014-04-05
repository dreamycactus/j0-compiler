\section{Code Emitter}
This module contains the front end code to travel the syntax tree and emit the corresponding IR.
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP

import Data.Word
import Data.Int
import Data.List
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import qualified Data.Map as Map

import Debug.Trace

import Codegen
import qualified SyntaxMini as S

\end{code}
\subsection{Compilation}
These functions start the process in which the syntax tree data is transformed into LLVM module data.
\begin{code}
liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return
\end{code}
The codegen function takes a module and a syntax tree and wraps it in an error handling context and executes
the llvm-general Module to IR generator.
\begin{code}
codegen :: Module2 -> S.Program -> IO (Module2, String)
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return (mimi, llstr)
  where
    modn    = codegenTop fns
    mimi@(Module2 newast _ ) = runLLVM mod modn
\end{code}
\subsection{Top Down Generation}
Top down traversal starts here. The codegenClass function runs on each class definition in the program.
\begin{code}
codegenTop :: S.Program -> LLVM ()
codegenTop prog = trace "entering codegen" $ do
    putcharDef
    mc <- codegenClass True (S.progMainClass prog)
    cs <- mapM (codegenClass False) (S.progClassDecls prog)
    return ()
\end{code}
Here we call the function that declares an LLVM struct with the appropriate fields, and run codegenMethod on
the method declarations in the class.
\begin{code}
codegenClass :: Bool -> S.ClassDecl -> LLVM ()
codegenClass isMain (S.ClassDecl name _ fd md) = do
    ft <- gets module2FTs
    (res1) <- defineClassStruct (AST.Name name) (map (varDeclTuple ft) fd)
    mapM (codegenMethod name) md
    return ()
\end{code}
Method codegen is more involved. On the highest level, we define a function after retrieving the type data,
the function id, and the transformed syntax data types to types LLVM recognizes. A code block must also be
specified.
\begin{code}
codegenMethod :: String -> S.MethodDecl -> LLVM ()
codegenMethod clazz (S.MethodDecl ty name args decl body retty) = do
    ft <- gets module2FTs
    define (typeToAST ty ft)
           (classFunc clazz name)
           (map (varDeclTuple ft) ([args2]++args))
           (blks ft)
    return ()
        where
        args2 = (S.VarDecl (S.T_Id clazz) "this")
\end{code}
This is the subfunction that creates the block containing the function logic. First we initialize the block
with entry label.
\begin{code}
        blks dd = createBlocks $ execCodegen dd clazz $ do
          entry <- addBlock entryBlockName
          setBlock entry
\end{code}
Each class function takes a reference to itself as the first argument. This would not be the case for 
static functions, but they are not implemented right now. A copy of each argument is made in order to implement
pass-by-value behavior.
\begin{code}
          thisop <- alloca $ fst $ param dd args2
          store thisop (local $ AST.Name $ vn args2)
          assign (vn args2) (fst (param dd args2)) thisop
\end{code}
For each argument, we do the same as above.
\begin{code}
          forM args $ \a -> do
            var <- alloca $ fst $ param dd a
            store var (local $ AST.Name $ vn a)
            assign (vn a) (fst (param dd a)) var
\end{code}
Now we call the functions that generate code for each local declaration, and make the adjustments in the
symbol table.
\begin{code}
          rr <- mapM (cgenVarDecl) decl
          sss <- gets symtab
\end{code}
We also need to pass information about the current class to the code responsible for generating child nodes
of the syntax tree.
\begin{code}
          (curTy, _ ) <- gets codeCurrentClass
          modify $ \s -> s{ currentClass= (curTy, Just thisop) }
\end{code}
Then we generate the method body which consists of an array of statements, and the return expression.
\begin{code}
          res <- mapM (cgenStatement) body
          resret <- cgenExp retty
          ret $ resret
        vn a = S.varName a
        param dd a = varDeclTuple dd a
\end{code}
\subsection{Statement Codegen}
Statement code generation is the next step in the top down process. A block statement simply does a recursive 
call on each of its statement elements.
\begin{code}
cgenStatement :: S.Statement -> Codegen AST.Operand
cgenStatement (S.S_Block ss) = do
    res <- mapM cgenStatement ss
    return $ last res
\end{code}
In the case the lval is not prefixed with an identifier, the assignment statement either finds 
a local reference matching the lval, or a class field matching the lval,or returns an error. 
If there is a class identifier, then there is no ambiguity. The function classFieldPtr is defined later,
which we will see calculates the offset of the struct field to assign to.
\begin{code}
cgenStatement (S.S_Assign id classId val) = do
    valop <- cgenExp val
    syms <- gets symtab
    case classId of
        ""   -> case (lookup id syms) of
                    Just (symty, symop)  -> do
                        store symop valop
                        return symop
                    Nothing -> do
                        ptrop <- classFieldPtr classId id
                        store ptrop valop
        cid  -> do
                    ptrop <- classFieldPtr classId id
                    store ptrop valop
cgenStatement (S.S_Print e) = do
    res <- cgenExp e
    call (externf (AST.Name "putchar")) [res]
    return res
\end{code}
To implement an if statement, 3 blocks are needed.
\begin{code}
cgenStatement (S.S_If cond t e) = do
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"

\end{code}
First we calculate which branch to jump to by calculating the value of the condition expression.
Note there are no type checks so we do not know if the condition is even valid or not. An integer
value 0 corresponds to FALSE, and all other values are TRUE.
\begin{code}      
    --Entry
    cond <- cgenExp cond
    test <- icmp IP.NE (AST.ConstantOperand (C.Int 32 0)) cond
    cbr test ifthen ifelse
\end{code}
For each branch, we create a label and run codegen on the conditional statements in the branch.
\begin{code}
    --if.then
    setBlock ifthen
    trval <- cgenStatement t
    br ifexit
    ifthen <- getBlock

    --if.else
    setBlock ifelse
    flval <- cgenStatement e
    br ifexit
    ifelse <- getBlock

    --ifexit
    setBlock ifexit
\end{code}
phi is used to keep track of which block we came from and the values stored in registers.
\begin{code}
    phi (AST.IntegerType 32) [(trval, ifthen), (flval, ifelse)]
cgenStatement (S.S_Return exp) = do
    e <- cgenExp exp
    t <- ret e
    return e
cgenStatement (S.S_Void exp) = do
    e <- cgenExp exp
    return e

\end{code}
\subsection{Expression Codegen}
An expression which is just a variable is very much like the assign lval resolution code. We need
to figure out which reference an id refers to.
\begin{code}
cgenExp :: S.Exp -> Codegen AST.Operand
cgenExp (S.E_Int n)
    = return $ cons $ (C.Int 32 (fromIntegral n))
cgenExp (S.E_Id classId id) = do
    syms <- gets    symtab
    case classId of
        ""   -> case (lookup id syms) of
                    Just (symty, symop)  -> do
                        load symop
                    Nothing -> do
                        ptrop <- classFieldPtr classId id
                        load ptrop
        cid  -> do
                    ptrop <- classFieldPtr classId id
                    load ptrop
\end{code}
A function does not know by itself to which class it belongs. This is why we must extract
the class id from the call syntax node.
\begin{code}
cgenExp (S.Call cid callee fn args) = do
    syms <- gets symtab
    largs <- mapM cgenExp args
    objs <- case (lookup cid syms) of
            Just (cty, cop) -> do
                ll <- load cop
                return [ll]
            Nothing -> return []

    call (externf (AST.Name (classFunc cid fn))) $ objs++largs
\end{code}
Binary expression generation calls code generation on each of the operands to get references to their values, 
and then calls the operator function passing the two operand values.
\begin{code}
cgenExp (S.B_Op op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgenExp a
      cb <- cgenExp b
      f ca cb
    Nothing -> error "No such operator"
\end{code}
cgenVarDecl handles code generation for local declarations.
\begin{code}
cgenVarDecl :: S.VarDecl -> Codegen ()
cgenVarDecl vd = do
    ft <- gets codeFieldTable
    let (typ, AST.Name nm) = varDeclTuple ft vd
    newvar <- alloca typ
    lcls <- gets symtab
    modify $ \s -> s { symtab = [(nm, (typ, newvar))] ++ lcls }
\end{code}
This function returns a reference to a structure field given an object id and an offset.
\begin{code}
structFieldFromOff :: AST.Operand -> Int -> Codegen AST.Operand
structFieldFromOff ty off
    = do
          res <- instr $ AST.GetElementPtr
                True
                ty
                [ AST.ConstantOperand $ C.Int 32 0
                , AST.ConstantOperand $ C.Int 32 (fromIntegral off)]
                []
          return res
\end{code}
classFieldPtr returns the reference to a structure field given an object id and field id by doing a lookup
on the class field table. I certainly will not winning any functional elegance awards for this one.
\begin{code}
classFieldPtr :: String -> String -> Codegen AST.Operand
classFieldPtr classId fieldId = do
    ft <- gets codeFieldTable
\end{code}
First we lookup the object id to see whether or not the instance registered in the class field table. We attempt
to discern the class type from this lookup.
\begin{code}
    (ctynm, currClazzOp) <- case classId of
        ""      -> gets codeCurrentClass
        cid     -> do
            syms <- gets symtab
            case lookup classId syms of
                Just (tty, top)
                    -> return $ (findTypeName ft tty, Just top)
                Nothing
                    -> error $ "No local object named "
                        ++ classId
\end{code}
Then we lookup to see if the named field exists inside the class definition.
\begin{code}
    case currClazzOp of
        Nothing -> return $ error $ "No class field named"++fieldId++" in object "++classId
        Just cop -> do
            let clazzFieldTable = findCurrentClassTable ft ctynm
                in case (clazzFieldTable) of
\end{code}
Finally, we find the offset of the field in the class struct type and return the reference to the field.
\begin{code}
                    Just cc@(ClassFieldTable (AST.TypeDefinition nm (Just ty)) fields) 
                        -> case (findIndexOfField cc fieldId) of
                              (Just fieldty, Just n) 
                                           -> structFieldFromOff cop $ findOffestOfField ty n
                              (_, Nothing) -> do error ("In class, symbol with id not defined:" 
                                                       ++ctynm ++ "."
                                                       ++ fieldId)
                    Nothing ->  do error $ "Symbol with id not defined: " ++ ctynm 
                                        ++ "." ++ fieldId
\end{code}
This methods returns the name of a class type.
\begin{code}
findTypeName :: [ClassFieldTable] -> AST.Type -> String
findTypeName ft ty = do
    case (find (\(ClassFieldTable (AST.TypeDefinition nm (Just td)) _) -> td == ty) ft) of
        Just (ClassFieldTable (AST.TypeDefinition (AST.Name nm) (Just td) ) _ ) -> nm
        Nothing -> ""
\end{code}
This methods looks up the class field table given a class name from a list of class field tables.
It is basically a convenience function to search the dictionary.
\begin{code}
findCurrentClassTable :: [ClassFieldTable] -> String-> Maybe ClassFieldTable
findCurrentClassTable fts cur
    = find (\(ClassFieldTable (AST.TypeDefinition nm _) _) -> nm == (AST.Name cur)) fts
\end{code}
The rest of these functions are convenience functions to calculate field offsets, and find type from names.
\begin{code}
findIndexOfField :: ClassFieldTable -> String -> (Maybe AST.Type, Maybe Int)
findIndexOfField (ClassFieldTable _ fields) fd
    = ( liftM fst $ find matchName fields
      , findIndex matchName fields)
    where matchName = (\(ty, nm) -> nm == fd)

findOffestOfField :: AST.Type -> Int -> Int
findOffestOfField (AST.StructureType _ tys) idx
    = foldr (\x acc -> acc + sizeofType x) 0 $ take idx tys

sizeofType :: AST.Type -> Int
sizeofType (AST.IntegerType 32) = 1
sizeofType (AST.StructureType _ tys) = sum $ map sizeofType tys
--sizeofType x = error $ show x

typeToAST :: S.Type -> [ClassFieldTable] -> AST.Type
typeToAST (S.T_Int) _ = AST.IntegerType 32
typeToAST (S.T_Id id) ft = case (findTypeFromModule id ft) of
    Nothing -> error $ "No type exists " ++ id ++ (show ft)
    Just t  -> t

findTypeFromModule :: String -> [ClassFieldTable] -> Maybe AST.Type
findTypeFromModule nm ft =
    case typedef of
        Nothing -> Nothing
        Just (ClassFieldTable (AST.TypeDefinition _ mty) _) -> mty
    where
        typedef = find (\def -> case def of
            ClassFieldTable (AST.TypeDefinition (AST.Name id) mt) _-> id == nm -> False) ft
\end{code}
varDeclTuple is a useful function to convert a variable declaration syntax node to a tuple llvm-general
can use.
\begin{code}
varDeclTuple :: [ClassFieldTable] -> S.VarDecl -> (AST.Type, AST.Name)
varDeclTuple ft (S.VarDecl ty nm)= (typeToAST ty ft, AST.Name nm)

-- This function does basically nothing.
classFunc :: String -> String -> String
classFunc nm func = func
\end{code}
\subsection{Binary Operator Codegen}
\begin{code}
lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- icmp IP.SLT a b
  uitofp (AST.IntegerType 32) test

gt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
gt a b = do
  test <- icmp IP.SGT a b
  uitofp (AST.IntegerType 32) test

binops = Map.fromList [
      (S.Add, iadd)
    , (S.Subtract, isub)
    , (S.Multiply, imul)
    , (S.Divide, idiv)
    , (S.LessThan, lt)
    , (S.GreaterThan, gt)
  ]
\end{code}
