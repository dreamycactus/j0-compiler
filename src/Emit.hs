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

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

typeToAST :: S.Type -> [ClassFieldTable] -> AST.Type
typeToAST (S.T_Int) _ = AST.IntegerType 32
typeToAST (S.T_Id id) ft = case (findTypeFromModule id ft) of
    Nothing -> AST.VoidType
    Just t  -> t

findTypeFromModule :: String -> [ClassFieldTable] -> Maybe AST.Type
findTypeFromModule nm ft =
    case typedef of
        Nothing -> Nothing
        Just (ClassFieldTable (AST.TypeDefinition _ mty) _) -> mty
    where
        typedef = find (\def ->  case def of
            ClassFieldTable (AST.TypeDefinition (AST.Name id) mt) _ -> id == nm
            _                      -> False) ft

varDeclTuple :: [ClassFieldTable] -> S.VarDecl -> (AST.Type, AST.Name)
varDeclTuple ft (S.VarDecl ty nm) = (typeToAST ty ft, AST.Name nm)

classFunc :: String -> String -> String
classFunc nm func = nm ++ "__" ++ func

codegenTop :: S.Program -> LLVM ()
codegenTop prog = trace "entering codegen" $ do
    putcharDef
    mc <- codegenClass True (S.progMainClass prog)
    cs <- mapM (codegenClass False) (S.progClassDecls prog)
    return ()

codegenClass :: Bool -> S.ClassDecl -> LLVM ()
codegenClass isMain (S.ClassDecl name _ fd md) = do
    ft <- gets module2FTs
    (res1) <- defineClassStruct (AST.Name name) (map (varDeclTuple ft) fd)
    mapM (codegenMethod name) md
    return ()

codegenMethod :: String -> S.MethodDecl -> LLVM ()
codegenMethod clazz (S.MethodDecl ty name args decl body retty) = do
    ft <- gets module2FTs
    define (typeToAST ty ft) (classFunc clazz name) (map (varDeclTuple ft) args2) (blks ft)
    return ()
        where
        args2 = [(S.VarDecl (S.T_Id clazz) "this")] ++ args
        blks dd = createBlocks $ execCodegen dd clazz $ do
          entry <- addBlock entryBlockName
          setBlock entry
          forM args2 $ \a -> do
            var <- alloca $ fst $ param dd a
            store var (local $ AST.Name $ vn a)
            assign (vn a) (fst (param dd a)) var
          res <- mapM cgenStatement body
          resret <- cgenExp retty
          ret resret
        vn a = S.varName a
        param dd a = varDeclTuple dd a

cgenStatement :: S.Statement -> Codegen AST.Operand
cgenStatement (S.S_Block ss) = do
    res <- mapM cgenStatement ss
    return $ last res

--cgenStatement (S.S_Assign var classId val) = do
--    (ty, a) <- getvar var
--    cval <- cgenExp val
--    store a cval
--    return cval
--    where
--        realvar v = case classId of
--            "" -> do
--                syms <- gets symtab
--                return $ symvar v syms
--            c  -> AST.GetElementPtr True
--        symvar var syms = Map.lookup var syms

cgenStatement (S.S_Print e) = do
    res <- cgenExp e
    call (externf (AST.Name "putchar")) [res]
    return res

cgenStatement (S.S_If cond t e) = do
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"

    --Entry
    cond <- cgenExp cond
    test <- icmp IP.NE (AST.ConstantOperand (C.Int 32 0)) cond
    cbr test ifthen ifelse

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
    phi (AST.IntegerType 32) [(trval, ifthen), (flval, ifelse)]

cgenStatement (S.S_Return exp) = do
    e <- cgenExp exp
    t <- ret e
    return e
cgenStatement (S.S_Void exp) = do
    e <- cgenExp exp
    return e
cgenExp :: S.Exp -> Codegen AST.Operand
cgenExp (S.E_Int n) = return $ cons $ (C.Int 32 (fromIntegral n))
cgenExp (S.E_Id id) = do
    syms <- gets symtab
    res <- let ss = getval syms
            in case (ss) of
                Just s -> do
                    return $ snd $ snd s
                Nothing -> error $ "Symbol with id not defined: " ++ id
    return res
    where
        getval syms = (find (\(i, (ty,val)) -> i == id) syms)

cgenExp (S.Call cid callee fn args) = do
  largs <- mapM cgenExp args
  call (externf (AST.Name (classFunc cid fn))) largs
cgenExp (S.B_Op op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgenExp a
      cb <- cgenExp b
      f ca cb
    Nothing -> error "No such operator"
addClassIdentifier clazz item = clazz ++ "__" ++ item

--getClazzFieldOffset :: [ClassFieldTable] -> AST.StructureType -> String -> Codegen Maybe Int
--getClazzFieldOffset defs clazzty field = do
--    where
--        fieldIndex fd (AST.StructureType _ tys) = findIndex (\x ->


--codegenStatement (S.S_Block stats) = do
--codegenStatement (S.S_Assign lhs rhs) = do
--codegenStatement (S.S_If cond thenArm elseArm) = do
--codegenStatement (S.S_While cond body) = do
--codegenStatement (S.S_Print exp) = do
--codegenExpr

--codegenTop (S.Function name args body) = do
--  define double name fnargs bls
--  where
--    fnargs = toSig args
--    bls = createBlocks $ execCodegen $ do
--      entry <- addBlock entryBlockName
--      setBlock entry
--      forM args $ \a -> do
--        var <- alloca double
--        store var (local (AST.Name a))
--        assign a var
--      cgen body >>= ret
--
--codegenTop exp = do
--  define double "main" [] blks
--  where
--    blks = createBlocks $ execCodegen $ do
--      entry <- addBlock entryBlockName
--      setBlock entry
--      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

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

cgen :: S.Exp -> Codegen AST.Operand
--cgen (S.UnaryOp op a) = do
--  cgen $ S.Call ("unary" ++ op) [a]
--cgen (S.S_Assign (S.E_Id var) val) = do
--  a <- getvar var
--  cval <- cgen val
--  store a cval
--  return cval
cgen (S.B_Op op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
--cgen (S.E_Id x) = getvar x >>= load
--cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call _ _ fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

codegen :: Module2 -> S.Program -> IO Module2
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return mimi
  where
    modn    = codegenTop fns
    mimi@(Module2 newast _ ) = runLLVM mod modn
