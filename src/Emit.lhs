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

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

typeToAST :: S.Type -> [ClassFieldTable] -> AST.Type
typeToAST (S.T_Int) _ = AST.IntegerType 32
typeToAST (S.T_Id id) ft = case (findTypeFromModule id ft) of
    Nothing -> error $ "No type exists " ++ id ++ (show ft)
    Just t  -> t

findTypeFromModule :: String -> [ClassFieldTable]
                   -> Maybe AST.Type
findTypeFromModule nm ft =
    case typedef of
        Nothing -> Nothing
        Just (ClassFieldTable (AST.TypeDefinition _ mty) _)
            -> mty
    where
        typedef = find (\def -> case def of
            ClassFieldTable (AST.TypeDefinition
                                (AST.Name id)
                                mt) _
                -> id == nm
            _                      -> False) ft

varDeclTuple :: [ClassFieldTable] -> S.VarDecl
             -> (AST.Type, AST.Name)
varDeclTuple ft (S.VarDecl ty nm)
    = (typeToAST ty ft, AST.Name nm)

classFunc :: String -> String -> String
classFunc nm func = func

codegenTop :: S.Program -> LLVM ()
codegenTop prog = trace "entering codegen" $ do
    putcharDef
    mc <- codegenClass True (S.progMainClass prog)
    cs <- mapM (codegenClass False) (S.progClassDecls prog)
    return ()

codegenClass :: Bool -> S.ClassDecl -> LLVM ()
codegenClass isMain (S.ClassDecl name _ fd md) = do
    ft <- gets module2FTs
    (res1) <- defineClassStruct (AST.Name name)
                                (map (varDeclTuple ft) fd)
    mapM (codegenMethod name) md
    return ()

codegenMethod :: String -> S.MethodDecl -> LLVM ()
codegenMethod clazz
              (S.MethodDecl ty name args decl body retty) = do
    ft <- gets module2FTs
    define (typeToAST ty ft)
           (classFunc clazz name)
           (map (varDeclTuple ft) ([args2]++args))
           (blks ft)
    return ()
        where
        args2 = (S.VarDecl (S.T_Id clazz) "this")
        blks dd = createBlocks $ execCodegen dd clazz $ do
          entry <- addBlock entryBlockName
          setBlock entry
          -- This operand
          thisop <- alloca $ fst $ param dd args2
          store thisop (local $ AST.Name $ vn args2)
          assign (vn args2) (fst (param dd args2)) thisop
          forM args $ \a -> do
            var <- alloca $ fst $ param dd a
            store var (local $ AST.Name $ vn a)
            assign (vn a) (fst (param dd a)) var
          rr <- mapM (cgenVarDecl) decl
          sss <- gets symtab
          (curTy, _ ) <- gets codeCurrentClass
          modify $ \s -> s{ currentClass= (curTy, Just thisop) }
          res <- mapM (cgenStatement) body
          resret <- cgenExp retty
          ret $ resret
        vn a = S.varName a
        param dd a = varDeclTuple dd a

cgenVarDecl :: S.VarDecl -> Codegen ()
cgenVarDecl vd = do
    ft <- gets codeFieldTable
    let (typ, AST.Name nm) = varDeclTuple ft vd
    newvar <- alloca typ
    lcls <- gets symtab
    modify $ \s -> s { symtab = [(nm, (typ, newvar))] ++ lcls }

cgenStatement :: S.Statement -> Codegen AST.Operand
cgenStatement (S.S_Block ss) = do
    res <- mapM cgenStatement ss
    return $ last res

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

cgenExp (S.Call cid callee fn args) = do
    syms <- gets symtab
    largs <- mapM cgenExp args
    objs <- case (lookup cid syms) of
            Just (cty, cop) -> do
                ll <- load cop
                return [ll]
            Nothing -> return []

    call (externf (AST.Name (classFunc cid fn))) $ objs++largs

cgenExp (S.B_Op op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgenExp a
      cb <- cgenExp b
      f ca cb
    Nothing -> error "No such operator"

classFieldPtr :: String -> String -> Codegen AST.Operand
classFieldPtr classId fieldId = do
    ft <- gets codeFieldTable
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
    case currClazzOp of
        Nothing -> return $ error
          $ "No class field named"++fieldId++" in object "
            ++classId
        Just cop -> do
            let clazzFieldTable = findCurrentClassTable ft ctynm
                in case (clazzFieldTable) of
                    Just cc@(ClassFieldTable
                              (AST.TypeDefinition nm (Just ty))
                               fields) ->
                        case (findIndexOfField cc fieldId) of
                            (Just fieldty, Just n) ->
                                structFieldFromOff cop
                                    $ findOffestOfField ty n
                            (_, Nothing)
                              -> do error ("In class, \
                                     \symbol with id not define\
                                     \d: "++ctynm ++ "."
                                     ++ fieldId)
                    Nothing ->  do error $ "Symbol with id not \
                        \defined: "++ ctynm ++ "." ++ fieldId

addClassIdentifier clazz item = item

findTypeName :: [ClassFieldTable] -> AST.Type -> String
findTypeName ft ty = do
    case (find (\(ClassFieldTable (AST.TypeDefinition nm
                                                     (Just td))
                                   _) -> td == ty) ft) of
        Just (ClassFieldTable (
                AST.TypeDefinition (AST.Name nm) (Just td) )
                _ ) -> nm
        Nothing -> ""

findCurrentClassTable :: [ClassFieldTable] -> String
                      -> Maybe ClassFieldTable
findCurrentClassTable fts cur
    = find (\(ClassFieldTable (AST.TypeDefinition nm _) _)
            -> nm == (AST.Name cur)) fts

findIndexOfField :: ClassFieldTable -> String
                 -> (Maybe AST.Type, Maybe Int)
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
----------------------------------------------------------------
-- Operations
----------------------------------------------------------------

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

----------------------------------------------------------------
-- Compilation
----------------------------------------------------------------

liftError :: ErrorT String IO a -> IO a
liftError = runErrorT >=> either fail return

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
