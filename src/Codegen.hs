{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.General.AST
import qualified LLVM.General.AST.Global as A.G
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.Linkage as L
import Debug.Trace
-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

data Module2 =
    Module2 { modul :: AST.Module,
                classFT :: [ClassFieldTable] }
module2modul (Module2 m _) = m
module2Definitions (Module2 m _) = moduleDefinitions m
module2FTs (Module2 _ ft) = ft

newtype LLVM a = LLVM { unLLVM :: State Module2 a }
  deriving (Functor, Applicative, Monad, MonadState Module2)

runLLVM :: Module2 -> LLVM a -> Module2
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> Module2
emptyModule label = Module2 { modul = defaultModule { moduleName = label, moduleDefinitions =  []}, classFT = []}

putcharDef = external (IntegerType 32) "putchar" [(IntegerType 32, "i")]

addDefn :: Definition -> LLVM ()
addDefn newdef = do
  defs <- gets module2Definitions
  (Module a b c d) <- gets module2modul
  modify $ \s -> s { modul = (Module a b c (defs ++ [newdef])) }

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    A.G.name        = Name label
  , A.G.parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , A.G.returnType  = retty
  , A.G.basicBlocks = body
  }

external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    A.G.name        = Name label
  , A.G.parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , A.G.returnType  = retty
  , A.G.basicBlocks = []
  }

defineClassStruct :: Name -> [(Type, Name)] -> LLVM ()
defineClassStruct nm vars = do
    fts <- gets module2FTs
    modify $ \s -> s { classFT = fts ++ [ft] }
    addDefn $ ty
--    res2 <- addDefn $ GlobalDefinition $ globalVariableDefaults
--        { A.G.name = nm
--        , A.G.type' = ty
--        }

    where
        ty = TypeDefinition nm $ Just $ (StructureType False $ map fst vars )
        vars2 = map (\(x, AST.Name n) -> (x, n)) vars
        ft = ClassFieldTable ty vars2

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
double :: Type
double = FloatingPointType 64 IEEE

integer_:: Word32 -> Type
integer_ = IntegerType

clazz_ :: [Type] -> Type
clazz_ ty = StructureType False ty

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

instance IsString Name where
  fromString = Name . fromString

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, (Type, Operand))]

data ClassFieldTable
    = ClassFieldTable { ty :: AST.Definition, fields :: [(AST.Type, String)] }
    deriving (Eq, Show)


data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  , mod          :: [ClassFieldTable]
  , currentClass :: String
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: [ClassFieldTable] -> String -> CodegenState
emptyCodegen cft clazz = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty cft clazz

execCodegen :: [ClassFieldTable]-> String -> Codegen a -> CodegenState
execCodegen cft clazz m = execState (runCodegen m) $ emptyCodegen cft clazz

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------
assign :: String -> Type -> Operand -> Codegen ()
assign var ty x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, (ty, x))] ++ lcls }

getvar :: String -> Codegen (Type, Operand)
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

--declareClass :: String -> Type -> Codegen ()
--declareClass nm cl = do
--    typs <- gets typetab
--    case lookup nm typs of
--        Just x -> error $ "Multiple declaration of class " ++ show cl
--        Nothing -> modify $ \s -> s { typetab = [(nm, cl)] ++ typs }
-------------------------------------------------------------------------------

-- References
local ::  Name -> Operand
local = LocalReference

global ::  Name -> C.Constant
global = C.GlobalReference

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference

-- Arithmetic and Constants
icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr $ ICmp cond a b []

iadd :: Operand -> Operand -> Codegen Operand
iadd a b = instr $ Add False False a b []

isub :: Operand -> Operand -> Codegen Operand
isub a b = instr $ Sub False False a b []

imul :: Operand -> Operand -> Codegen Operand
imul a b = instr $ Mul False False a b []

idiv :: Operand -> Operand -> Codegen Operand
idiv a b = instr $ SDiv False a b []

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
