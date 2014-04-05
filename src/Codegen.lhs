\section{Code Generation Backend}
This module contains the backend for generating LLVM IR from the syntax tree. It wraps
many llvm-general functions for use by the IR emitter.
\begin{code}
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
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.Linkage as L
import Debug.Trace
\end{code}
\subsection{Module Level}
Since LLVM has no support for keeping track of named class members, I had to use this
hack to add this functionality onto the llvm-general module. It makes code rather ugly,
but it works. The Module is what is translated by llvm-general and used to emit IR.
\begin{code}
data Module2 =
    Module2 { modul   :: AST.Module,
              classFT :: [ClassFieldTable] }
              
module2modul (Module2 m _) = m
module2Definitions (Module2 m _) = moduleDefinitions m
module2FTs (Module2 _ ft) = ft
\end{code}
The Module is wrapped in a state monad which changes as the syntax tree unravels.
\begin{code}
newtype LLVM a = LLVM { unLLVM :: State Module2 a }
  deriving (Functor, Applicative, Monad, MonadState Module2)
runLLVM :: Module2 -> LLVM a -> Module2
runLLVM = do
    flip (execState . unLLVM)
\end{code}
An empty module is used as an intial state.
\begin{code}
emptyModule :: String -> Module2
emptyModule label
  = Module2 { modul = defaultModule
                          { moduleName = label
                          , moduleDefinitions = []}
            , classFT = [] }
\end{code}
These external definitions are useful for debugging and testing the compiler.
\begin{code}
putcharDef = external (IntegerType 32) "putchar"
                      [(IntegerType 32, "i")]
printDef = external (IntegerType 32) "printf"
                    [(IntegerType 8, "i"), (IntegerType 8, "s")]
\end{code}
Any function definition, class definition is added to the module using either the define
function or defineClassStruct function. Both of these functions then call the addDefn function,
which modifies the excuting Module state.
\begin{code}
addDefn :: Definition -> LLVM ()
addDefn newdef = do
  defs <- gets module2Definitions
  (Module a b c d) <- gets module2modul
  modify $ \s -> s { modul = (Module a b c (defs ++ [newdef])) }
define ::  Type -> String -> [(Type, Name)] -> [BasicBlock]
        -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    A.G.name        = Name label
  , A.G.parameters  = ( [Parameter ty nm [] | (ty, nm) <- argtys], False)
  , A.G.returnType  = retty
  , A.G.basicBlocks = body
  }
\end{code}
The external function handles external function prototypes like putchar.
\begin{code}
external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    A.G.name        = Name label
  , A.G.parameters  = ( [Parameter ty nm [] | (ty, nm) <- argtys], False)
  , A.G.returnType  = retty
  , A.G.basicBlocks = []
  }
\end{code}
Whenever a class is defined, a llvm struct type is created with the appropriate field
members. Functions are assigned to a class via special compiler prefixes, or would be
in a more complete version. 
\begin{code}
defineClassStruct :: Name -> [(Type, Name)] -> LLVM ()
defineClassStruct nm@(Name nn) vars = do
    fts <- gets module2FTs
    modify $ \s -> s { classFT = fts ++ [ft] }
\end{code}
In this version a global variable of the class type is declared
automatically for each class definition. I have found this useful for debugging.
\begin{code}
    addDefn $ ty
    addDefn $ GlobalDefinition $ globalVariableDefaults
        { A.G.name = AST.Name $ nn ++  "0"
        , A.G.type' = NamedTypeReference nm
        }
    where
        ty = TypeDefinition nm $ Just st
        st = (StructureType False $ map fst vars )
        vars2 = map (\(x, AST.Name n) -> (x, n)) vars
        ft = ClassFieldTable ty vars2
\end{code}
\subsection{Names}
Names are used to keep track of the symbols defined. This code is untouched from
Stephen Diel's tutorials.
\begin{code}
type Names = Map.Map String Int
uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)
instance IsString Name where
  fromString = Name . fromString
\end{code}
\subsection{Codegen}
A symbol table contains a list of id, type and operand pairs. Operand roughly maps to
a reference to an instance of a symbol here.
\begin{code}
type SymbolTable = [(String, (Type, Operand))]
\end{code}
ClassFieldTable is something I made up on the spot to keep track of which named member fields
corresponded to which type in each class. It is necessary as LLVM does not keep track of named
fields.
\begin{code}
data ClassFieldTable
    = ClassFieldTable { ty :: AST.Definition
                      , fields :: [(AST.Type, String)] }
    deriving (Eq, Show)
getFTFields (ClassFieldTable _ fds) = fds
\end{code}
I did not look into BlockState, which worked fine straight out of Stephen Diel's tutorials.
It is another data type used in managing the blocks that LLVM uses.
\begin{code}
data BlockState
  = BlockState {
    idx   :: Int
  , stack :: [Named Instruction]
  , term  :: Maybe (Named Terminator)
} deriving Show
-- %%\ignore{cc = 5}
\end{code}
The data type CodegenState keeps track of many important code generation states(obviously).
During the generation of code, each class has its own code generation state which keeps track of
the LLVM blocks assigned, the symbol table, the class field table, and the current class and
class instance. Like the LLVM Module state, the CodegenState mutates frequently as the program
traverses the syntax tree.
\begin{code}
data CodegenState
  = CodegenState {
    -- Name of the active block to append to
    currentBlock :: Name
    -- Blocks for function
  , blocks       :: Map.Map Name BlockState
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  , ft           :: [ClassFieldTable]
  , currentClass :: (String, Maybe Operand)
  } deriving Show

codeFieldTable :: CodegenState -> [ClassFieldTable]
codeFieldTable (CodegenState _ _ _ _ _ _ ft _) = ft
codeCurrentClass (CodegenState _ _ _ _ _ _ _ cl) = cl
\end{code}
Since each class declaration is passed its own CodegenState, a global table of all classes definitions
must be passed to each CodegenState. This is a hack around global mutatable states in Haskell.
\begin{code}
emptyCodegen :: [ClassFieldTable] -> String -> CodegenState
emptyCodegen cft clazz = CodegenState (Name entryBlockName)
                                      Map.empty [] 1 0
                                      Map.empty cft
                                      (clazz, Nothing)

execCodegen :: [ClassFieldTable]-> String -> Codegen a -> CodegenState
execCodegen cft clazz m = execState ( runCodegen m) $ emptyCodegen cft clazz

newtype Codegen a = Codegen{ runCodegen::State CodegenState a }
  deriving (Functor,Applicative,Monad,MonadState CodegenState )
\end{code}
\subsection{Block Management}
As stated before, this part of the code I did not touch much. These functions are used to manage
blocks and reference them.
\begin{code}
sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: "++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

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
\end{code}
\subsection{Instructions}
All LLVM instructions results are named and stored in virtual registers, which LLVM keeps track of.
Fresh keeps track of the current unnamed number in a block.
\begin{code}
fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1
\end{code}
This function wraps any LLVM instruction and does the necessary block manipulation .
\begin{code}
instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ref
\end{code}
The terminator instruction indicates the end of a block, which corresponds to a method.
\begin{code}
terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm
\end{code}
\subsection{Symbol Table}
These functions are associated with retrieving, or modifying the symbol table elements. Their purposes should
be self evident.
\begin{code}
assign :: String -> Type -> Operand -> Codegen ()
assign var ty x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, (ty, x))] ++ lcls }
getvar :: String -> Codegen (Type, Operand)
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: "++show var
\end{code}
\subsection{References}
These are just wrappers to LLVM references.
\begin{code}
local ::  Name -> Operand
local = LocalReference
global ::  Name -> C.Constant
global = C.GlobalReference
externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference
\end{code}
\subsection{Arithmetic and Contants}
\begin{code}
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
\end{code}
\subsection{Side effects}
These functions include calling functions, allocation of stack memory, and loading and storing values to and from
registers.
\begin{code}
toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) (toArgs args) [] []
alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []
store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []
load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

\end{code}
\subsection{Control Flow}
These functions include breaks, conditional breaks, and keeping track of control statement labels.
\begin{code}
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []
cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []
phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []
ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
\end{code}
