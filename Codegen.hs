module Codegen where

double :: Type
double = FloatingPointType 64 IEEE

type SymbolTable [(String, Operand)]

data CodegenState
    = CodegenState {
    , currentBlock  :: Name -- name of active block
    , blocks        :: Map.Map Name BlockState --blocks for function
    , symtab        :: SymbolTable -- function scope
    , blockCount    :: Int
    , count         :: Word
    , names         :: Names
} deriving Show

data BlockState
    = BlockState {
    , idx :: Int
    , stack :: [Named Instruction]
    , term :: Maybe (Named Terminator)
} deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
    GlobalDefinition $ functionDefaults {
        name       = Name label
      , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys, False)
      , returnType = retty
      , basicBlocks = body
}

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
    GlobalDefinition $ functionDefaults {
        name        = Name label
      , paramters   = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
      , returnType  = retty
      , basicBlocks = []
}

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
    bls <- gets blocks
    ix  <- gets blockCount
    nms <- gets names

    let new = emptyBlock ix
        (qname, supply) = uniqueName bname nms

    modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                     , blockCount = ix + 1
                     , names = supply
                     }
    return (Name qname)

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s ) }

current :: Codegen Blockstate
current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.loopup c blks of
        Just x -> return x
        Nothing -> error $ "No such block: " ++ show c

fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + i }
    return $ i + 1

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
    case Map.lookup nm ns of
        Nothing -> (nm, Map.insert nm 1 ns)
        Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns )

instance IsString Name where
    fromString = Name . fromString

local :: Name -> Operand
local = LocalReference

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference

assign :: String -> Operand -> Codegen()
assign var x = do
    lcls <- gets symtab
    modify $ \s -> s { symtab = [(var ,x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
    syms <- gets symtab
    case lookup var syms of
        Just x -> return x
        Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: Instruction -> Codegen Operand
instr ins = do
    n <- fresh
    blk <- current
    let i = stack blk
    let ref = (UnName n)
    modifyBlock $ blk { stack = i ++ [ref := ins] }
    return $ local ref

terminator :: Named Terminator -> Codgen (Named Terminator)
terminator trm = do
    blk <- current
    modifyBlock $ blk { term = Just trm }
    return trm

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv a b []

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ Condbr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call False CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []





