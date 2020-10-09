{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen
  ( double

  , localRef
  , globalRef
  , externfRef

  , Codegen
  , execCodegen
  , entryBlockName
  , addBlock
  , setBlock

  , assignVar
  , getVar

  , fadd
  , fsub
  , fmul
  , fdiv
  , fcmp
  , uitofp
  , toArgs

  , call
  , alloca
  , store
  , load

  , br
  , cbr
  , ret

  , uniqueName
  ) where

import Data.Bifunctor (first)
import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS

import Control.Monad.State
import Control.Applicative

import qualified LLVM.AST as LLVM

import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Linkage as L

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
double :: LLVM.Type
double = LLVM.FloatingPointType LLVM.DoubleFP

-- Specializing references to type Double

localRef ::  LLVM.Name -> LLVM.Operand
localRef = LLVM.LocalReference double

globalRef ::  LLVM.Name -> C.Constant
globalRef = C.GlobalReference double

externfRef :: LLVM.Name -> LLVM.Operand
externfRef = LLVM.ConstantOperand . C.GlobalReference double

-------------------------------------------------------------------------------
-- Code generation for a single basic block

type SymbolTable = [(BS.ByteString, LLVM.Operand)]

data CodegenState = CodegenState
  { cs_currentBlock :: LLVM.Name                 -- Name of the active block to append to
  , cs_blocks       :: Map.Map LLVM.Name BlockState -- Blocks for function
  , cs_symbolTable  :: SymbolTable              -- Function scope symbol table
  , cs_blockCount   :: Int                      -- Count of basic blocks
  , cs_instrCount   :: Word                     -- Count of unnamed instructions
  , cs_nameSupply   :: Names                    -- Name Supply
  } deriving Show

data BlockState = BlockState
  { bs_index :: Int                            -- Block index
  , bs_stack :: [LLVM.Named LLVM.Instruction]            -- Stack of instructions
  , bs_terminator :: Maybe (LLVM.Named LLVM.Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map BS.ByteString Int

-- Hey, this isn't guaranteed to make unique names
uniqueName :: BS.ByteString -> Names -> (BS.ByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm <> BS.pack (show ix), Map.insert nm (ix+1) ns)

getName :: BS.ByteString -> Codegen BS.ByteString
getName requestedName = do
  nameSupply <- gets cs_nameSupply
  let (name, nameSupply') = uniqueName requestedName nameSupply
  modify $ \cs -> cs { cs_nameSupply = nameSupply' }
  pure name

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

-- The point of the `Codegen` monad is to create a self-contained sequence of
-- basic blocks.
execCodegen :: Codegen a -> [LLVM.BasicBlock]
execCodegen m = createBlocks $ execState (runCodegen m) emptyCodegen

createBlocks :: CodegenState -> [LLVM.BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (cs_blocks m)
  where
  sortBlocks :: [(LLVM.Name, BlockState)] -> [(LLVM.Name, BlockState)]
  sortBlocks = sortBy (compare `on` (bs_index . snd))

  makeBlock :: (LLVM.Name, BlockState) -> LLVM.BasicBlock
  makeBlock (label, (BlockState _ s t)) =
    LLVM.BasicBlock label (reverse s) (maketerm t)
    where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show label

entryBlockName :: BS.ByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState
  { bs_index = i
  , bs_stack = []
  , bs_terminator = Nothing
  }

emptyCodegen :: CodegenState
emptyCodegen = CodegenState
  { cs_currentBlock = LLVM.Name $ BSS.toShort entryBlockName
  , cs_blocks = Map.empty
  , cs_symbolTable = []
  , cs_blockCount = 1
  , cs_instrCount = 0
  , cs_nameSupply = Map.empty
  }

freshNum :: Codegen Word
freshNum = do
  i <- gets cs_instrCount
  let i1 = i+1
  modify $ \s -> s { cs_instrCount = i1 }
  return i1

instr :: LLVM.Instruction -> Codegen (LLVM.Operand)
instr ins = do
  ref <- LLVM.UnName <$> freshNum
  blk <- current
  let stack = bs_stack blk
  modifyBlock (blk { bs_stack = (ref LLVM.:= ins) : stack } )
  return $ localRef ref

terminator :: LLVM.Named LLVM.Terminator -> Codegen (LLVM.Named LLVM.Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { bs_terminator = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen LLVM.Name
entry = gets cs_currentBlock

addBlock :: BS.ByteString -> Codegen LLVM.Name
addBlock blockName = do
  qname <- LLVM.Name . BSS.toShort <$> getName blockName

  blocks <- gets cs_blocks
  ix  <- gets cs_blockCount
  let new = emptyBlock ix

  modify $ \s -> s { cs_blocks = Map.insert qname new blocks
                   , cs_blockCount = ix + 1
                   }
  return qname

setBlock :: LLVM.Name -> Codegen LLVM.Name
setBlock blockName = do
  modify $ \s -> s { cs_currentBlock = blockName }
  return blockName

getBlock :: Codegen LLVM.Name
getBlock = gets cs_currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets cs_currentBlock
  modify $ \s -> s { cs_blocks = Map.insert active new (cs_blocks s) }

current :: Codegen BlockState
current = do
  c <- gets cs_currentBlock
  blks <- gets cs_blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assignVar :: BS.ByteString -> LLVM.Operand -> Codegen ()
assignVar var x = do
  locals <- gets cs_symbolTable
  modify $ \s -> s { cs_symbolTable = [(var, x)] <> locals }

getVar :: BS.ByteString -> Codegen LLVM.Operand
getVar var = do
  syms <- gets cs_symbolTable
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------
-- Instructions (wrapped with `instr`)

-- Arithmetic and Constants

fadd :: LLVM.Operand -> LLVM.Operand -> Codegen LLVM.Operand
fadd a b = instr $ LLVM.FAdd LLVM.noFastMathFlags a b []

fsub :: LLVM.Operand -> LLVM.Operand -> Codegen LLVM.Operand
fsub a b = instr $ LLVM.FSub LLVM.noFastMathFlags a b []

fmul :: LLVM.Operand -> LLVM.Operand -> Codegen LLVM.Operand
fmul a b = instr $ LLVM.FMul LLVM.noFastMathFlags a b []

fdiv :: LLVM.Operand -> LLVM.Operand -> Codegen LLVM.Operand
fdiv a b = instr $ LLVM.FDiv LLVM.noFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> LLVM.Operand -> LLVM.Operand -> Codegen LLVM.Operand
fcmp cond a b = instr $ LLVM.FCmp cond a b []

uitofp :: LLVM.Type -> LLVM.Operand -> Codegen LLVM.Operand
uitofp ty a = instr $ LLVM.UIToFP a ty []

toArgs :: [LLVM.Operand] -> [(LLVM.Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects

call :: LLVM.Operand -> [LLVM.Operand] -> Codegen LLVM.Operand
call fn args = instr $ LLVM.Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: LLVM.Type -> Codegen LLVM.Operand
alloca ty = instr $ LLVM.Alloca ty Nothing 0 []

store :: LLVM.Operand -> LLVM.Operand -> Codegen LLVM.Operand
store ptr val = instr $ LLVM.Store False ptr val Nothing 0 []

load :: LLVM.Operand -> Codegen LLVM.Operand
load ptr = instr $ LLVM.Load False ptr Nothing 0 []

-------------------------------------------------------------------------------
-- Control Flow (wrapped with `terminator`)

br :: LLVM.Name -> Codegen (LLVM.Named LLVM.Terminator)
br val = terminator $ LLVM.Do $ LLVM.Br val []

cbr :: LLVM.Operand -> LLVM.Name -> LLVM.Name -> Codegen (LLVM.Named LLVM.Terminator)
cbr cond tr fl = terminator $ LLVM.Do $ LLVM.CondBr cond tr fl []

ret :: LLVM.Operand -> Codegen (LLVM.Named LLVM.Terminator)
ret val = terminator $ LLVM.Do $ LLVM.Ret (Just val) []
