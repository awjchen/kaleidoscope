{-# LANGUAGE OverloadedStrings #-}

module Emit where


import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as C
import qualified LLVM.Context as Context
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.Module as M

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS
import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import ModuleGen
import qualified Syntax as S

--------------------------------------------------------------------------------

codegenTop :: S.Expr -> LLVM ()
codegenTop expr = case expr of
  S.Function name argNames expr ->
    define double name fnargs bls
    where
    fnargs = toSig argNames
    bls = execCodegen $ do
      entryBlock <- addBlock entryBlockName
      setBlock entryBlock
      forM argNames $ \argName -> do
        ptr <- alloca double
        store ptr (localRef (LLVM.Name $ BSS.toShort argName))
        assignVar argName ptr
      codeGen expr >>= ret

  S.Extern name argNames ->
    external double name fnargs
    where
    fnargs = toSig argNames

  exp -> -- catch-all
    define double "main" [] blocks
    where
    blocks = execCodegen $ do
      entryBlock <- addBlock entryBlockName
      setBlock entryBlock
      codeGen exp >>= ret

  where
  toSig :: [BS.ByteString] -> [(LLVM.Type, LLVM.Name)]
  toSig = map (\x -> (double, LLVM.Name $ BSS.toShort x))

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: LLVM.Operand -> LLVM.Operand -> Codegen LLVM.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]

codeGen :: S.Expr -> Codegen LLVM.Operand
codeGen expr = case expr of
  S.UnaryOp op a ->
    codeGen $ S.Call ("unary" <> op) [a]
  S.BinaryOp "=" (S.Var var) val -> do
    a <- getVar var
    cval <- codeGen val
    store a cval
    return cval
  S.BinaryOp op a b -> case Map.lookup op binops of
    Just f -> do
      ca <- codeGen a
      cb <- codeGen b
      f ca cb
    Nothing -> error "No such operator"
  S.Var x ->
    getVar x >>= load
  S.Float n ->
    return $ LLVM.ConstantOperand $ C.Float (F.Double n)
  S.Call fn args -> do
    operandArgs <- mapM codeGen args
    call (externfRef (LLVM.Name $ BSS.toShort fn)) operandArgs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: LLVM.Module -> [S.Expr] -> IO LLVM.Module
codegen llvmModule fns =
  Context.withContext $ \context ->
    M.withModuleFromAST context newAST $ \m -> do
      llstr <- M.moduleLLVMAssembly m
      BS.putStrLn llstr
      return newAST
  where
  newAST  = runLLVM llvmModule $ mapM codegenTop fns
