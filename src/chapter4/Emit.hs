{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as BSS
import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import JIT
import qualified Syntax as S

toSig :: [BS.ByteString] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name $ BSS.toShort x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  define double (BS.pack name) fnargs bls
  where
    fnargs = toSig (map BS.pack args)
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name $ BSS.toShort $ BS.pack a))
        assign (BS.pack a) var
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external double (BS.pack name) fnargs
  where fnargs = toSig (map BS.pack args)

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
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

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var x) = getvar (BS.pack x) >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name $ BSS.toShort $ BS.pack fn)) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = do
  res <- runJIT oldast
  case res  of
    Right newast -> return newast
    Left err     -> putStrLn err >> return oldast
  where
    modn    = mapM codegenTop fns
    oldast = runLLVM mod modn
