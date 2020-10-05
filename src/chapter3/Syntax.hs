module Syntax where

import qualified Data.ByteString.Char8 as BS

type Name = BS.ByteString

data Expr
  = Float Double
  | Var BS.ByteString
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  deriving (Eq, Ord, Show)
