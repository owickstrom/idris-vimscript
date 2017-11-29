{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vimscript.AST where

import           Data.Hashable
import           Data.String
import           Data.Text
import           GHC.Generics

data NameScope
  = BuiltIn
  | Global
  | Local
  | Script
  | Argument
  deriving (Eq, Show, Generic)

instance Hashable NameScope

newtype Name =
  Name Text
  deriving (Eq, Show, IsString, Generic)

instance Hashable Name

data ScopedName =
  ScopedName NameScope
             Name
  deriving (Eq, Show, Generic)

instance Hashable ScopedName

builtIn :: Text -> ScopedName
builtIn = ScopedName BuiltIn . Name

data BinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  | Equals
  | LT
  | LTE
  | GT
  | GTE
  | Concat
  deriving (Eq, Show)

data Primitive
  = Integer Integer
  | Floating Double
  | String Text
  | List [Expr]
  deriving (Eq, Show)

data Projection
  = ProjSingle Expr -- [expr]
  | ProjFrom Expr -- [expr:]
  | ProjTo Expr -- [:expr]
  | ProjBoth Expr
             Expr -- [expr1:expr2]
  deriving (Eq, Show)

data Expr
  = BinOpApply BinOp
               Expr
               Expr
  | Prim Primitive
  | Ref ScopedName
  | Apply Expr
          [Expr]
  | Proj Expr
         Projection
  deriving (Eq, Show)

intExpr :: Integral n => n -> Expr
intExpr = Prim . Integer . fromIntegral

stringExpr :: Text -> Expr
stringExpr = Prim . String

listExpr :: [Expr] -> Expr
listExpr = Prim . List

applyBuiltIn :: Text -> [Expr] -> Expr
applyBuiltIn n = Apply (Ref (builtIn n))

type Block = [Stmt]

data CondCase =
  CondCase Expr
           Block
  deriving (Eq, Show)

data CondStmt =
  CondStmt CondCase
           [CondCase]
           (Maybe Block)
  deriving (Eq, Show)

data AssignTarget
  = AssignName ScopedName
  | AssignProj AssignTarget
               Projection
  deriving (Eq, Show)

data Stmt
  = Let Name
        Expr
  | Return Expr
  | Function ScopedName
             [Name]
             Block
  | Call ScopedName
         [Expr]
  | Cond CondStmt
  | Assign AssignTarget
           Expr
  | BuiltInStmt Name
                Expr
  deriving (Eq, Show)

newtype Program =
  Program [Stmt]
  deriving (Eq, Show)
