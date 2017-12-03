{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, StandaloneDeriving              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Vimscript.AST where

import Data.Monoid
import           Data.Hashable
import           Data.String
import Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import qualified Data.Map as M
import Data.Map (Map)
import Debug.Trace
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data

data NameScope
  = BuiltIn
  | Global
  | Local
  | Script
  | Argument
  | Register
  | Option
  | LocalOption
  | GlobalOption
  | Environment
  deriving (Eq, Show, Generic, Data, Typeable)

instance Hashable NameScope

newtype Name =
  Name Text
  deriving (Eq, Show, IsString, Generic, Ord, Data, Typeable)

instance Hashable Name

data ScopedName =
  ScopedName NameScope
             Name
  deriving (Eq, Show, Generic, Data, Typeable)

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
  deriving (Eq, Show, Data, Typeable)

data Primitive
  = Integer Integer
  | Floating Double
  | String Text
  | List [Expr]
  deriving (Eq, Show, Data, Typeable)

data Projection
  = ProjSingle Expr -- [expr]
  | ProjFrom Expr -- [expr:]
  | ProjTo Expr -- [:expr]
  | ProjBoth Expr
             Expr -- [expr1:expr2]
  deriving (Eq, Show, Data, Typeable)

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
  deriving (Eq, Show, Data, Typeable)

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
  deriving (Eq, Show, Data, Typeable)

data CondStmt =
  CondStmt CondCase
           [CondCase]
           (Maybe Block)
  deriving (Eq, Show, Data, Typeable)

data AssignTarget
  = AssignName ScopedName
  | AssignProj AssignTarget
               Projection
  deriving (Eq, Show, Data, Typeable)

data Stmt
  = Let ScopedName
        Expr
  | Return Expr
  | LineComment Text
  | While Expr Block
  | Break
  | Continue
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
  deriving (Eq, Show, Data, Typeable)

pattern LocalLet :: Name -> Expr -> Stmt
pattern LocalLet n e = Let (ScopedName Local n) e

newtype Program =
  Program [Stmt]
  deriving (Eq, Show, Data, Typeable)

