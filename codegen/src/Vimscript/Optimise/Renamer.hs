{-# LANGUAGE LambdaCase #-}

module Vimscript.Optimise.Renamer
  ( runPass
  ) where

import           Data.Generics.Uniplate.Data
import           Data.Monoid
import qualified Data.Text                   as T
import           Vimscript.AST

runPass :: Program -> Program
runPass = arityRename

arityRename :: Program -> Program
arityRename (Program ss) = Program ss'
  where
    ss' = map (transform goStmt) (transformBi goExpr ss)
    goStmt =
      \case
        Function sn ns bl -> Function (goSnLen ns sn) ns bl
        Call sn es -> Call (goSn (length es) sn) es
        x -> x
    goExpr =
      \case
        Apply (Ref sn) es -> Apply (Ref (goSnLen es sn)) es
        x -> x
    goSnLen = goSn . length
    goSn count =
      \case
        ScopedName Script n -> ScopedName Script (arityRenameBinding n count)
        x -> x
    arityRenameBinding (Name n) arity =
      Name (n <> T.pack "_" <> T.pack (show arity))
