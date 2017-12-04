{-# LANGUAGE LambdaCase #-}

module Vimscript.Optimise
  ( transforms
  ) where

import           Data.Generics.Uniplate.Data
import           Data.Monoid
import qualified Data.Text                   as T
import           Vimscript.AST
import qualified Vimscript.Optimise.DCE      as DCE
import qualified Vimscript.Optimise.TCO      as TCO

transforms :: Program -> Program
transforms = insertHeader . TCO.runPass . DCE.runPass . arityRename

arityRename :: Program -> Program
arityRename (Program ss) = Program ss'
  where
    ss' = map (transform goStmt) (transformBi goExpr ss)
    goStmt =
      \case
        Function sn ns bl -> Function (goSn (length ns) sn) ns bl
        Call sn es -> Call (goSn (length es) sn) es
        x -> x
    goExpr =
      \case
        Apply (Ref sn) es -> Apply (Ref (goSn (length es) sn)) es
        x -> x
    goSn count =
      \case
        ScopedName Script (Name n) ->
          ScopedName Script (Name (n <> T.pack "_" <> T.pack (show count)))
        x -> x

insertHeader :: Program -> Program
insertHeader (Program ss) = Program (c : ss)
  where
    c = LineComment (T.pack "Generated from Idris")

