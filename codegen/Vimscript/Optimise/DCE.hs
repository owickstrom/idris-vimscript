{-# LANGUAGE LambdaCase #-}

module Vimscript.Optimise.DCE
  ( runPass
  ) where

import           Data.Generics.Uniplate.Data
import           Vimscript.AST

runPass :: Program -> Program
runPass = dce

-- | Dead-code elimination toplevel.
dce :: Program -> Program
dce (Program es) = Program (filter (notDeadCode usedRefs) es)
  where
    usedRefs = foldMap refsName (childrenBi es)

notDeadCode :: [Name] -> Stmt -> Bool
notDeadCode useds =
  \case
    Function (ScopedName Script n) _ _ -> n `elem` useds
    _ -> True

refsName :: ScopedName -> [Name]
refsName =
  \case
    ScopedName Script n -> [n]
    _ -> []
