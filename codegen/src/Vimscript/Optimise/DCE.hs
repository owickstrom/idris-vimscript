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
    usedRefs = foldMap referredScriptScope es

notDeadCode :: [Name] -> Stmt -> Bool
notDeadCode useds =
  \case
    Function (ScopedName Script n) _ _ -> n `elem` useds
    _ -> True

referredScriptScope :: Stmt -> [Name]
referredScriptScope =
  \case
    Let _ expr -> referredInExpr expr
    Return expr -> referredInExpr expr
    LineComment {} -> []
    While expr block -> referredInExpr expr ++ foldMap referredScriptScope block
    Break -> []
    Continue -> []
    Function _ _ block -> foldMap referredScriptScope block
    Call (ScopedName Script n) exprs -> n : foldMap referredInExpr exprs
    Call _ exprs -> foldMap referredInExpr exprs
    Cond (CondStmt ifCase elseIfCases maybeElse) ->
      referredCondCase ifCase ++
      foldMap referredCondCase elseIfCases ++
      maybe [] (foldMap referredScriptScope) maybeElse
    Assign _ expr -> referredInExpr expr
    BuiltInStmt _ expr -> referredInExpr expr
  where
    referredInExpr expr = [n | ScopedName Script n <- childrenBi expr]
    referredCondCase (CondCase expr block) =
      referredInExpr expr ++ foldMap referredScriptScope block
