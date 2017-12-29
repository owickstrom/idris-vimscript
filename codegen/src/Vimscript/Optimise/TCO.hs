{-# LANGUAGE LambdaCase #-}

module Vimscript.Optimise.TCO
  ( runPass
  ) where

import           Data.Generics.Uniplate.Data
import           Vimscript.AST

runPass :: Program -> Program
runPass = tco

tco :: Program -> Program
tco (Program ss) = Program (foldMap tco' ss)

tco' :: Stmt -> Block
tco' x =
  case x of
    Function {} -> tcoFunc x
    _           -> pure x

isRecursiveFn :: Stmt -> Bool
isRecursiveFn
  -- we're only checking for recursiveness!
 =
  \case
    Function n _ bl
      | n `elem` universeBi bl -> True
    _ -> False

tcoFunc :: Stmt -> Block
tcoFunc fn@(Function n as bl) = [Function n as bl'']
  where
    bl' = tcoBlock n as bl
    loopedBl = [While (Prim (Integer 1)) (bl' ++ [Break])]
    localCopy = zipWith (\x y -> LocalLet x (Ref (ScopedName Argument y))) as as
    bl'' =
      if isRecursiveFn fn
        then localCopy ++ argumentToLocal loopedBl
        else bl'
tcoFunc _ = error "unreachable: tcoFunc"

argumentToLocal :: Block -> Block
argumentToLocal = transformBi go
  where
    go (ScopedName s n) =
      ScopedName
        (if s == Argument
           then Local
           else s)
        n

tcoBlock :: ScopedName -> [Name] -> Block -> Block
tcoBlock n as bl = bl'
  where
    bl' = concatMap (tcoStmt n as) bl

tcoStmt :: ScopedName -> [Name] -> Stmt -> Block
tcoStmt n as =
  \case
    Return (Apply (Ref sn) args)
      | sn == n -> adjust
      where adjust = zipWith LocalLet as args ++ [Continue]
    Cond c -> [Cond (tcoCond n as c)]
    s -> [s]

tcoCond :: ScopedName -> [Name] -> CondStmt -> CondStmt
tcoCond n as (CondStmt c cs bl) =
  CondStmt (tcoAlt n as c) (tcoAlt n as <$> cs) (tcoBlock n as <$> bl)

-- TODO check this: e can never be a tailcall, right?
tcoAlt :: ScopedName -> [Name] -> CondCase -> CondCase
tcoAlt n as (CondCase e bl) = CondCase e (tcoBlock n as bl)
