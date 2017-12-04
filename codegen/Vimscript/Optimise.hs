{-# LANGUAGE LambdaCase #-}

module Vimscript.Optimise
  ( transforms
  ) where

import           Data.Generics.Uniplate.Data
import           Data.Monoid
import qualified Data.Text                   as T
import           Vimscript.AST
import qualified Vimscript.Optimise.DCE      as DCE

transforms :: Program -> Program
transforms = insertHeader . tco . DCE.runPass . arityRename

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
