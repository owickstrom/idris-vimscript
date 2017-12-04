{-# LANGUAGE LambdaCase #-}

module Vimscript.Optimise
  ( transforms
  ) where

import           Data.Generics.Uniplate.Data
import           Data.Monoid
import qualified Data.Text                   as T
import           Vimscript.AST

transforms :: Program -> Program
transforms = insertHeader . tco . dce . arityRename

dce :: Program -> Program
dce (Program es) = Program (filter (notDeadCode usedRefs) es)
  where
    usedRefs = concatMap refs es

notDeadCode :: [Name] -> Stmt -> Bool
notDeadCode useds =
  \case
    Function (ScopedName Script n) _ _ -> n `elem` useds
    _ -> True

-- TODO use something better than [Name]
refs :: Stmt -> [Name]
refs =
  \case
    Let _ e -> refsExpr e
    Function _ _ ss -> concatMap refs ss
    Return e -> refsExpr e
    Call s es -> refsName s ++ concatMap refsExpr es
    Assign _ e -> refsExpr e
    BuiltInStmt _ e -> refsExpr e
    While e bl -> refsExpr e ++ concatMap refs bl
    Cond cond -> refsCond cond
    Break -> []
    Continue -> []
    LineComment {} -> []

refsCond :: CondStmt -> [Name]
refsCond (CondStmt e alts bl) =
  refsCondCase e ++ concatMap refsCondCase alts ++ maybe [] (concatMap refs) bl

refsCondCase :: CondCase -> [Name]
refsCondCase (CondCase e bl) = refsExpr e ++ concatMap refs bl

refsExpr :: Expr -> [Name]
refsExpr =
  \case
    BinOpApply _ e f -> refsExpr e ++ refsExpr f
    Ref s -> refsName s
    Apply e es -> refsExpr e ++ concatMap refsExpr es
    Proj e pr -> refsExpr e ++ refsProj pr
    Prim p -> refsPrim p

refsPrim :: Primitive -> [Name]
refsPrim =
  \case
    List es -> concatMap refsExpr es
    _ -> []

refsProj :: Projection -> [Name]
refsProj =
  \case
    ProjSingle e -> refsExpr e
    ProjFrom e -> refsExpr e
    ProjTo e -> refsExpr e
    ProjBoth e f -> refsExpr e ++ refsExpr f

refsName :: ScopedName -> [Name]
refsName =
  \case
    ScopedName Script n -> [n]
    _ -> []

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
