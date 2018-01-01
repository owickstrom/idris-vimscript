{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module IRTS.CodegenVim where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Char
import           Data.Generics.Uniplate.Data
import           Data.HashMap.Strict                (HashMap)
import qualified Data.HashMap.Strict                as HM
import           Data.HashSet                       (HashSet)
import qualified Data.HashSet                       as HashSet
import           Data.Semigroup
import qualified Data.Text                          as T
import           Idris.Core.TT                      as Idris
import           IRTS.CodegenCommon
import           IRTS.CodegenVim.Internal.ZEncoding
import           IRTS.Lang
import           IRTS.Simplified
import           Text.PrettyPrint.Mainland          (pretty)
import qualified Vimscript.AST                      as Vim
import qualified Vimscript.Optimise                 as Optimise
import qualified Vimscript.Render                   as Vim

codegenVim :: Optimise.Flags -> CodeGenerator
codegenVim fs ci = do
  let decls = simpleDecls ci
  let prg =
        Optimise.performTransformsWithFlags fs $
        fst
          (runCodegen
             mempty
             GenState {currentFunction = (), tailCalls = mempty}
             (genProgram decls))
  writeFile (outputFile ci) (pretty 200 (Vim.renderProgram prg))

data GenState fn = GenState
  { tailCalls       :: HashSet Vim.ScopedName
  , currentFunction :: fn
  } deriving (Show, Eq)

type NameMappings = HashMap Vim.Name Vim.ScopedName

type Gen fn a = StateT (GenState fn) (Reader NameMappings) a

type ProgramGen a = StateT (GenState ()) (Reader NameMappings) a

type FunctionGen a
   = StateT (GenState (Vim.ScopedName, [Vim.Name])) (Reader NameMappings) a

runCodegen :: NameMappings -> GenState fn -> Gen fn a -> (a, GenState fn)
runCodegen names st gen = runReader (runStateT gen st) names

vimName :: Idris.Name -> Vim.Name
vimName n = Vim.Name (T.pack ("Idris_" <> foldMap vimChar (showCG n)))
  where
    vimChar x
      | isAlpha x || isDigit x = [x]
      | otherwise = zEncode x

lookupName :: Vim.Name -> Gen fn Vim.ScopedName
lookupName n = do
  names <- ask
  case HM.lookup n names of
    Just sn -> pure sn
    Nothing -> pure (Vim.ScopedName Vim.Global n)

asNameMapping :: Vim.ScopedName -> HashMap Vim.Name Vim.ScopedName
asNameMapping sn@(Vim.ScopedName _ n) = HM.singleton n sn

asNameMappings :: [Vim.ScopedName] -> HashMap Vim.Name Vim.ScopedName
asNameMappings = foldMap asNameMapping

withNewName :: Vim.ScopedName -> Gen fn a -> Gen fn a
withNewName = local . HM.union . asNameMapping

withNewNames :: [Vim.ScopedName] -> Gen fn a -> Gen fn a
withNewNames = local . HM.union . asNameMappings

loc :: Int -> Vim.Name
loc i
  | i <= 26 = Vim.Name (T.singleton (toEnum (i + fromEnum 'a') :: Char))
  | otherwise = Vim.Name ("loc" <> T.pack (show i))

topLevelName :: Name -> Vim.ScopedName
topLevelName n = Vim.ScopedName Vim.Script (vimName n)

genProgram :: [(Name, SDecl)] -> ProgramGen Vim.Program
genProgram defs = do
  let topLevelNames = map (topLevelName . fst) defs
  fns <- withNewNames topLevelNames (mapM genTopLevel defs)
  let start =
        Vim.Call (Vim.ScopedName Vim.Script (vimName (sMN 0 "runMain"))) []
  let stmts = fns ++ [start]
  pure (Vim.Program stmts)

genTopLevel :: (Name, SDecl) -> ProgramGen Vim.Stmt
genTopLevel (n, SFun _ args _i def) = genFunc n args def

genFunc :: Name -> [Name] -> SExp -> ProgramGen Vim.Stmt
genFunc n args def = withNewName fName stmt
  where
    fName = topLevelName n
    args' = map loc [0 .. (length args - 1)]
    localArgs =
      map
        (\an -> Vim.LocalLet an (Vim.Ref (Vim.ScopedName Vim.Argument an)))
        args'
    stmt = do
      nameMappings <- ask
      let argsScoped = map (Vim.ScopedName Vim.Argument) args'
          (stmts, genState) =
            runCodegen
              (nameMappings `HM.union` asNameMappings argsScoped)
              GenState {currentFunction = (fName, args'), tailCalls = mempty}
              (genStmts (pure . Vim.Return) def)
          isTailRec = fName `HashSet.member` tailCalls genState
      if isTailRec
        then pure (tcoFunc stmts)
        else pure (Vim.Function fName args' stmts)
    tcoFunc stmts =
      let looped = [Vim.While (Vim.intExpr (1 :: Int)) (stmts ++ [Vim.Break])]
      in Vim.Function fName args' (localArgs ++ argumentToLocal looped)

argumentToLocal :: Vim.Block -> Vim.Block
argumentToLocal = transformBi go
  where
    go (Vim.ScopedName s n) =
      Vim.ScopedName
        (if s == Vim.Argument
           then Vim.Local
           else s)
        n

genVar :: LVar -> FunctionGen Vim.ScopedName
genVar =
  \case
    Loc n -> lookupName (loc n)
    Glob n -> lookupName (vimName n)

addTailCall :: Vim.ScopedName -> FunctionGen ()
addTailCall f = modify (\s -> s {tailCalls = HashSet.insert f (tailCalls s)})

genStmts :: (Vim.Expr -> FunctionGen Vim.Stmt) -> SExp -> FunctionGen Vim.Block
genStmts ret =
  \case
    SV (Glob n) -> do
      vn <- lookupName (vimName n)
      stmt <- ret (Vim.Apply (Vim.Ref vn) [])
      pure [stmt]
    SV (Loc n) -> do
      ln <- lookupName (loc n)
      stmt <- ret (Vim.Ref ln)
      pure [stmt]
    SApp tailCall f params -> do
      f' <- lookupName (vimName f)
      params' <- map Vim.Ref <$> mapM genVar params
      (fnName, localArgs) <- gets currentFunction
      if tailCall && f' == fnName
        then do
          addTailCall f'
          pure (zipWith Vim.LocalLet localArgs params' ++ [Vim.Continue])
        else do
          stmt <- ret (Vim.Apply (Vim.Ref f') params')
          pure [stmt]
    SOp op args -> do
      args' <- mapM (fmap Vim.Ref . genVar) args
      stmt <- genPrimFn op args' >>= ret
      pure [stmt]
    SLet (Loc i) v sc -> do
      let n = loc i
          sn = Vim.ScopedName Vim.Local n
      let' <- genStmts (pure . Vim.LocalLet n) v
      rest <- withNewName sn (genStmts (withNewName sn . ret) sc)
      pure (let' ++ rest)
    SUpdate _ e -> genStmts ret e
    SProj e i -> do
      v <- genVar e
      proj <- ret (Vim.Proj (Vim.Ref v) (Vim.ProjSingle (Vim.intExpr i)))
      pure [proj]
    SCon _ t _ args -> do
      let con = Vim.Prim (Vim.Integer (fromIntegral t))
      args' <- map Vim.Ref <$> mapM genVar args
      stmt <- ret (Vim.Prim (Vim.List (con : args')))
      pure [stmt]
    SCase _ e alts -> do
      e' <- Vim.Ref <$> genVar e
      genCases ret e' alts
    SChkCase e alts -> do
      e' <- Vim.Ref <$> genVar e
      genCases ret e' alts
    SConst c -> do
      stmt <- genConst c >>= ret
      pure [stmt]
    SForeign _ f params -> do
      params' <- mapM (fmap Vim.Ref . genVar . snd) params
      genForeign ret f params'
    SNothing -> do
      stmt <- ret (Vim.intExpr (0 :: Int))
      pure [stmt]
    SError x -> pure [Vim.BuiltInStmt "throw" (Vim.stringExpr (T.pack x))]
    expr -> error ("Expression not supported: " <> show expr)

projCons :: Vim.Expr -> Vim.Expr
projCons expr = Vim.Proj expr (Vim.ProjSingle (Vim.intExpr (0 :: Int)))

genCases ::
     (Vim.Expr -> FunctionGen Vim.Stmt)
  -> Vim.Expr
  -> [SAlt]
  -> FunctionGen Vim.Block
genCases ret c alts =
  foldM go ([], []) alts >>= \case
    ([], block) -> pure block
    (ifCase:elseIfCases, defaultBlock) ->
      let defaults =
            if null defaultBlock
              then Nothing
              else Just defaultBlock
      in pure [Vim.Cond (Vim.CondStmt ifCase elseIfCases defaults)]
  where
    go (cases, defaultCase) =
      \case
        SConstCase t exp' -> do
          test' <- Vim.BinOpApply Vim.Equals c <$> genConst t
          block <- genStmts ret exp'
          pure (cases ++ [Vim.CondCase test' block], defaultCase)
        SConCase lv t _ args exp' -> do
          let t' = Vim.intExpr (t :: Int)
              test' = Vim.BinOpApply Vim.Equals (projCons c) t'
              letPairs = zip [1 .. length args] [lv ..]
              newLocalNames =
                map (Vim.ScopedName Vim.Local . loc . snd) letPairs
          lets <- mapM letProject letPairs
          block <- withNewNames newLocalNames (genStmts ret exp')
          pure (cases ++ [Vim.CondCase test' (lets ++ block)], defaultCase)
          where letProject :: (Int, Int) -> FunctionGen Vim.Stmt
                letProject (i, v) = do
                  let expr = Vim.Proj c (Vim.ProjSingle (Vim.intExpr i))
                  pure (Vim.LocalLet (loc v) expr)
        SDefaultCase exp' -> do
          block <- genStmts ret exp'
          pure (cases, defaultCase ++ block)

-- | Translate constants to Vim expressions.
genConst :: Const -> FunctionGen Vim.Expr
genConst =
  \case
    (I i) -> pure (Vim.Prim (Vim.Integer (fromIntegral i)))
    (Ch i) -> pure (Vim.Prim (Vim.String (T.singleton i)))
    (BI i) -> pure (Vim.Prim (Vim.Integer i)) -- No support for Big Integer.
    (Str s) -> pure (Vim.Prim (Vim.String (T.pack s)))
    TheWorld -> pure (Vim.Prim (Vim.Integer 0))
    x
      | isTypeConst x -> pure (Vim.Prim (Vim.Integer 0))
    x -> error $ "Constant " ++ show x ++ " not compilable yet"

asBinOp :: PrimFn -> Maybe Vim.BinOp
asBinOp =
  \case
    LStrConcat -> Just Vim.Concat
    LStrCons -> Just Vim.Concat
    (LPlus (ATInt _)) -> Just Vim.Add
    (LMinus (ATInt _)) -> Just Vim.Subtract
    (LTimes (ATInt _)) -> Just Vim.Multiply
    (LEq (ATInt _)) -> Just Vim.Equals
    (LSLt (ATInt _)) -> Just Vim.LT
    (LSLe (ATInt _)) -> Just Vim.LTE
    (LSGt (ATInt _)) -> Just Vim.GT
    (LSGe (ATInt _)) -> Just Vim.GTE
    LStrEq -> Just Vim.Equals
    _ -> Nothing

-- | Translate primops from the Idris FFI to Vim commands.
genForeign ::
     (Vim.Expr -> FunctionGen Vim.Stmt)
  -> FDesc
  -> [Vim.Expr]
  -> FunctionGen Vim.Block
genForeign ret (FCon name) params =
  case (showCG name, params) of
    ("VIM_Echo", [x]) -> pure [Vim.BuiltInStmt "echo" x]
    ("VIM_ListEmpty", []) -> do
      stmt <- ret (Vim.listExpr [])
      pure [stmt]
    ("VIM_ListIndex", [i, l]) -> do
      stmt <- ret (Vim.Proj l (Vim.ProjSingle i))
      pure [stmt]
    ("VIM_ListSetAt", [i, x, l]) ->
      case l of
        Vim.Ref l' ->
          pure
            [ Vim.Assign
                (Vim.AssignProj (Vim.AssignName l') (Vim.ProjSingle i))
                x
            ]
        _ -> error "Cannot Idris_list_setAt without a list reference!"
    ("VIM_ListCons", [x, l]) -> do
      stmt <- ret (Vim.BinOpApply Vim.Add (Vim.listExpr [x]) l)
      pure [stmt]
    ("VIM_ListSnoc", [l, x]) -> do
      stmt <- ret (Vim.BinOpApply Vim.Add l (Vim.listExpr [x]))
      pure [stmt]
    ("VIM_ListConcat", [l1, l2]) -> do
      stmt <- ret (Vim.BinOpApply Vim.Add l1 l2)
      pure [stmt]
    (other, p) ->
      error ("Foreign function not supported: " ++ other ++ " " ++ show p)
genForeign ret (FApp (showCG -> "VIM_BuiltIn") [FStr name]) params = do
  stmt <- ret (Vim.Apply (Vim.Ref (Vim.builtIn (T.pack name))) params)
  pure [stmt]
genForeign ret (FApp (showCG -> "VIM_Get") fs) params =
  case fs of
    [FCon (showCG -> con), FStr name] -> do
      stmt <-
        ret (Vim.Ref (Vim.ScopedName (fromFFICon con) (Vim.Name (T.pack name))))
      pure [stmt]
    _ ->
      error
        ("VIM_Get: " ++
         show fs ++
         " " ++ show params ++ " not sufficiently reduced! Use a %inline.")
genForeign _ (FApp (showCG -> "VIM_Set") fs) params =
  case (fs, params) of
    ([FCon (showCG -> con), FStr name], [rhs]) -> do
      stmt <-
        pure
          (Vim.Let
             (Vim.ScopedName (fromFFICon con) (Vim.Name (T.pack name)))
             rhs)
      pure [stmt]
    _
      | length params > 1 -> error "Too many RHS terms!"
    _ ->
      error
        (show fs ++
         " " ++ show params ++ " not sufficiently reduced! Use a %inline.")
genForeign _ f _ = error ("Foreign function not supported: " ++ show f)

fromFFICon :: String -> Vim.NameScope
fromFFICon =
  \case
    "VIM_Option" -> Vim.Option
    "VIM_LocalOption" -> Vim.LocalOption
    "VIM_GlobalOption" -> Vim.GlobalOption
    "VIM_Argument" -> Vim.Argument
    "VIM_Register" -> Vim.Register
    x -> error ("Ilegal FFI mutable variable type " ++ show x)

-- | Implement a @PrimFn@ in terms of Vim primitives.
genPrimFn :: PrimFn -> [Vim.Expr] -> FunctionGen Vim.Expr
genPrimFn LWriteStr [_, s] = pure (Vim.applyBuiltIn "Idris_echo" [s])
genPrimFn LStrIndex [x, y] = pure (Vim.Proj x (Vim.ProjSingle y))
genPrimFn LStrSubstr [i, l, s] =
  pure (Vim.Proj s (Vim.ProjBoth i (Vim.BinOpApply Vim.Subtract l i)))
genPrimFn (asBinOp -> Just binOp) [l, r] = pure (Vim.BinOpApply binOp l r)
genPrimFn (LExternal n) params =
  pure (Vim.applyBuiltIn (T.pack (showCG n)) params)
genPrimFn LReadStr _ =
  error "Cannot read strings using the idris-vimscript-backend!"
genPrimFn unaryPrimFn [x] =
  pure $
  case unaryPrimFn of
    LStrRev ->
      Vim.applyBuiltIn
        "join"
        [ Vim.applyBuiltIn
            "reverse"
            [Vim.applyBuiltIn "split" [x, Vim.stringExpr ".\\zs"]]
        , Vim.stringExpr ""
        ]
    LStrLen -> Vim.applyBuiltIn "len" [x]
    LStrHead -> Vim.Proj x (Vim.ProjSingle (Vim.intExpr (0 :: Int)))
    LStrTail -> Vim.Proj x (Vim.ProjFrom (Vim.intExpr (0 :: Int)))
    LIntStr {} -> Vim.BinOpApply Vim.Concat x (Vim.stringExpr "")
    LStrInt {} -> Vim.applyBuiltIn "str2nr" [x]
    LChInt {} -> Vim.applyBuiltIn "char2nr" [x]
    LIntCh {} -> Vim.applyBuiltIn "nr2char" [x]
    LSExt {} -> x
    LTrunc {} -> x
    LZExt {} -> x
    _ ->
      error $
      "Unary primitive function " ++
      show unaryPrimFn ++ " " ++ show [x] ++ " not implemented!"
genPrimFn f exps =
  error $ "PrimFn " ++ show f ++ " " ++ show exps ++ " not implemented!"
