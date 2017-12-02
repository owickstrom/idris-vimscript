{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Vimscript.Render where

import           Prelude                   hiding (Ordering (..))
import           Text.PrettyPrint.Mainland

import           Vimscript.AST
import           Vimscript.Optimise as Vim

indentWidth :: Int
indentWidth = 4

renderName :: Name -> Doc
renderName (Name n) = strictText n

renderScopedName :: ScopedName -> Doc
renderScopedName (ScopedName scope name) = prefix <> renderName name
  where
    prefix =
      case scope of
        BuiltIn  -> empty
        Global   -> "g:"
        Local    -> "l:"
        Script   -> "s:"
        Argument -> "a:"
        Register -> "@"
        Option -> "&"
        LocalOption -> "&l:"
        GlobalOption -> "&g:"
        Environment -> "$"

renderBinOp :: BinOp -> Doc
renderBinOp =
  \case
    Add -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
    Equals -> "=="
    LT -> "<"
    LTE -> "<="
    GT -> ">"
    GTE -> ">="
    Concat -> "."

commaSepIn :: Doc -> Doc -> [Doc] -> Doc
commaSepIn l r = enclose l r . folddoc (<>) . punctuate comma

renderPrim :: Primitive -> Doc
renderPrim =
  \case
    Integer i -> integer i
    Floating d -> double d
    String t -> string (show t) -- Will this hold?!
    List exprs -> commaSepIn lbracket rbracket (map renderExpr exprs)

renderProjection :: Projection -> Doc
renderProjection =
  \case
    ProjSingle expr -> brackets (renderExpr expr)
    ProjFrom expr -> brackets (renderExpr expr <> ":")
    ProjTo expr -> brackets (":" <> renderExpr expr)
    ProjBoth e1 e2 -> brackets (renderExpr e1 <> ":" <> renderExpr e2)

renderExpr :: Expr -> Doc
renderExpr =
  \case
    BinOpApply op lhs rhs ->
      renderExpr lhs <+> renderBinOp op <+> renderExpr rhs
    Prim prim -> renderPrim prim
    Ref scopedName -> renderScopedName scopedName
    Apply expr params ->
      renderExpr expr <> commaSepIn lparen rparen (map renderExpr params)
    Proj expr proj -> renderExpr expr <> renderProjection proj

renderAssignTarget :: AssignTarget -> Doc
renderAssignTarget =
  \case
    AssignName sn -> renderScopedName sn
    AssignProj at ap -> renderAssignTarget at <> renderProjection ap

renderStmt :: Stmt -> Doc
renderStmt =
  \case
    Let name expr ->
      "let" <+>
      renderScopedName (ScopedName Local name) <+> "=" <+> renderExpr expr
    Return expr -> "return" <+> renderExpr expr
    Function scopedName args block ->
      "function!" <+>
      renderScopedName scopedName <>
      commaSepIn lparen rparen (map renderName args) </>
      indent indentWidth (renderBlock block) </>
      "endfunction"
    Break -> "break"
    Continue -> "continue"
    While expr bl -> "while" <+> renderExpr expr </> indent indentWidth (renderBlock bl) </> "endwhile"
    Call name params ->
      "call" <+>
      renderScopedName name <> commaSepIn lparen rparen (map renderExpr params)
    Cond (CondStmt (CondCase ifExpr ifBlock) elseIfCases mElseCase) ->
      renderCase ("if" <+> renderExpr ifExpr) ifBlock </>
      stack (map renderElseIf elseIfCases) </>
      maybe empty (renderCase "else") mElseCase </>
      "endif"
      where renderCase h block = h </> indent indentWidth (renderBlock block)
            renderElseIf (CondCase expr block) =
              renderCase ("elseif" <+> renderExpr expr) block
    Assign tgt expr ->
      "let" <+> renderAssignTarget tgt <+> "=" <+> renderExpr expr
    BuiltInStmt name expr -> renderName name <+> parens (renderExpr expr)
    LineComment contents -> "\"" <+> strictText contents

renderBlock :: Block -> Doc
renderBlock = stack . map renderStmt

renderProgram :: Program -> Doc
renderProgram prog = stack (map renderStmt stmts)
  where Program stmts = Vim.transforms prog
