module Vimscript.Optimise
  ( performTransforms
  ) where

import           Vimscript.AST
import qualified Vimscript.Optimise.Annotate as Annotate
import qualified Vimscript.Optimise.DCE      as DCE
import qualified Vimscript.Optimise.Renamer  as Renamer
import qualified Vimscript.Optimise.TCO      as TCO

performTransforms :: Program -> Program
performTransforms =
  Annotate.runPass . TCO.runPass . DCE.runPass . Renamer.runPass
