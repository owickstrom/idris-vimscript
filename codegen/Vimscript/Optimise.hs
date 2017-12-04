module Vimscript.Optimise
  ( transforms
  ) where

import           Data.Generics.Uniplate.Data
import           Data.Monoid
import qualified Data.Text                   as T
import           Vimscript.AST
import qualified Vimscript.Optimise.DCE      as DCE
import qualified Vimscript.Optimise.TCO      as TCO
import qualified Vimscript.Optimise.Renamer  as Renamer

transforms :: Program -> Program
transforms = insertHeader . TCO.runPass . DCE.runPass . Renamer.runPass

insertHeader :: Program -> Program
insertHeader (Program ss) = Program (c : ss)
  where
    c = LineComment (T.pack "Generated from Idris")
