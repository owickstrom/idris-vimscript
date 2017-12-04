module Vimscript.Optimise.Annotate
  ( runPass
  ) where

import qualified Data.Text     as T
import           Vimscript.AST

runPass :: Program -> Program
runPass = insertHeader

insertHeader :: Program -> Program
insertHeader (Program ss) = Program (c : ss)
  where
    c = LineComment (T.pack "Generated from Idris")
