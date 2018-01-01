module Vimscript.Optimise
  ( performTransformsWithFlags
  , Flags(..)
  , defaultFlags
  ) where

import           Vimscript.AST
import qualified Vimscript.Optimise.Annotate as Annotate
import qualified Vimscript.Optimise.DCE      as DCE
import qualified Vimscript.Optimise.Renamer  as Renamer

newtype Flags = Flags
  { dce :: Bool
  } deriving (Show)

defaultFlags :: Flags
defaultFlags = Flags True

performTransformsWithFlags :: Flags -> Program -> Program
performTransformsWithFlags flags = Annotate.runPass . dceF . Renamer.runPass
  where
    dceF =
      if dce flags
        then DCE.runPass
        else id
