module Vimscript.Optimise
  ( performTransformsWithFlags
  , Flags(..)
  , defaultFlags
  ) where

import           Vimscript.AST
import qualified Vimscript.Optimise.Annotate as Annotate
import qualified Vimscript.Optimise.DCE      as DCE
import qualified Vimscript.Optimise.Renamer  as Renamer
import qualified Vimscript.Optimise.TCO      as TCO


data Flags = Flags 
  { dce :: Bool
  , tco :: Bool
  }
  deriving Show

defaultFlags :: Flags  
defaultFlags = Flags True True

performTransformsWithFlags :: Flags -> Program -> Program
performTransformsWithFlags flags = 
  Annotate.runPass . tcoF . dceF . Renamer.runPass where
    dceF = if dce flags then DCE.runPass else id
    tcoF = if tco flags then TCO.runPass else id


performTransforms :: Program -> Program
performTransforms =
  Annotate.runPass . TCO.runPass . DCE.runPass . Renamer.runPass
