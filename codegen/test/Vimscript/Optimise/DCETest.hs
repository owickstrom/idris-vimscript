{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Vimscript.Optimise.DCETest where

import           Test.Tasty.Hspec

import           Vimscript.AST
import qualified Vimscript.Optimise.DCE as DCE

sFoo = ScopedName Script "sFoo"

sBar = ScopedName Script "sBar"

sBaz = ScopedName Script "sBaz"

lFoo = ScopedName Local "lFoo"

lBar = ScopedName Local "lBar"

spec_dce = do
  it "eliminates unused top-level functions" $ do
    let neverCallsBaz =
          Program
            [ Function sFoo [] [Return (Ref sBar)]
            , Function sBar [] [Return (Ref sFoo)]
            , Function sBaz [] []
            ]
        withoutBaz =
          Program
            [ Function sFoo [] [Return (Ref sBar)]
            , Function sBar [] [Return (Ref sFoo)]
            ]
    DCE.runPass neverCallsBaz `shouldBe` withoutBaz
  it "keeps top-level functions referred in let bindings" $ do
    let neverCallsBaz =
          Program
            [ Function sFoo [] [Let lFoo (Ref sBar), Return (Ref lFoo)]
            , Function sBar [] [Let lBar (Ref sFoo), Return (Ref lBar)]
            , Function sBaz [] []
            ]
        withoutBaz =
          Program
            [ Function sFoo [] [Let lFoo (Ref sBar), Return (Ref lFoo)]
            , Function sBar [] [Let lBar (Ref sFoo), Return (Ref lBar)]
            ]
    DCE.runPass neverCallsBaz `shouldBe` withoutBaz

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
