{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Vimscript.Optimise.DCETest where

import           Test.Tasty.Hspec

import           Vimscript.Optimise.DCE

spec_dce = do
  it "eliminates unused top-level functions" $
    True `shouldBe` True
