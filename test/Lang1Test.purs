module Test.Lang1Test where

import Prelude
import Control.Monad.Free
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Common

import Types.XUnit
import Lang1


lang1Test = do
  let x = LangA1 IntF 10 xunit
  liftEff $ log $ encodeJSON x
  pure unit

runTests = do
  describe "Flow tests " do
    it "Lang1 test" lang1Test
    pending "other tests..."
