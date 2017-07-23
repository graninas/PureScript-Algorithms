module Test.FlowTest where

import Prelude
--import Control.Monad.Free
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.NaturalTransformation (NaturalTransformation)
import Data.Map (member, empty, insert, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Eq
import Data.Int (fromString)
import Data.Array
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run) as T
import Test.Common

import Types.Exists
import Types.Free
import Types.XUnit

flowTest = do
  pure unit

runTests = do
  describe "Flow tests " do
    it "Flow test" flowTest
    pending "other tests..."
