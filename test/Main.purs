module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run) as T
import Test.FlowTest as FlowTest
import Test.Lang1Test as Lang1Test

main :: Eff (T.RunnerEffects (console :: CONSOLE, exception :: EXCEPTION)) Unit
main = T.run [consoleReporter] do
  FlowTest.runTests
  Lang1Test.runTests
