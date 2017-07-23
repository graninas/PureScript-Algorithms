module Main where

import Data.Maybe
import Data.Exists (runExists)
import Data.NaturalTransformation
import Control.Monad.Eff (Eff)
import Control.Monad.Free
import Prelude
import Flow

interpret :: forall s. NaturalTransformation (FlowMethod s) (Eff ())
interpret (Action i next) = do
  pure next

interpret (EAction1 flow next) = do
  v <- run flow
  pure $ next $ Just v

interpret (EAction2 (Source source) next) = do
  pure $ next $ Just source

run :: NaturalTransformation Flow (Eff ())
run = foldFree (\(Wrapper x) -> runExists interpret x)

flow1 = pure "abc"

app :: Flow Unit
app = do
  action 10
  mbResult1 <- eAction1 flow1
  case mbResult1 of
       Just _ -> pure unit
       Nothing -> do
         mbResult2 <- eAction2 (source "abc")
         case mbResult2 of
              Just _ -> pure unit
              Nothing -> pure unit

main = run app