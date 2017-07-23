module Flow where

import Data.Exists
import Prelude
import Control.Monad.Free (Free, liftF)
import Data.Maybe (Maybe(..))

data Source s = Source s

source :: String -> Source String
source = Source

data FlowMethodF a s
  = Action Int a
  | EAction1 (Flow s) (Maybe s -> a)
  | EAction2 (Source s) (Maybe s -> a)

type FlowMethod s a = FlowMethodF a s
data FlowMethodE a = Wrapper (Exists (FlowMethodF a))
type Flow a = Free FlowMethodE a

wrap :: forall a s. FlowMethodF a s -> Flow a
wrap = liftF <<< Wrapper <<< mkExists

-- monadic language

action :: Int -> Flow Unit
action i = wrap $ Action i unit

eAction1 :: forall s. Flow s -> Flow (Maybe s)
eAction1 flow = wrap $ EAction1 flow id

eAction2 :: forall s. Source s -> Flow (Maybe s)
eAction2 source = wrap $ EAction2 source id

