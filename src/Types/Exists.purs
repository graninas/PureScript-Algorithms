module Types.Exists where

import Data.Exists as E
import Data.Generic.Rep as G
import Data.Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Unsafe.Coerce (unsafeCoerce)

newtype Exists a = Exists (E.Exists a)

runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r
runExists f (Exists e) = E.runExists f e

mkExists :: forall f a. f a -> Exists f
mkExists v = Exists (E.mkExists v)

derive instance genericExists :: G.Generic (Exists (a b)) _


ee' :: forall y a b. Exists (y a) -> y a b
ee' = unsafeCoerce

showExists :: forall y a b. (y a b -> String) -> Exists (y a) -> String
showExists showE e =
  let v = ee' e
  in showE v

ee'' :: forall y a b. E.Exists (y a) -> y a b
ee'' = unsafeCoerce
