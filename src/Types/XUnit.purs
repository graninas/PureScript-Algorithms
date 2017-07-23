module Types.XUnit where

import Prelude as P

import Data.Generic.Rep as G
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Data.Generic.Rep.Bounded as GBounded
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign (toForeign)
import Data.Show (class Show)

newtype XUnit = XUnit P.Unit

derive newtype instance xUnitShow :: Show XUnit

instance encodeXUnit :: Encode XUnit where
  encode _ = toForeign "()"

xunit :: XUnit
xunit = XUnit P.unit
