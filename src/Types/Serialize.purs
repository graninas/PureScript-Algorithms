module Types.Serialize where

import Prelude
import Data.Generic.Rep as G
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show as GShow
import Data.Generic.Rep.Bounded as GBounded
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)

data S a = Serialized a { json :: String, str :: String }

class (Encode a, Show a) <= Serialize a where
  serialize :: a -> S a

defaultSerialize :: forall a. Encode a => Show a => a -> S a
defaultSerialize a = Serialized a { json: encodeJSON a, str: show a }

getValue :: forall s. S s -> s
getValue (Serialized v _) = v

getJSON :: forall s. S s -> String
getJSON (Serialized _ s) = s.json

getString :: forall s. S s -> String
getString (Serialized _ s) = s.str
