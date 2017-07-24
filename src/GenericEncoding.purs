module GenericEncoding where

import Data.Foreign (F, Foreign)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Data.Generic.Rep (class Generic)

defaultEncode ::  forall a b. Generic a b => GenericEncode b => a -> Foreign
defaultEncode x = genericEncode (defaultOptions {unwrapSingleConstructors=true}) x

defaultDecode :: forall a b. Generic a b => GenericDecode b => Foreign -> F a
defaultDecode x = genericDecode (defaultOptions {unwrapSingleConstructors=true}) x
