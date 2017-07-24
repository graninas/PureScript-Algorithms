module Types.Free where

import Prelude

import Control.Monad.State as S
import Control.Monad.Free as F
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.NaturalTransformation (NaturalTransformation)
import Data.CatList (CatList, empty, snoc, uncons)
import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Data.Generic.Rep as G
import Data.Generic.Rep.Show as GShow
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Function.Uncurried (Fn2, runFn2)
import Unsafe.Coerce (unsafeCoerce)

import Types.XUnit

newtype Free f a = Free (F.Free f a)

foldFree :: forall f m. MonadRec m => (NaturalTransformation f m) -> (NaturalTransformation (Free f) m)
foldFree f (Free v) = F.foldFree f v

liftF :: forall f. NaturalTransformation f (Free f)
liftF f = Free (F.liftF f)

--------------------------------------------------------------------------------

derive instance genericFree :: G.Generic (Free f a) _
derive instance genericFreeU :: G.Generic (Free f Unit) _
derive instance genericFreeXU :: G.Generic (Free f XUnit) _

derive newtype instance eqFree' :: (Functor f, Eq1 f, Eq a) => Eq (Free f a)
derive newtype instance eq1Free' :: (Functor f, Eq1 f) => Eq1 (Free f)
derive newtype instance ordFree' :: (Functor f, Ord1 f, Ord a) => Ord (Free f a)
-- derive newtype instance ord1Free' :: (Functor f, Ord1 f, Ord a) => Ord1 (Free f)
instance ord1Free' :: (Functor f, Ord1 f, Ord a) => Ord1 (Free f) where
  compare1 = compare
derive newtype instance freeFunctor' :: Functor (Free f)
derive newtype instance freeBind' :: Bind (Free f)
derive newtype instance freeApplicative' :: Applicative (Free f)
derive newtype instance freeApply' :: Apply (Free f)
derive newtype instance freeMonad' :: Monad (Free f)
derive newtype instance freeMonadTrans' :: MonadTrans Free
derive newtype instance freeMonadRec' :: MonadRec (Free f)
derive newtype instance foldableFree' :: (Functor f, Foldable f) => Foldable (Free f)
derive newtype instance traversableFree' :: Traversable f => Traversable (Free f)
