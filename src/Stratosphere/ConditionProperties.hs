module Stratosphere.ConditionProperties
  ( ConditionProperties (..),
    ToConditionRef (..),
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable
import GHC.Exts (IsList (..))
import Prelude

-- | This type is a wrapper around any values in a template. A value can be a
-- 'Literal', a 'Ref', or an intrinsic function. See:
-- http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference.html
data ConditionProperties a where
  And :: ConditionProperties Bool -> ConditionProperties Bool -> ConditionProperties Bool
  ConditionRef :: Text -> ConditionProperties a
  Equals :: (Show a, JSON.ToJSON a, Eq a, Typeable a) => ConditionProperties a -> ConditionProperties a -> ConditionProperties Bool
  If :: Text -> ConditionProperties a -> ConditionProperties a -> ConditionProperties a
  Literal :: a -> ConditionProperties a
  Not :: ConditionProperties Bool -> ConditionProperties Bool
  Or :: ConditionProperties Bool -> ConditionProperties Bool -> ConditionProperties Bool

deriving instance (Show a) => Show (ConditionProperties a)

instance (Eq a) => Eq (ConditionProperties a) where
  And a b == And a' b' = a == a' && b == b'
  ConditionRef a == ConditionRef a' = a == a'
  Equals a b == Equals a' b' = eqEquals a b a' b'
  If a b c == If a' b' c' = a == a' && b == b' && c == c'
  Literal a == Literal a' = a == a'
  Not a == Not a' = a == a'
  Or a b == Or a' b' = a == a' && b == b'
  _ == _ = False

eqEquals :: (Typeable a, Typeable b, Eq a) => a -> a -> b -> b -> Bool
eqEquals a b a' b' = fromMaybe False $ do
  a'' <- cast a'
  b'' <- cast b'
  pure $ a == a'' && b == b''

instance (JSON.ToJSON a) => JSON.ToJSON (ConditionProperties a) where
  toJSON = \case
    (And x y) -> mkFunc "Fn::And" [JSON.toJSON x, JSON.toJSON y]
    (ConditionRef r) -> refToJSON r
    (Equals x y) -> mkFunc "Fn::Equals" [JSON.toJSON x, JSON.toJSON y]
    (If i x y) -> mkFunc "Fn::If" [JSON.toJSON i, JSON.toJSON x, JSON.toJSON y]
    (Literal v) -> JSON.toJSON v
    (Not x) -> mkFunc "Fn::Not" [JSON.toJSON x]
    (Or x y) -> mkFunc "Fn::Or" [JSON.toJSON x, JSON.toJSON y]

mkFunc :: JSON.Key -> [JSON.Value] -> JSON.Value
mkFunc key args = JSON.object [(key, JSON.Array $ fromList args)]

refToJSON :: Text -> JSON.Value
refToJSON ref = JSON.object ["Condition" .= ref]

-- | Class used to create a 'Ref' from another type.
class ToConditionRef a b where
  toConditionRef :: a -> ConditionProperties b
