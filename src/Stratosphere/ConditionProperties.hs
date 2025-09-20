module Stratosphere.ConditionProperties
  ( ConditionProperties (..),
    ToCRef (..),
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
  CAnd :: ConditionProperties Bool -> ConditionProperties Bool -> ConditionProperties Bool
  CEquals :: (Show a, JSON.ToJSON a, Eq a, Typeable a) => ConditionProperties a -> ConditionProperties a -> ConditionProperties Bool
  CIf :: Text -> ConditionProperties a -> ConditionProperties a -> ConditionProperties a
  CLiteral :: a -> ConditionProperties a
  CNot :: ConditionProperties Bool -> ConditionProperties Bool
  COr :: ConditionProperties Bool -> ConditionProperties Bool -> ConditionProperties Bool
  CRef :: Text -> ConditionProperties a

deriving instance (Show a) => Show (ConditionProperties a)

instance (Eq a) => Eq (ConditionProperties a) where
  CAnd a b == CAnd a' b' = a == a' && b == b'
  CEquals a b == CEquals a' b' = eqEquals a b a' b'
  CIf a b c == CIf a' b' c' = a == a' && b == b' && c == c'
  CLiteral a == CLiteral a' = a == a'
  CNot a == CNot a' = a == a'
  COr a b == COr a' b' = a == a' && b == b'
  CRef a == CRef a' = a == a'
  _ == _ = False

eqEquals :: (Typeable a, Typeable b, Eq a) => a -> a -> b -> b -> Bool
eqEquals a b a' b' = fromMaybe False $ do
  a'' <- cast a'
  b'' <- cast b'
  pure $ a == a'' && b == b''

instance (JSON.ToJSON a) => JSON.ToJSON (ConditionProperties a) where
  toJSON = \case
    (CAnd x y) -> mkFunc "Fn::And" [JSON.toJSON x, JSON.toJSON y]
    (CEquals x y) -> mkFunc "Fn::Equals" [JSON.toJSON x, JSON.toJSON y]
    (CIf i x y) -> mkFunc "Fn::If" [JSON.toJSON i, JSON.toJSON x, JSON.toJSON y]
    (CLiteral v) -> JSON.toJSON v
    (CNot x) -> mkFunc "Fn::Not" [JSON.toJSON x]
    (COr x y) -> mkFunc "Fn::Or" [JSON.toJSON x, JSON.toJSON y]
    (CRef r) -> refToJSON r

mkFunc :: JSON.Key -> [JSON.Value] -> JSON.Value
mkFunc key args = JSON.object [(key, JSON.Array $ fromList args)]

refToJSON :: Text -> JSON.Value
refToJSON ref = JSON.object ["Condition" .= ref]

-- | Class used to create a 'Ref' from another type.
class ToCRef a b where
  toCRef :: a -> ConditionProperties b
