{-# LANGUAGE OverloadedLabels #-}

module Stratosphere.Condition
  ( Condition (..),
    Conditions (..),
    ConditionProperties (..),
    ToCRef (..),
    conditionToJSON,
  )
where

import Control.Lens ((^.))
import qualified Data.Aeson as JSON
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.Typeable
import GHC.Exts (IsList (..))
import Stratosphere.NamedItem
import Stratosphere.Prelude
import Stratosphere.Property

data Condition a = Condition
  { logicalName :: Text,
    value :: ConditionProperties a
  }
  deriving (Show, Eq, Generic)

instance Property "LogicalName" (Condition a) where
  type PropertyType "LogicalName" (Condition a) = Text
  set newValue Condition {..} = Condition {logicalName = newValue, ..}

instance Property "Value" (Condition a) where
  type PropertyType "Value" (Condition a) = ConditionProperties a
  set newValue Condition {..} = Condition {value = newValue, ..}

instance ToCRef (Condition a) b where
  toCRef = CRef . (.logicalName)

conditionToJSON :: (JSON.ToJSON a) => Condition a -> JSON.Value
conditionToJSON Condition {..} = JSON.toJSON value

-- | Wrapper around a list of 'Conditions's to we can modify the aeson instances.
newtype Conditions a = Conditions {conditionList :: [Condition a]}
  deriving stock (Show, Eq)
  deriving newtype (Monoid, MonoFunctor, Semigroup)

type instance Element (Conditions a) = Condition a

instance IsList (Conditions a) where
  type Item (Conditions a) = Condition a
  fromList = Conditions
  toList = (.conditionList)

instance (JSON.ToJSON a) => NamedItem (Condition a) where
  itemName = (.logicalName)
  nameToJSON = conditionToJSON

instance (JSON.ToJSON a) => JSON.ToJSON (Conditions a) where
  toJSON = namedItemToJSON . (.conditionList)

-- | This type is a wrapper around any values in a template. A value can be a
-- 'Literal', a 'Ref', or an intrinsic function. See:
-- http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference.html
data ConditionProperties a where
  CAnd :: ConditionProperties a -> ConditionProperties a -> ConditionProperties a
  CCondition :: Condition a -> ConditionProperties a
  CEquals :: (Show a, JSON.ToJSON a, Eq a, Typeable a) => ConditionProperties a -> ConditionProperties a -> ConditionProperties a
  CIf :: Text -> ConditionProperties a -> ConditionProperties a -> ConditionProperties a
  CLiteral :: a -> ConditionProperties a
  CNot :: ConditionProperties a -> ConditionProperties a
  COr :: ConditionProperties a -> ConditionProperties a -> ConditionProperties a
  CRef :: Text -> ConditionProperties a

deriving instance (Show a) => Show (ConditionProperties a)

instance (Eq a) => Eq (ConditionProperties a) where
  CAnd a b == CAnd a' b' = a == a' && b == b'
  CCondition a == CCondition a' = a == a'
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
    (CCondition c) -> JSON.object ["Condition" .= (c ^. #logicalName)]
    (CEquals x y) -> mkFunc "Fn::Equals" [JSON.toJSON x, JSON.toJSON y]
    (CIf i x y) -> mkFunc "Fn::If" [JSON.toJSON i, JSON.toJSON x, JSON.toJSON y]
    (CLiteral v) -> JSON.toJSON v
    (CNot x) -> mkFunc "Fn::Not" [JSON.toJSON x]
    (COr x y) -> mkFunc "Fn::Or" [JSON.toJSON x, JSON.toJSON y]
    (CRef r) -> JSON.object ["Ref" .= r]

mkFunc :: JSON.Key -> [JSON.Value] -> JSON.Value
mkFunc key args = JSON.object [(key, JSON.Array $ fromList args)]

-- | Class used to create a 'Ref' from another type.
class ToCRef a b where
  toCRef :: a -> ConditionProperties b
