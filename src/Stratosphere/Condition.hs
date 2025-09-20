module Stratosphere.Condition
  ( Condition (..),
    Conditions (..),
    conditionToJSON,
  )
where

import qualified Data.Aeson as JSON
import GHC.Exts (IsList (..))
import Stratosphere.ConditionProperties
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
