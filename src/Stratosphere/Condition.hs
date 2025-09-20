module Stratosphere.Condition
  ( Condition (..),
    Conditions (..),
    conditionToJSON,
  )
where

import qualified Data.Aeson as JSON
import GHC.Exts (IsList (..))
import Stratosphere.NamedItem
import Stratosphere.Prelude
import Stratosphere.Property
import Stratosphere.Value

data Condition = Condition
  { logicalName :: Text,
    value :: Value Bool
  }
  deriving (Show, Eq, Generic)

instance Property "LogicalName" Condition where
  type PropertyType "LogicalName" Condition = Text
  set newValue Condition {..} = Condition {logicalName = newValue, ..}

instance Property "Value" Condition where
  type PropertyType "Value" Condition = Value Bool
  set newValue Condition {..} = Condition {value = newValue, ..}

instance ToRef Condition b where
  toRef = Ref . (.logicalName)

conditionToJSON :: Condition -> JSON.Value
conditionToJSON Condition {..} = JSON.toJSON value

-- | Wrapper around a list of 'Conditions's to we can modify the aeson instances.
newtype Conditions = Conditions {conditionList :: [Condition]}
  deriving stock (Show, Eq)
  deriving newtype (Monoid, MonoFunctor, Semigroup)

type instance Element Conditions = Condition

instance IsList Conditions where
  type Item Conditions = Condition
  fromList = Conditions
  toList = (.conditionList)

instance NamedItem Condition where
  itemName = (.logicalName)
  nameToJSON = conditionToJSON

instance JSON.ToJSON Conditions where
  toJSON = namedItemToJSON . (.conditionList)
