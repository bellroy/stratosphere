module Stratosphere.QuickSight.Template.DataPathColorProperty (
        module Exports, DataPathColorProperty(..), mkDataPathColorProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QuickSight.Template.DataPathValueProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Value
data DataPathColorProperty
  = DataPathColorProperty {color :: (Value Prelude.Text),
                           element :: DataPathValueProperty,
                           timeGranularity :: (Prelude.Maybe (Value Prelude.Text))}
  deriving stock (Prelude.Eq, Prelude.Show)
mkDataPathColorProperty ::
  Value Prelude.Text
  -> DataPathValueProperty -> DataPathColorProperty
mkDataPathColorProperty color element
  = DataPathColorProperty
      {color = color, element = element,
       timeGranularity = Prelude.Nothing}
instance ToResourceProperties DataPathColorProperty where
  toResourceProperties DataPathColorProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Template.DataPathColor",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["Color" JSON..= color, "Element" JSON..= element]
                           (Prelude.catMaybes
                              [(JSON..=) "TimeGranularity" Prelude.<$> timeGranularity]))}
instance JSON.ToJSON DataPathColorProperty where
  toJSON DataPathColorProperty {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["Color" JSON..= color, "Element" JSON..= element]
              (Prelude.catMaybes
                 [(JSON..=) "TimeGranularity" Prelude.<$> timeGranularity])))
instance Property "Color" DataPathColorProperty where
  type PropertyType "Color" DataPathColorProperty = Value Prelude.Text
  set newValue DataPathColorProperty {..}
    = DataPathColorProperty {color = newValue, ..}
instance Property "Element" DataPathColorProperty where
  type PropertyType "Element" DataPathColorProperty = DataPathValueProperty
  set newValue DataPathColorProperty {..}
    = DataPathColorProperty {element = newValue, ..}
instance Property "TimeGranularity" DataPathColorProperty where
  type PropertyType "TimeGranularity" DataPathColorProperty = Value Prelude.Text
  set newValue DataPathColorProperty {..}
    = DataPathColorProperty
        {timeGranularity = Prelude.pure newValue, ..}