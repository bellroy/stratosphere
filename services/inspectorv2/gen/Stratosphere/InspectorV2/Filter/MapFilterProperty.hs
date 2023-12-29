module Stratosphere.InspectorV2.Filter.MapFilterProperty (
        MapFilterProperty(..), mkMapFilterProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import Stratosphere.ResourceProperties
import Stratosphere.Value
data MapFilterProperty
  = MapFilterProperty {comparison :: (Value Prelude.Text),
                       key :: (Prelude.Maybe (Value Prelude.Text)),
                       value :: (Prelude.Maybe (Value Prelude.Text))}
  deriving stock (Prelude.Eq, Prelude.Show)
mkMapFilterProperty :: Value Prelude.Text -> MapFilterProperty
mkMapFilterProperty comparison
  = MapFilterProperty
      {comparison = comparison, key = Prelude.Nothing,
       value = Prelude.Nothing}
instance ToResourceProperties MapFilterProperty where
  toResourceProperties MapFilterProperty {..}
    = ResourceProperties
        {awsType = "AWS::InspectorV2::Filter.MapFilter",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["Comparison" JSON..= comparison]
                           (Prelude.catMaybes
                              [(JSON..=) "Key" Prelude.<$> key,
                               (JSON..=) "Value" Prelude.<$> value]))}
instance JSON.ToJSON MapFilterProperty where
  toJSON MapFilterProperty {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["Comparison" JSON..= comparison]
              (Prelude.catMaybes
                 [(JSON..=) "Key" Prelude.<$> key,
                  (JSON..=) "Value" Prelude.<$> value])))
instance Property "Comparison" MapFilterProperty where
  type PropertyType "Comparison" MapFilterProperty = Value Prelude.Text
  set newValue MapFilterProperty {..}
    = MapFilterProperty {comparison = newValue, ..}
instance Property "Key" MapFilterProperty where
  type PropertyType "Key" MapFilterProperty = Value Prelude.Text
  set newValue MapFilterProperty {..}
    = MapFilterProperty {key = Prelude.pure newValue, ..}
instance Property "Value" MapFilterProperty where
  type PropertyType "Value" MapFilterProperty = Value Prelude.Text
  set newValue MapFilterProperty {..}
    = MapFilterProperty {value = Prelude.pure newValue, ..}