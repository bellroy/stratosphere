module Stratosphere.QuickSight.Dashboard.ParameterControlProperty (
        module Exports, ParameterControlProperty(..),
        mkParameterControlProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.ParameterDateTimePickerControlProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.ParameterDropDownControlProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.ParameterListControlProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.ParameterSliderControlProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.ParameterTextAreaControlProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.ParameterTextFieldControlProperty as Exports
import Stratosphere.ResourceProperties
data ParameterControlProperty
  = ParameterControlProperty {dateTimePicker :: (Prelude.Maybe ParameterDateTimePickerControlProperty),
                              dropdown :: (Prelude.Maybe ParameterDropDownControlProperty),
                              list :: (Prelude.Maybe ParameterListControlProperty),
                              slider :: (Prelude.Maybe ParameterSliderControlProperty),
                              textArea :: (Prelude.Maybe ParameterTextAreaControlProperty),
                              textField :: (Prelude.Maybe ParameterTextFieldControlProperty)}
  deriving stock (Prelude.Eq, Prelude.Show)
mkParameterControlProperty :: ParameterControlProperty
mkParameterControlProperty
  = ParameterControlProperty
      {dateTimePicker = Prelude.Nothing, dropdown = Prelude.Nothing,
       list = Prelude.Nothing, slider = Prelude.Nothing,
       textArea = Prelude.Nothing, textField = Prelude.Nothing}
instance ToResourceProperties ParameterControlProperty where
  toResourceProperties ParameterControlProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Dashboard.ParameterControl",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "DateTimePicker" Prelude.<$> dateTimePicker,
                            (JSON..=) "Dropdown" Prelude.<$> dropdown,
                            (JSON..=) "List" Prelude.<$> list,
                            (JSON..=) "Slider" Prelude.<$> slider,
                            (JSON..=) "TextArea" Prelude.<$> textArea,
                            (JSON..=) "TextField" Prelude.<$> textField])}
instance JSON.ToJSON ParameterControlProperty where
  toJSON ParameterControlProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "DateTimePicker" Prelude.<$> dateTimePicker,
               (JSON..=) "Dropdown" Prelude.<$> dropdown,
               (JSON..=) "List" Prelude.<$> list,
               (JSON..=) "Slider" Prelude.<$> slider,
               (JSON..=) "TextArea" Prelude.<$> textArea,
               (JSON..=) "TextField" Prelude.<$> textField]))
instance Property "DateTimePicker" ParameterControlProperty where
  type PropertyType "DateTimePicker" ParameterControlProperty = ParameterDateTimePickerControlProperty
  set newValue ParameterControlProperty {..}
    = ParameterControlProperty
        {dateTimePicker = Prelude.pure newValue, ..}
instance Property "Dropdown" ParameterControlProperty where
  type PropertyType "Dropdown" ParameterControlProperty = ParameterDropDownControlProperty
  set newValue ParameterControlProperty {..}
    = ParameterControlProperty {dropdown = Prelude.pure newValue, ..}
instance Property "List" ParameterControlProperty where
  type PropertyType "List" ParameterControlProperty = ParameterListControlProperty
  set newValue ParameterControlProperty {..}
    = ParameterControlProperty {list = Prelude.pure newValue, ..}
instance Property "Slider" ParameterControlProperty where
  type PropertyType "Slider" ParameterControlProperty = ParameterSliderControlProperty
  set newValue ParameterControlProperty {..}
    = ParameterControlProperty {slider = Prelude.pure newValue, ..}
instance Property "TextArea" ParameterControlProperty where
  type PropertyType "TextArea" ParameterControlProperty = ParameterTextAreaControlProperty
  set newValue ParameterControlProperty {..}
    = ParameterControlProperty {textArea = Prelude.pure newValue, ..}
instance Property "TextField" ParameterControlProperty where
  type PropertyType "TextField" ParameterControlProperty = ParameterTextFieldControlProperty
  set newValue ParameterControlProperty {..}
    = ParameterControlProperty {textField = Prelude.pure newValue, ..}