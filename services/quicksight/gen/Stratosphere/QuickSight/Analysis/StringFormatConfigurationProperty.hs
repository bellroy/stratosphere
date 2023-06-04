module Stratosphere.QuickSight.Analysis.StringFormatConfigurationProperty (
        module Exports, StringFormatConfigurationProperty(..),
        mkStringFormatConfigurationProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QuickSight.Analysis.NullValueFormatConfigurationProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Analysis.NumericFormatConfigurationProperty as Exports
import Stratosphere.ResourceProperties
data StringFormatConfigurationProperty
  = StringFormatConfigurationProperty {nullValueFormatConfiguration :: (Prelude.Maybe NullValueFormatConfigurationProperty),
                                       numericFormatConfiguration :: (Prelude.Maybe NumericFormatConfigurationProperty)}
mkStringFormatConfigurationProperty ::
  StringFormatConfigurationProperty
mkStringFormatConfigurationProperty
  = StringFormatConfigurationProperty
      {nullValueFormatConfiguration = Prelude.Nothing,
       numericFormatConfiguration = Prelude.Nothing}
instance ToResourceProperties StringFormatConfigurationProperty where
  toResourceProperties StringFormatConfigurationProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Analysis.StringFormatConfiguration",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "NullValueFormatConfiguration"
                              Prelude.<$> nullValueFormatConfiguration,
                            (JSON..=) "NumericFormatConfiguration"
                              Prelude.<$> numericFormatConfiguration])}
instance JSON.ToJSON StringFormatConfigurationProperty where
  toJSON StringFormatConfigurationProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "NullValueFormatConfiguration"
                 Prelude.<$> nullValueFormatConfiguration,
               (JSON..=) "NumericFormatConfiguration"
                 Prelude.<$> numericFormatConfiguration]))
instance Property "NullValueFormatConfiguration" StringFormatConfigurationProperty where
  type PropertyType "NullValueFormatConfiguration" StringFormatConfigurationProperty = NullValueFormatConfigurationProperty
  set newValue StringFormatConfigurationProperty {..}
    = StringFormatConfigurationProperty
        {nullValueFormatConfiguration = Prelude.pure newValue, ..}
instance Property "NumericFormatConfiguration" StringFormatConfigurationProperty where
  type PropertyType "NumericFormatConfiguration" StringFormatConfigurationProperty = NumericFormatConfigurationProperty
  set newValue StringFormatConfigurationProperty {..}
    = StringFormatConfigurationProperty
        {numericFormatConfiguration = Prelude.pure newValue, ..}