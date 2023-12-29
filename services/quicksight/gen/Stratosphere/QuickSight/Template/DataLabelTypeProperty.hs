module Stratosphere.QuickSight.Template.DataLabelTypeProperty (
        module Exports, DataLabelTypeProperty(..), mkDataLabelTypeProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QuickSight.Template.DataPathLabelTypeProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Template.FieldLabelTypeProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Template.MaximumLabelTypeProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Template.MinimumLabelTypeProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Template.RangeEndsLabelTypeProperty as Exports
import Stratosphere.ResourceProperties
data DataLabelTypeProperty
  = DataLabelTypeProperty {dataPathLabelType :: (Prelude.Maybe DataPathLabelTypeProperty),
                           fieldLabelType :: (Prelude.Maybe FieldLabelTypeProperty),
                           maximumLabelType :: (Prelude.Maybe MaximumLabelTypeProperty),
                           minimumLabelType :: (Prelude.Maybe MinimumLabelTypeProperty),
                           rangeEndsLabelType :: (Prelude.Maybe RangeEndsLabelTypeProperty)}
  deriving stock (Prelude.Eq, Prelude.Show)
mkDataLabelTypeProperty :: DataLabelTypeProperty
mkDataLabelTypeProperty
  = DataLabelTypeProperty
      {dataPathLabelType = Prelude.Nothing,
       fieldLabelType = Prelude.Nothing,
       maximumLabelType = Prelude.Nothing,
       minimumLabelType = Prelude.Nothing,
       rangeEndsLabelType = Prelude.Nothing}
instance ToResourceProperties DataLabelTypeProperty where
  toResourceProperties DataLabelTypeProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Template.DataLabelType",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "DataPathLabelType" Prelude.<$> dataPathLabelType,
                            (JSON..=) "FieldLabelType" Prelude.<$> fieldLabelType,
                            (JSON..=) "MaximumLabelType" Prelude.<$> maximumLabelType,
                            (JSON..=) "MinimumLabelType" Prelude.<$> minimumLabelType,
                            (JSON..=) "RangeEndsLabelType" Prelude.<$> rangeEndsLabelType])}
instance JSON.ToJSON DataLabelTypeProperty where
  toJSON DataLabelTypeProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "DataPathLabelType" Prelude.<$> dataPathLabelType,
               (JSON..=) "FieldLabelType" Prelude.<$> fieldLabelType,
               (JSON..=) "MaximumLabelType" Prelude.<$> maximumLabelType,
               (JSON..=) "MinimumLabelType" Prelude.<$> minimumLabelType,
               (JSON..=) "RangeEndsLabelType" Prelude.<$> rangeEndsLabelType]))
instance Property "DataPathLabelType" DataLabelTypeProperty where
  type PropertyType "DataPathLabelType" DataLabelTypeProperty = DataPathLabelTypeProperty
  set newValue DataLabelTypeProperty {..}
    = DataLabelTypeProperty
        {dataPathLabelType = Prelude.pure newValue, ..}
instance Property "FieldLabelType" DataLabelTypeProperty where
  type PropertyType "FieldLabelType" DataLabelTypeProperty = FieldLabelTypeProperty
  set newValue DataLabelTypeProperty {..}
    = DataLabelTypeProperty
        {fieldLabelType = Prelude.pure newValue, ..}
instance Property "MaximumLabelType" DataLabelTypeProperty where
  type PropertyType "MaximumLabelType" DataLabelTypeProperty = MaximumLabelTypeProperty
  set newValue DataLabelTypeProperty {..}
    = DataLabelTypeProperty
        {maximumLabelType = Prelude.pure newValue, ..}
instance Property "MinimumLabelType" DataLabelTypeProperty where
  type PropertyType "MinimumLabelType" DataLabelTypeProperty = MinimumLabelTypeProperty
  set newValue DataLabelTypeProperty {..}
    = DataLabelTypeProperty
        {minimumLabelType = Prelude.pure newValue, ..}
instance Property "RangeEndsLabelType" DataLabelTypeProperty where
  type PropertyType "RangeEndsLabelType" DataLabelTypeProperty = RangeEndsLabelTypeProperty
  set newValue DataLabelTypeProperty {..}
    = DataLabelTypeProperty
        {rangeEndsLabelType = Prelude.pure newValue, ..}