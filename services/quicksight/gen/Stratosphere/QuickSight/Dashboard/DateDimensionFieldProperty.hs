module Stratosphere.QuickSight.Dashboard.DateDimensionFieldProperty (
        module Exports, DateDimensionFieldProperty(..),
        mkDateDimensionFieldProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.ColumnIdentifierProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.DateTimeFormatConfigurationProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Value
data DateDimensionFieldProperty
  = DateDimensionFieldProperty {column :: ColumnIdentifierProperty,
                                dateGranularity :: (Prelude.Maybe (Value Prelude.Text)),
                                fieldId :: (Value Prelude.Text),
                                formatConfiguration :: (Prelude.Maybe DateTimeFormatConfigurationProperty),
                                hierarchyId :: (Prelude.Maybe (Value Prelude.Text))}
mkDateDimensionFieldProperty ::
  ColumnIdentifierProperty
  -> Value Prelude.Text -> DateDimensionFieldProperty
mkDateDimensionFieldProperty column fieldId
  = DateDimensionFieldProperty
      {column = column, fieldId = fieldId,
       dateGranularity = Prelude.Nothing,
       formatConfiguration = Prelude.Nothing,
       hierarchyId = Prelude.Nothing}
instance ToResourceProperties DateDimensionFieldProperty where
  toResourceProperties DateDimensionFieldProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Dashboard.DateDimensionField",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["Column" JSON..= column, "FieldId" JSON..= fieldId]
                           (Prelude.catMaybes
                              [(JSON..=) "DateGranularity" Prelude.<$> dateGranularity,
                               (JSON..=) "FormatConfiguration" Prelude.<$> formatConfiguration,
                               (JSON..=) "HierarchyId" Prelude.<$> hierarchyId]))}
instance JSON.ToJSON DateDimensionFieldProperty where
  toJSON DateDimensionFieldProperty {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["Column" JSON..= column, "FieldId" JSON..= fieldId]
              (Prelude.catMaybes
                 [(JSON..=) "DateGranularity" Prelude.<$> dateGranularity,
                  (JSON..=) "FormatConfiguration" Prelude.<$> formatConfiguration,
                  (JSON..=) "HierarchyId" Prelude.<$> hierarchyId])))
instance Property "Column" DateDimensionFieldProperty where
  type PropertyType "Column" DateDimensionFieldProperty = ColumnIdentifierProperty
  set newValue DateDimensionFieldProperty {..}
    = DateDimensionFieldProperty {column = newValue, ..}
instance Property "DateGranularity" DateDimensionFieldProperty where
  type PropertyType "DateGranularity" DateDimensionFieldProperty = Value Prelude.Text
  set newValue DateDimensionFieldProperty {..}
    = DateDimensionFieldProperty
        {dateGranularity = Prelude.pure newValue, ..}
instance Property "FieldId" DateDimensionFieldProperty where
  type PropertyType "FieldId" DateDimensionFieldProperty = Value Prelude.Text
  set newValue DateDimensionFieldProperty {..}
    = DateDimensionFieldProperty {fieldId = newValue, ..}
instance Property "FormatConfiguration" DateDimensionFieldProperty where
  type PropertyType "FormatConfiguration" DateDimensionFieldProperty = DateTimeFormatConfigurationProperty
  set newValue DateDimensionFieldProperty {..}
    = DateDimensionFieldProperty
        {formatConfiguration = Prelude.pure newValue, ..}
instance Property "HierarchyId" DateDimensionFieldProperty where
  type PropertyType "HierarchyId" DateDimensionFieldProperty = Value Prelude.Text
  set newValue DateDimensionFieldProperty {..}
    = DateDimensionFieldProperty
        {hierarchyId = Prelude.pure newValue, ..}