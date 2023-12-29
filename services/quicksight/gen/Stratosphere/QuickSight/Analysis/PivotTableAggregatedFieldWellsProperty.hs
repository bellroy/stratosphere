module Stratosphere.QuickSight.Analysis.PivotTableAggregatedFieldWellsProperty (
        module Exports, PivotTableAggregatedFieldWellsProperty(..),
        mkPivotTableAggregatedFieldWellsProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QuickSight.Analysis.DimensionFieldProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Analysis.MeasureFieldProperty as Exports
import Stratosphere.ResourceProperties
data PivotTableAggregatedFieldWellsProperty
  = PivotTableAggregatedFieldWellsProperty {columns :: (Prelude.Maybe [DimensionFieldProperty]),
                                            rows :: (Prelude.Maybe [DimensionFieldProperty]),
                                            values :: (Prelude.Maybe [MeasureFieldProperty])}
  deriving stock (Prelude.Eq, Prelude.Show)
mkPivotTableAggregatedFieldWellsProperty ::
  PivotTableAggregatedFieldWellsProperty
mkPivotTableAggregatedFieldWellsProperty
  = PivotTableAggregatedFieldWellsProperty
      {columns = Prelude.Nothing, rows = Prelude.Nothing,
       values = Prelude.Nothing}
instance ToResourceProperties PivotTableAggregatedFieldWellsProperty where
  toResourceProperties PivotTableAggregatedFieldWellsProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Analysis.PivotTableAggregatedFieldWells",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "Columns" Prelude.<$> columns,
                            (JSON..=) "Rows" Prelude.<$> rows,
                            (JSON..=) "Values" Prelude.<$> values])}
instance JSON.ToJSON PivotTableAggregatedFieldWellsProperty where
  toJSON PivotTableAggregatedFieldWellsProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "Columns" Prelude.<$> columns,
               (JSON..=) "Rows" Prelude.<$> rows,
               (JSON..=) "Values" Prelude.<$> values]))
instance Property "Columns" PivotTableAggregatedFieldWellsProperty where
  type PropertyType "Columns" PivotTableAggregatedFieldWellsProperty = [DimensionFieldProperty]
  set newValue PivotTableAggregatedFieldWellsProperty {..}
    = PivotTableAggregatedFieldWellsProperty
        {columns = Prelude.pure newValue, ..}
instance Property "Rows" PivotTableAggregatedFieldWellsProperty where
  type PropertyType "Rows" PivotTableAggregatedFieldWellsProperty = [DimensionFieldProperty]
  set newValue PivotTableAggregatedFieldWellsProperty {..}
    = PivotTableAggregatedFieldWellsProperty
        {rows = Prelude.pure newValue, ..}
instance Property "Values" PivotTableAggregatedFieldWellsProperty where
  type PropertyType "Values" PivotTableAggregatedFieldWellsProperty = [MeasureFieldProperty]
  set newValue PivotTableAggregatedFieldWellsProperty {..}
    = PivotTableAggregatedFieldWellsProperty
        {values = Prelude.pure newValue, ..}