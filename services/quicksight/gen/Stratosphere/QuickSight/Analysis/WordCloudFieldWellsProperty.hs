module Stratosphere.QuickSight.Analysis.WordCloudFieldWellsProperty (
        module Exports, WordCloudFieldWellsProperty(..),
        mkWordCloudFieldWellsProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QuickSight.Analysis.WordCloudAggregatedFieldWellsProperty as Exports
import Stratosphere.ResourceProperties
data WordCloudFieldWellsProperty
  = WordCloudFieldWellsProperty {wordCloudAggregatedFieldWells :: (Prelude.Maybe WordCloudAggregatedFieldWellsProperty)}
  deriving stock (Prelude.Eq, Prelude.Show)
mkWordCloudFieldWellsProperty :: WordCloudFieldWellsProperty
mkWordCloudFieldWellsProperty
  = WordCloudFieldWellsProperty
      {wordCloudAggregatedFieldWells = Prelude.Nothing}
instance ToResourceProperties WordCloudFieldWellsProperty where
  toResourceProperties WordCloudFieldWellsProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Analysis.WordCloudFieldWells",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "WordCloudAggregatedFieldWells"
                              Prelude.<$> wordCloudAggregatedFieldWells])}
instance JSON.ToJSON WordCloudFieldWellsProperty where
  toJSON WordCloudFieldWellsProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "WordCloudAggregatedFieldWells"
                 Prelude.<$> wordCloudAggregatedFieldWells]))
instance Property "WordCloudAggregatedFieldWells" WordCloudFieldWellsProperty where
  type PropertyType "WordCloudAggregatedFieldWells" WordCloudFieldWellsProperty = WordCloudAggregatedFieldWellsProperty
  set newValue WordCloudFieldWellsProperty {}
    = WordCloudFieldWellsProperty
        {wordCloudAggregatedFieldWells = Prelude.pure newValue, ..}