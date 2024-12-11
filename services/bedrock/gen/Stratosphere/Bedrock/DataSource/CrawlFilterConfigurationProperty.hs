module Stratosphere.Bedrock.DataSource.CrawlFilterConfigurationProperty (
        module Exports, CrawlFilterConfigurationProperty(..),
        mkCrawlFilterConfigurationProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.Bedrock.DataSource.PatternObjectFilterConfigurationProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Value
data CrawlFilterConfigurationProperty
  = CrawlFilterConfigurationProperty {patternObjectFilter :: (Prelude.Maybe PatternObjectFilterConfigurationProperty),
                                      type' :: (Value Prelude.Text)}
  deriving stock (Prelude.Eq, Prelude.Show)
mkCrawlFilterConfigurationProperty ::
  Value Prelude.Text -> CrawlFilterConfigurationProperty
mkCrawlFilterConfigurationProperty type'
  = CrawlFilterConfigurationProperty
      {type' = type', patternObjectFilter = Prelude.Nothing}
instance ToResourceProperties CrawlFilterConfigurationProperty where
  toResourceProperties CrawlFilterConfigurationProperty {..}
    = ResourceProperties
        {awsType = "AWS::Bedrock::DataSource.CrawlFilterConfiguration",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["Type" JSON..= type']
                           (Prelude.catMaybes
                              [(JSON..=) "PatternObjectFilter"
                                 Prelude.<$> patternObjectFilter]))}
instance JSON.ToJSON CrawlFilterConfigurationProperty where
  toJSON CrawlFilterConfigurationProperty {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["Type" JSON..= type']
              (Prelude.catMaybes
                 [(JSON..=) "PatternObjectFilter"
                    Prelude.<$> patternObjectFilter])))
instance Property "PatternObjectFilter" CrawlFilterConfigurationProperty where
  type PropertyType "PatternObjectFilter" CrawlFilterConfigurationProperty = PatternObjectFilterConfigurationProperty
  set newValue CrawlFilterConfigurationProperty {..}
    = CrawlFilterConfigurationProperty
        {patternObjectFilter = Prelude.pure newValue, ..}
instance Property "Type" CrawlFilterConfigurationProperty where
  type PropertyType "Type" CrawlFilterConfigurationProperty = Value Prelude.Text
  set newValue CrawlFilterConfigurationProperty {..}
    = CrawlFilterConfigurationProperty {type' = newValue, ..}