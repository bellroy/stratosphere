module Stratosphere.QuickSight.Dashboard.KPIComparisonValueConditionalFormattingProperty (
        module Exports,
        KPIComparisonValueConditionalFormattingProperty(..),
        mkKPIComparisonValueConditionalFormattingProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.ConditionalFormattingColorProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.ConditionalFormattingIconProperty as Exports
import Stratosphere.ResourceProperties
data KPIComparisonValueConditionalFormattingProperty
  = KPIComparisonValueConditionalFormattingProperty {icon :: (Prelude.Maybe ConditionalFormattingIconProperty),
                                                     textColor :: (Prelude.Maybe ConditionalFormattingColorProperty)}
  deriving stock (Prelude.Eq, Prelude.Show)
mkKPIComparisonValueConditionalFormattingProperty ::
  KPIComparisonValueConditionalFormattingProperty
mkKPIComparisonValueConditionalFormattingProperty
  = KPIComparisonValueConditionalFormattingProperty
      {icon = Prelude.Nothing, textColor = Prelude.Nothing}
instance ToResourceProperties KPIComparisonValueConditionalFormattingProperty where
  toResourceProperties
    KPIComparisonValueConditionalFormattingProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Dashboard.KPIComparisonValueConditionalFormatting",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "Icon" Prelude.<$> icon,
                            (JSON..=) "TextColor" Prelude.<$> textColor])}
instance JSON.ToJSON KPIComparisonValueConditionalFormattingProperty where
  toJSON KPIComparisonValueConditionalFormattingProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "Icon" Prelude.<$> icon,
               (JSON..=) "TextColor" Prelude.<$> textColor]))
instance Property "Icon" KPIComparisonValueConditionalFormattingProperty where
  type PropertyType "Icon" KPIComparisonValueConditionalFormattingProperty = ConditionalFormattingIconProperty
  set newValue KPIComparisonValueConditionalFormattingProperty {..}
    = KPIComparisonValueConditionalFormattingProperty
        {icon = Prelude.pure newValue, ..}
instance Property "TextColor" KPIComparisonValueConditionalFormattingProperty where
  type PropertyType "TextColor" KPIComparisonValueConditionalFormattingProperty = ConditionalFormattingColorProperty
  set newValue KPIComparisonValueConditionalFormattingProperty {..}
    = KPIComparisonValueConditionalFormattingProperty
        {textColor = Prelude.pure newValue, ..}