module Stratosphere.QuickSight.Dashboard.ConditionalFormattingCustomIconOptionsProperty (
        ConditionalFormattingCustomIconOptionsProperty(..),
        mkConditionalFormattingCustomIconOptionsProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import Stratosphere.ResourceProperties
import Stratosphere.Value
data ConditionalFormattingCustomIconOptionsProperty
  = ConditionalFormattingCustomIconOptionsProperty {icon :: (Prelude.Maybe (Value Prelude.Text)),
                                                    unicodeIcon :: (Prelude.Maybe (Value Prelude.Text))}
  deriving stock (Prelude.Eq, Prelude.Show)
mkConditionalFormattingCustomIconOptionsProperty ::
  ConditionalFormattingCustomIconOptionsProperty
mkConditionalFormattingCustomIconOptionsProperty
  = ConditionalFormattingCustomIconOptionsProperty
      {icon = Prelude.Nothing, unicodeIcon = Prelude.Nothing}
instance ToResourceProperties ConditionalFormattingCustomIconOptionsProperty where
  toResourceProperties
    ConditionalFormattingCustomIconOptionsProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Dashboard.ConditionalFormattingCustomIconOptions",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "Icon" Prelude.<$> icon,
                            (JSON..=) "UnicodeIcon" Prelude.<$> unicodeIcon])}
instance JSON.ToJSON ConditionalFormattingCustomIconOptionsProperty where
  toJSON ConditionalFormattingCustomIconOptionsProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "Icon" Prelude.<$> icon,
               (JSON..=) "UnicodeIcon" Prelude.<$> unicodeIcon]))
instance Property "Icon" ConditionalFormattingCustomIconOptionsProperty where
  type PropertyType "Icon" ConditionalFormattingCustomIconOptionsProperty = Value Prelude.Text
  set newValue ConditionalFormattingCustomIconOptionsProperty {..}
    = ConditionalFormattingCustomIconOptionsProperty
        {icon = Prelude.pure newValue, ..}
instance Property "UnicodeIcon" ConditionalFormattingCustomIconOptionsProperty where
  type PropertyType "UnicodeIcon" ConditionalFormattingCustomIconOptionsProperty = Value Prelude.Text
  set newValue ConditionalFormattingCustomIconOptionsProperty {..}
    = ConditionalFormattingCustomIconOptionsProperty
        {unicodeIcon = Prelude.pure newValue, ..}