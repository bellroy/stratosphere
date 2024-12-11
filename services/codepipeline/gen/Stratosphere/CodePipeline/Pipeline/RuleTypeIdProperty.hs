module Stratosphere.CodePipeline.Pipeline.RuleTypeIdProperty (
        RuleTypeIdProperty(..), mkRuleTypeIdProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import Stratosphere.ResourceProperties
import Stratosphere.Value
data RuleTypeIdProperty
  = RuleTypeIdProperty {category :: (Prelude.Maybe (Value Prelude.Text)),
                        owner :: (Prelude.Maybe (Value Prelude.Text)),
                        provider :: (Prelude.Maybe (Value Prelude.Text)),
                        version :: (Prelude.Maybe (Value Prelude.Text))}
  deriving stock (Prelude.Eq, Prelude.Show)
mkRuleTypeIdProperty :: RuleTypeIdProperty
mkRuleTypeIdProperty
  = RuleTypeIdProperty
      {category = Prelude.Nothing, owner = Prelude.Nothing,
       provider = Prelude.Nothing, version = Prelude.Nothing}
instance ToResourceProperties RuleTypeIdProperty where
  toResourceProperties RuleTypeIdProperty {..}
    = ResourceProperties
        {awsType = "AWS::CodePipeline::Pipeline.RuleTypeId",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "Category" Prelude.<$> category,
                            (JSON..=) "Owner" Prelude.<$> owner,
                            (JSON..=) "Provider" Prelude.<$> provider,
                            (JSON..=) "Version" Prelude.<$> version])}
instance JSON.ToJSON RuleTypeIdProperty where
  toJSON RuleTypeIdProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "Category" Prelude.<$> category,
               (JSON..=) "Owner" Prelude.<$> owner,
               (JSON..=) "Provider" Prelude.<$> provider,
               (JSON..=) "Version" Prelude.<$> version]))
instance Property "Category" RuleTypeIdProperty where
  type PropertyType "Category" RuleTypeIdProperty = Value Prelude.Text
  set newValue RuleTypeIdProperty {..}
    = RuleTypeIdProperty {category = Prelude.pure newValue, ..}
instance Property "Owner" RuleTypeIdProperty where
  type PropertyType "Owner" RuleTypeIdProperty = Value Prelude.Text
  set newValue RuleTypeIdProperty {..}
    = RuleTypeIdProperty {owner = Prelude.pure newValue, ..}
instance Property "Provider" RuleTypeIdProperty where
  type PropertyType "Provider" RuleTypeIdProperty = Value Prelude.Text
  set newValue RuleTypeIdProperty {..}
    = RuleTypeIdProperty {provider = Prelude.pure newValue, ..}
instance Property "Version" RuleTypeIdProperty where
  type PropertyType "Version" RuleTypeIdProperty = Value Prelude.Text
  set newValue RuleTypeIdProperty {..}
    = RuleTypeIdProperty {version = Prelude.pure newValue, ..}