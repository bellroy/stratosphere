module Stratosphere.Bedrock.DataSource.TransformationProperty (
        module Exports, TransformationProperty(..),
        mkTransformationProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.Bedrock.DataSource.TransformationFunctionProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Value
data TransformationProperty
  = TransformationProperty {stepToApply :: (Value Prelude.Text),
                            transformationFunction :: TransformationFunctionProperty}
  deriving stock (Prelude.Eq, Prelude.Show)
mkTransformationProperty ::
  Value Prelude.Text
  -> TransformationFunctionProperty -> TransformationProperty
mkTransformationProperty stepToApply transformationFunction
  = TransformationProperty
      {stepToApply = stepToApply,
       transformationFunction = transformationFunction}
instance ToResourceProperties TransformationProperty where
  toResourceProperties TransformationProperty {..}
    = ResourceProperties
        {awsType = "AWS::Bedrock::DataSource.Transformation",
         supportsTags = Prelude.False,
         properties = ["StepToApply" JSON..= stepToApply,
                       "TransformationFunction" JSON..= transformationFunction]}
instance JSON.ToJSON TransformationProperty where
  toJSON TransformationProperty {..}
    = JSON.object
        ["StepToApply" JSON..= stepToApply,
         "TransformationFunction" JSON..= transformationFunction]
instance Property "StepToApply" TransformationProperty where
  type PropertyType "StepToApply" TransformationProperty = Value Prelude.Text
  set newValue TransformationProperty {..}
    = TransformationProperty {stepToApply = newValue, ..}
instance Property "TransformationFunction" TransformationProperty where
  type PropertyType "TransformationFunction" TransformationProperty = TransformationFunctionProperty
  set newValue TransformationProperty {..}
    = TransformationProperty {transformationFunction = newValue, ..}