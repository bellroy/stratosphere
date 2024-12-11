module Stratosphere.EntityResolution.IdMappingWorkflow.IdMappingWorkflowInputSourceProperty (
        IdMappingWorkflowInputSourceProperty(..),
        mkIdMappingWorkflowInputSourceProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import Stratosphere.ResourceProperties
import Stratosphere.Value
data IdMappingWorkflowInputSourceProperty
  = IdMappingWorkflowInputSourceProperty {inputSourceARN :: (Value Prelude.Text),
                                          schemaArn :: (Prelude.Maybe (Value Prelude.Text)),
                                          type' :: (Prelude.Maybe (Value Prelude.Text))}
  deriving stock (Prelude.Eq, Prelude.Show)
mkIdMappingWorkflowInputSourceProperty ::
  Value Prelude.Text -> IdMappingWorkflowInputSourceProperty
mkIdMappingWorkflowInputSourceProperty inputSourceARN
  = IdMappingWorkflowInputSourceProperty
      {inputSourceARN = inputSourceARN, schemaArn = Prelude.Nothing,
       type' = Prelude.Nothing}
instance ToResourceProperties IdMappingWorkflowInputSourceProperty where
  toResourceProperties IdMappingWorkflowInputSourceProperty {..}
    = ResourceProperties
        {awsType = "AWS::EntityResolution::IdMappingWorkflow.IdMappingWorkflowInputSource",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["InputSourceARN" JSON..= inputSourceARN]
                           (Prelude.catMaybes
                              [(JSON..=) "SchemaArn" Prelude.<$> schemaArn,
                               (JSON..=) "Type" Prelude.<$> type']))}
instance JSON.ToJSON IdMappingWorkflowInputSourceProperty where
  toJSON IdMappingWorkflowInputSourceProperty {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["InputSourceARN" JSON..= inputSourceARN]
              (Prelude.catMaybes
                 [(JSON..=) "SchemaArn" Prelude.<$> schemaArn,
                  (JSON..=) "Type" Prelude.<$> type'])))
instance Property "InputSourceARN" IdMappingWorkflowInputSourceProperty where
  type PropertyType "InputSourceARN" IdMappingWorkflowInputSourceProperty = Value Prelude.Text
  set newValue IdMappingWorkflowInputSourceProperty {..}
    = IdMappingWorkflowInputSourceProperty
        {inputSourceARN = newValue, ..}
instance Property "SchemaArn" IdMappingWorkflowInputSourceProperty where
  type PropertyType "SchemaArn" IdMappingWorkflowInputSourceProperty = Value Prelude.Text
  set newValue IdMappingWorkflowInputSourceProperty {..}
    = IdMappingWorkflowInputSourceProperty
        {schemaArn = Prelude.pure newValue, ..}
instance Property "Type" IdMappingWorkflowInputSourceProperty where
  type PropertyType "Type" IdMappingWorkflowInputSourceProperty = Value Prelude.Text
  set newValue IdMappingWorkflowInputSourceProperty {..}
    = IdMappingWorkflowInputSourceProperty
        {type' = Prelude.pure newValue, ..}