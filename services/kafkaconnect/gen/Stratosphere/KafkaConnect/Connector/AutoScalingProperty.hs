module Stratosphere.KafkaConnect.Connector.AutoScalingProperty (
        module Exports, AutoScalingProperty(..), mkAutoScalingProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.KafkaConnect.Connector.ScaleInPolicyProperty as Exports
import {-# SOURCE #-} Stratosphere.KafkaConnect.Connector.ScaleOutPolicyProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Value
data AutoScalingProperty
  = AutoScalingProperty {maxWorkerCount :: (Value Prelude.Integer),
                         mcuCount :: (Value Prelude.Integer),
                         minWorkerCount :: (Value Prelude.Integer),
                         scaleInPolicy :: ScaleInPolicyProperty,
                         scaleOutPolicy :: ScaleOutPolicyProperty}
mkAutoScalingProperty ::
  Value Prelude.Integer
  -> Value Prelude.Integer
     -> Value Prelude.Integer
        -> ScaleInPolicyProperty
           -> ScaleOutPolicyProperty -> AutoScalingProperty
mkAutoScalingProperty
  maxWorkerCount
  mcuCount
  minWorkerCount
  scaleInPolicy
  scaleOutPolicy
  = AutoScalingProperty
      {maxWorkerCount = maxWorkerCount, mcuCount = mcuCount,
       minWorkerCount = minWorkerCount, scaleInPolicy = scaleInPolicy,
       scaleOutPolicy = scaleOutPolicy}
instance ToResourceProperties AutoScalingProperty where
  toResourceProperties AutoScalingProperty {..}
    = ResourceProperties
        {awsType = "AWS::KafkaConnect::Connector.AutoScaling",
         supportsTags = Prelude.False,
         properties = ["MaxWorkerCount" JSON..= maxWorkerCount,
                       "McuCount" JSON..= mcuCount,
                       "MinWorkerCount" JSON..= minWorkerCount,
                       "ScaleInPolicy" JSON..= scaleInPolicy,
                       "ScaleOutPolicy" JSON..= scaleOutPolicy]}
instance JSON.ToJSON AutoScalingProperty where
  toJSON AutoScalingProperty {..}
    = JSON.object
        ["MaxWorkerCount" JSON..= maxWorkerCount,
         "McuCount" JSON..= mcuCount,
         "MinWorkerCount" JSON..= minWorkerCount,
         "ScaleInPolicy" JSON..= scaleInPolicy,
         "ScaleOutPolicy" JSON..= scaleOutPolicy]
instance Property "MaxWorkerCount" AutoScalingProperty where
  type PropertyType "MaxWorkerCount" AutoScalingProperty = Value Prelude.Integer
  set newValue AutoScalingProperty {..}
    = AutoScalingProperty {maxWorkerCount = newValue, ..}
instance Property "McuCount" AutoScalingProperty where
  type PropertyType "McuCount" AutoScalingProperty = Value Prelude.Integer
  set newValue AutoScalingProperty {..}
    = AutoScalingProperty {mcuCount = newValue, ..}
instance Property "MinWorkerCount" AutoScalingProperty where
  type PropertyType "MinWorkerCount" AutoScalingProperty = Value Prelude.Integer
  set newValue AutoScalingProperty {..}
    = AutoScalingProperty {minWorkerCount = newValue, ..}
instance Property "ScaleInPolicy" AutoScalingProperty where
  type PropertyType "ScaleInPolicy" AutoScalingProperty = ScaleInPolicyProperty
  set newValue AutoScalingProperty {..}
    = AutoScalingProperty {scaleInPolicy = newValue, ..}
instance Property "ScaleOutPolicy" AutoScalingProperty where
  type PropertyType "ScaleOutPolicy" AutoScalingProperty = ScaleOutPolicyProperty
  set newValue AutoScalingProperty {..}
    = AutoScalingProperty {scaleOutPolicy = newValue, ..}