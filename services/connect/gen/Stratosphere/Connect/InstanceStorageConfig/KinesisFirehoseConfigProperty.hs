module Stratosphere.Connect.InstanceStorageConfig.KinesisFirehoseConfigProperty (
        KinesisFirehoseConfigProperty(..), mkKinesisFirehoseConfigProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import Stratosphere.ResourceProperties
import Stratosphere.Value
data KinesisFirehoseConfigProperty
  = KinesisFirehoseConfigProperty {firehoseArn :: (Value Prelude.Text)}
  deriving stock (Prelude.Eq, Prelude.Show)
mkKinesisFirehoseConfigProperty ::
  Value Prelude.Text -> KinesisFirehoseConfigProperty
mkKinesisFirehoseConfigProperty firehoseArn
  = KinesisFirehoseConfigProperty {firehoseArn = firehoseArn}
instance ToResourceProperties KinesisFirehoseConfigProperty where
  toResourceProperties KinesisFirehoseConfigProperty {..}
    = ResourceProperties
        {awsType = "AWS::Connect::InstanceStorageConfig.KinesisFirehoseConfig",
         supportsTags = Prelude.False,
         properties = ["FirehoseArn" JSON..= firehoseArn]}
instance JSON.ToJSON KinesisFirehoseConfigProperty where
  toJSON KinesisFirehoseConfigProperty {..}
    = JSON.object ["FirehoseArn" JSON..= firehoseArn]
instance Property "FirehoseArn" KinesisFirehoseConfigProperty where
  type PropertyType "FirehoseArn" KinesisFirehoseConfigProperty = Value Prelude.Text
  set newValue KinesisFirehoseConfigProperty {}
    = KinesisFirehoseConfigProperty {firehoseArn = newValue, ..}