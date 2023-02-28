module Stratosphere.KMS.ReplicaKey (
        ReplicaKey(..), mkReplicaKey
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import Stratosphere.ResourceProperties
import Stratosphere.Tag
import Stratosphere.Value
data ReplicaKey
  = ReplicaKey {description :: (Prelude.Maybe (Value Prelude.Text)),
                enabled :: (Prelude.Maybe (Value Prelude.Bool)),
                keyPolicy :: JSON.Object,
                pendingWindowInDays :: (Prelude.Maybe (Value Prelude.Integer)),
                primaryKeyArn :: (Value Prelude.Text),
                tags :: (Prelude.Maybe [Tag])}
mkReplicaKey :: JSON.Object -> Value Prelude.Text -> ReplicaKey
mkReplicaKey keyPolicy primaryKeyArn
  = ReplicaKey
      {keyPolicy = keyPolicy, primaryKeyArn = primaryKeyArn,
       description = Prelude.Nothing, enabled = Prelude.Nothing,
       pendingWindowInDays = Prelude.Nothing, tags = Prelude.Nothing}
instance ToResourceProperties ReplicaKey where
  toResourceProperties ReplicaKey {..}
    = ResourceProperties
        {awsType = "AWS::KMS::ReplicaKey", supportsTags = Prelude.True,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["KeyPolicy" JSON..= keyPolicy,
                            "PrimaryKeyArn" JSON..= primaryKeyArn]
                           (Prelude.catMaybes
                              [(JSON..=) "Description" Prelude.<$> description,
                               (JSON..=) "Enabled" Prelude.<$> enabled,
                               (JSON..=) "PendingWindowInDays" Prelude.<$> pendingWindowInDays,
                               (JSON..=) "Tags" Prelude.<$> tags]))}
instance JSON.ToJSON ReplicaKey where
  toJSON ReplicaKey {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["KeyPolicy" JSON..= keyPolicy,
               "PrimaryKeyArn" JSON..= primaryKeyArn]
              (Prelude.catMaybes
                 [(JSON..=) "Description" Prelude.<$> description,
                  (JSON..=) "Enabled" Prelude.<$> enabled,
                  (JSON..=) "PendingWindowInDays" Prelude.<$> pendingWindowInDays,
                  (JSON..=) "Tags" Prelude.<$> tags])))
instance Property "Description" ReplicaKey where
  type PropertyType "Description" ReplicaKey = Value Prelude.Text
  set newValue ReplicaKey {..}
    = ReplicaKey {description = Prelude.pure newValue, ..}
instance Property "Enabled" ReplicaKey where
  type PropertyType "Enabled" ReplicaKey = Value Prelude.Bool
  set newValue ReplicaKey {..}
    = ReplicaKey {enabled = Prelude.pure newValue, ..}
instance Property "KeyPolicy" ReplicaKey where
  type PropertyType "KeyPolicy" ReplicaKey = JSON.Object
  set newValue ReplicaKey {..}
    = ReplicaKey {keyPolicy = newValue, ..}
instance Property "PendingWindowInDays" ReplicaKey where
  type PropertyType "PendingWindowInDays" ReplicaKey = Value Prelude.Integer
  set newValue ReplicaKey {..}
    = ReplicaKey {pendingWindowInDays = Prelude.pure newValue, ..}
instance Property "PrimaryKeyArn" ReplicaKey where
  type PropertyType "PrimaryKeyArn" ReplicaKey = Value Prelude.Text
  set newValue ReplicaKey {..}
    = ReplicaKey {primaryKeyArn = newValue, ..}
instance Property "Tags" ReplicaKey where
  type PropertyType "Tags" ReplicaKey = [Tag]
  set newValue ReplicaKey {..}
    = ReplicaKey {tags = Prelude.pure newValue, ..}