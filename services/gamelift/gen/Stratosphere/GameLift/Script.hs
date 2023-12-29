module Stratosphere.GameLift.Script (
        module Exports, Script(..), mkScript
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.GameLift.Script.S3LocationProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Tag
import Stratosphere.Value
data Script
  = Script {name :: (Prelude.Maybe (Value Prelude.Text)),
            storageLocation :: S3LocationProperty,
            tags :: (Prelude.Maybe [Tag]),
            version :: (Prelude.Maybe (Value Prelude.Text))}
  deriving stock (Prelude.Eq, Prelude.Show)
mkScript :: S3LocationProperty -> Script
mkScript storageLocation
  = Script
      {storageLocation = storageLocation, name = Prelude.Nothing,
       tags = Prelude.Nothing, version = Prelude.Nothing}
instance ToResourceProperties Script where
  toResourceProperties Script {..}
    = ResourceProperties
        {awsType = "AWS::GameLift::Script", supportsTags = Prelude.True,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["StorageLocation" JSON..= storageLocation]
                           (Prelude.catMaybes
                              [(JSON..=) "Name" Prelude.<$> name,
                               (JSON..=) "Tags" Prelude.<$> tags,
                               (JSON..=) "Version" Prelude.<$> version]))}
instance JSON.ToJSON Script where
  toJSON Script {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["StorageLocation" JSON..= storageLocation]
              (Prelude.catMaybes
                 [(JSON..=) "Name" Prelude.<$> name,
                  (JSON..=) "Tags" Prelude.<$> tags,
                  (JSON..=) "Version" Prelude.<$> version])))
instance Property "Name" Script where
  type PropertyType "Name" Script = Value Prelude.Text
  set newValue Script {..}
    = Script {name = Prelude.pure newValue, ..}
instance Property "StorageLocation" Script where
  type PropertyType "StorageLocation" Script = S3LocationProperty
  set newValue Script {..} = Script {storageLocation = newValue, ..}
instance Property "Tags" Script where
  type PropertyType "Tags" Script = [Tag]
  set newValue Script {..}
    = Script {tags = Prelude.pure newValue, ..}
instance Property "Version" Script where
  type PropertyType "Version" Script = Value Prelude.Text
  set newValue Script {..}
    = Script {version = Prelude.pure newValue, ..}