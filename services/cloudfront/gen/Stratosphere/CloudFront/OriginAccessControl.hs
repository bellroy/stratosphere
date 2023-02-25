module Stratosphere.CloudFront.OriginAccessControl (
        module Exports, OriginAccessControl(..), mkOriginAccessControl
    ) where
import qualified Data.Aeson as JSON
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.CloudFront.OriginAccessControl.OriginAccessControlConfigProperty as Exports
import Stratosphere.ResourceProperties
data OriginAccessControl
  = OriginAccessControl {originAccessControlConfig :: OriginAccessControlConfigProperty}
mkOriginAccessControl ::
  OriginAccessControlConfigProperty -> OriginAccessControl
mkOriginAccessControl originAccessControlConfig
  = OriginAccessControl
      {originAccessControlConfig = originAccessControlConfig}
instance ToResourceProperties OriginAccessControl where
  toResourceProperties OriginAccessControl {..}
    = ResourceProperties
        {awsType = "AWS::CloudFront::OriginAccessControl",
         properties = ["OriginAccessControlConfig"
                         JSON..= originAccessControlConfig]}
instance JSON.ToJSON OriginAccessControl where
  toJSON OriginAccessControl {..}
    = JSON.object
        ["OriginAccessControlConfig" JSON..= originAccessControlConfig]
instance Property "OriginAccessControlConfig" OriginAccessControl where
  type PropertyType "OriginAccessControlConfig" OriginAccessControl = OriginAccessControlConfigProperty
  set newValue OriginAccessControl {}
    = OriginAccessControl {originAccessControlConfig = newValue, ..}