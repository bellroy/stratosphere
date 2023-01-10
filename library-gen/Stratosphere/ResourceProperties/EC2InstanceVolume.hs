
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-mount-point.html

module Stratosphere.ResourceProperties.EC2InstanceVolume where

import Stratosphere.ResourceImports


-- | Full data type definition for EC2InstanceVolume. See 'ec2InstanceVolume'
-- for a more convenient constructor.
data EC2InstanceVolume =
  EC2InstanceVolume
  { _eC2InstanceVolumeDevice :: Val Text
  , _eC2InstanceVolumeVolumeId :: Val Text
  } deriving (Show, Eq)

instance ToJSON EC2InstanceVolume where
  toJSON EC2InstanceVolume{..} =
    object $
    catMaybes
    [ (Just . ("Device",) . toJSON) _eC2InstanceVolumeDevice
    , (Just . ("VolumeId",) . toJSON) _eC2InstanceVolumeVolumeId
    ]

-- | Constructor for 'EC2InstanceVolume' containing required fields as
-- arguments.
ec2InstanceVolume
  :: Val Text -- ^ 'ecivDevice'
  -> Val Text -- ^ 'ecivVolumeId'
  -> EC2InstanceVolume
ec2InstanceVolume devicearg volumeIdarg =
  EC2InstanceVolume
  { _eC2InstanceVolumeDevice = devicearg
  , _eC2InstanceVolumeVolumeId = volumeIdarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-mount-point.html#cfn-ec2-mountpoint-device
ecivDevice :: Lens' EC2InstanceVolume (Val Text)
ecivDevice = lens _eC2InstanceVolumeDevice (\s a -> s { _eC2InstanceVolumeDevice = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-mount-point.html#cfn-ec2-mountpoint-volumeid
ecivVolumeId :: Lens' EC2InstanceVolume (Val Text)
ecivVolumeId = lens _eC2InstanceVolumeVolumeId (\s a -> s { _eC2InstanceVolumeVolumeId = a })
