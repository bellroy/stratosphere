
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-spotfleet-spotfleetrequestconfigdata-launchspecifications-blockdevicemappings-ebs.html

module Stratosphere.ResourceProperties.EC2SpotFleetEbsBlockDevice where

import Prelude
import Stratosphere.ResourceImports


-- | Full data type definition for EC2SpotFleetEbsBlockDevice. See
-- 'ec2SpotFleetEbsBlockDevice' for a more convenient constructor.
data EC2SpotFleetEbsBlockDevice =
  EC2SpotFleetEbsBlockDevice
  { _eC2SpotFleetEbsBlockDeviceDeleteOnTermination :: Maybe (Val Bool)
  , _eC2SpotFleetEbsBlockDeviceEncrypted :: Maybe (Val Bool)
  , _eC2SpotFleetEbsBlockDeviceIops :: Maybe (Val Integer)
  , _eC2SpotFleetEbsBlockDeviceSnapshotId :: Maybe (Val Text)
  , _eC2SpotFleetEbsBlockDeviceVolumeSize :: Maybe (Val Integer)
  , _eC2SpotFleetEbsBlockDeviceVolumeType :: Maybe (Val Text)
  } deriving (Show, Eq)

instance ToJSON EC2SpotFleetEbsBlockDevice where
  toJSON EC2SpotFleetEbsBlockDevice{..} =
    object $
    catMaybes
    [ fmap (("DeleteOnTermination",) . toJSON) _eC2SpotFleetEbsBlockDeviceDeleteOnTermination
    , fmap (("Encrypted",) . toJSON) _eC2SpotFleetEbsBlockDeviceEncrypted
    , fmap (("Iops",) . toJSON) _eC2SpotFleetEbsBlockDeviceIops
    , fmap (("SnapshotId",) . toJSON) _eC2SpotFleetEbsBlockDeviceSnapshotId
    , fmap (("VolumeSize",) . toJSON) _eC2SpotFleetEbsBlockDeviceVolumeSize
    , fmap (("VolumeType",) . toJSON) _eC2SpotFleetEbsBlockDeviceVolumeType
    ]

-- | Constructor for 'EC2SpotFleetEbsBlockDevice' containing required fields
-- as arguments.
ec2SpotFleetEbsBlockDevice
  :: EC2SpotFleetEbsBlockDevice
ec2SpotFleetEbsBlockDevice  =
  EC2SpotFleetEbsBlockDevice
  { _eC2SpotFleetEbsBlockDeviceDeleteOnTermination = Nothing
  , _eC2SpotFleetEbsBlockDeviceEncrypted = Nothing
  , _eC2SpotFleetEbsBlockDeviceIops = Nothing
  , _eC2SpotFleetEbsBlockDeviceSnapshotId = Nothing
  , _eC2SpotFleetEbsBlockDeviceVolumeSize = Nothing
  , _eC2SpotFleetEbsBlockDeviceVolumeType = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-spotfleet-spotfleetrequestconfigdata-launchspecifications-blockdevicemappings-ebs.html#cfn-ec2-spotfleet-ebsblockdevice-deleteontermination
ecsfebdDeleteOnTermination :: Lens' EC2SpotFleetEbsBlockDevice (Maybe (Val Bool))
ecsfebdDeleteOnTermination = lens _eC2SpotFleetEbsBlockDeviceDeleteOnTermination (\s a -> s { _eC2SpotFleetEbsBlockDeviceDeleteOnTermination = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-spotfleet-spotfleetrequestconfigdata-launchspecifications-blockdevicemappings-ebs.html#cfn-ec2-spotfleet-ebsblockdevice-encrypted
ecsfebdEncrypted :: Lens' EC2SpotFleetEbsBlockDevice (Maybe (Val Bool))
ecsfebdEncrypted = lens _eC2SpotFleetEbsBlockDeviceEncrypted (\s a -> s { _eC2SpotFleetEbsBlockDeviceEncrypted = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-spotfleet-spotfleetrequestconfigdata-launchspecifications-blockdevicemappings-ebs.html#cfn-ec2-spotfleet-ebsblockdevice-iops
ecsfebdIops :: Lens' EC2SpotFleetEbsBlockDevice (Maybe (Val Integer))
ecsfebdIops = lens _eC2SpotFleetEbsBlockDeviceIops (\s a -> s { _eC2SpotFleetEbsBlockDeviceIops = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-spotfleet-spotfleetrequestconfigdata-launchspecifications-blockdevicemappings-ebs.html#cfn-ec2-spotfleet-ebsblockdevice-snapshotid
ecsfebdSnapshotId :: Lens' EC2SpotFleetEbsBlockDevice (Maybe (Val Text))
ecsfebdSnapshotId = lens _eC2SpotFleetEbsBlockDeviceSnapshotId (\s a -> s { _eC2SpotFleetEbsBlockDeviceSnapshotId = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-spotfleet-spotfleetrequestconfigdata-launchspecifications-blockdevicemappings-ebs.html#cfn-ec2-spotfleet-ebsblockdevice-volumesize
ecsfebdVolumeSize :: Lens' EC2SpotFleetEbsBlockDevice (Maybe (Val Integer))
ecsfebdVolumeSize = lens _eC2SpotFleetEbsBlockDeviceVolumeSize (\s a -> s { _eC2SpotFleetEbsBlockDeviceVolumeSize = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-spotfleet-spotfleetrequestconfigdata-launchspecifications-blockdevicemappings-ebs.html#cfn-ec2-spotfleet-ebsblockdevice-volumetype
ecsfebdVolumeType :: Lens' EC2SpotFleetEbsBlockDevice (Maybe (Val Text))
ecsfebdVolumeType = lens _eC2SpotFleetEbsBlockDeviceVolumeType (\s a -> s { _eC2SpotFleetEbsBlockDeviceVolumeType = a })
