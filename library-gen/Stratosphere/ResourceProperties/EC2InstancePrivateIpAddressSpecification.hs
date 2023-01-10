
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-network-interface-privateipspec.html

module Stratosphere.ResourceProperties.EC2InstancePrivateIpAddressSpecification where

import Prelude
import Stratosphere.ResourceImports


-- | Full data type definition for EC2InstancePrivateIpAddressSpecification.
-- See 'ec2InstancePrivateIpAddressSpecification' for a more convenient
-- constructor.
data EC2InstancePrivateIpAddressSpecification =
  EC2InstancePrivateIpAddressSpecification
  { _eC2InstancePrivateIpAddressSpecificationPrimary :: Val Bool
  , _eC2InstancePrivateIpAddressSpecificationPrivateIpAddress :: Val Text
  } deriving (Show, Eq)

instance ToJSON EC2InstancePrivateIpAddressSpecification where
  toJSON EC2InstancePrivateIpAddressSpecification{..} =
    object $
    catMaybes
    [ (Just . ("Primary",) . toJSON) _eC2InstancePrivateIpAddressSpecificationPrimary
    , (Just . ("PrivateIpAddress",) . toJSON) _eC2InstancePrivateIpAddressSpecificationPrivateIpAddress
    ]

-- | Constructor for 'EC2InstancePrivateIpAddressSpecification' containing
-- required fields as arguments.
ec2InstancePrivateIpAddressSpecification
  :: Val Bool -- ^ 'ecipiasPrimary'
  -> Val Text -- ^ 'ecipiasPrivateIpAddress'
  -> EC2InstancePrivateIpAddressSpecification
ec2InstancePrivateIpAddressSpecification primaryarg privateIpAddressarg =
  EC2InstancePrivateIpAddressSpecification
  { _eC2InstancePrivateIpAddressSpecificationPrimary = primaryarg
  , _eC2InstancePrivateIpAddressSpecificationPrivateIpAddress = privateIpAddressarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-network-interface-privateipspec.html#cfn-ec2-networkinterface-privateipspecification-primary
ecipiasPrimary :: Lens' EC2InstancePrivateIpAddressSpecification (Val Bool)
ecipiasPrimary = lens _eC2InstancePrivateIpAddressSpecificationPrimary (\s a -> s { _eC2InstancePrivateIpAddressSpecificationPrimary = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-network-interface-privateipspec.html#cfn-ec2-networkinterface-privateipspecification-privateipaddress
ecipiasPrivateIpAddress :: Lens' EC2InstancePrivateIpAddressSpecification (Val Text)
ecipiasPrivateIpAddress = lens _eC2InstancePrivateIpAddressSpecificationPrivateIpAddress (\s a -> s { _eC2InstancePrivateIpAddressSpecificationPrivateIpAddress = a })
