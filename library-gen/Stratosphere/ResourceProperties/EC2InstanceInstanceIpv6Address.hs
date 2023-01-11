
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-instance-instanceipv6address.html

module Stratosphere.ResourceProperties.EC2InstanceInstanceIpv6Address where

import Prelude
import Stratosphere.ResourceImports


-- | Full data type definition for EC2InstanceInstanceIpv6Address. See
-- 'ec2InstanceInstanceIpv6Address' for a more convenient constructor.
data EC2InstanceInstanceIpv6Address =
  EC2InstanceInstanceIpv6Address
  { _eC2InstanceInstanceIpv6AddressIpv6Address :: Val Text
  } deriving (Show, Eq)

instance ToJSON EC2InstanceInstanceIpv6Address where
  toJSON EC2InstanceInstanceIpv6Address{..} =
    object $
    catMaybes
    [ (Just . ("Ipv6Address",) . toJSON) _eC2InstanceInstanceIpv6AddressIpv6Address
    ]

-- | Constructor for 'EC2InstanceInstanceIpv6Address' containing required
-- fields as arguments.
ec2InstanceInstanceIpv6Address
  :: Val Text -- ^ 'eciiiaIpv6Address'
  -> EC2InstanceInstanceIpv6Address
ec2InstanceInstanceIpv6Address ipv6Addressarg =
  EC2InstanceInstanceIpv6Address
  { _eC2InstanceInstanceIpv6AddressIpv6Address = ipv6Addressarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-instance-instanceipv6address.html#cfn-ec2-instance-instanceipv6address-ipv6address
eciiiaIpv6Address :: Lens' EC2InstanceInstanceIpv6Address (Val Text)
eciiiaIpv6Address = lens _eC2InstanceInstanceIpv6AddressIpv6Address (\s a -> s { _eC2InstanceInstanceIpv6AddressIpv6Address = a })
