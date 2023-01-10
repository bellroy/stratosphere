
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-networkinterface-instanceipv6address.html

module Stratosphere.ResourceProperties.EC2NetworkInterfaceInstanceIpv6Address where

import Stratosphere.ResourceImports


-- | Full data type definition for EC2NetworkInterfaceInstanceIpv6Address. See
-- 'ec2NetworkInterfaceInstanceIpv6Address' for a more convenient
-- constructor.
data EC2NetworkInterfaceInstanceIpv6Address =
  EC2NetworkInterfaceInstanceIpv6Address
  { _eC2NetworkInterfaceInstanceIpv6AddressIpv6Address :: Val Text
  } deriving (Show, Eq)

instance ToJSON EC2NetworkInterfaceInstanceIpv6Address where
  toJSON EC2NetworkInterfaceInstanceIpv6Address{..} =
    object $
    catMaybes
    [ (Just . ("Ipv6Address",) . toJSON) _eC2NetworkInterfaceInstanceIpv6AddressIpv6Address
    ]

-- | Constructor for 'EC2NetworkInterfaceInstanceIpv6Address' containing
-- required fields as arguments.
ec2NetworkInterfaceInstanceIpv6Address
  :: Val Text -- ^ 'ecniiiaIpv6Address'
  -> EC2NetworkInterfaceInstanceIpv6Address
ec2NetworkInterfaceInstanceIpv6Address ipv6Addressarg =
  EC2NetworkInterfaceInstanceIpv6Address
  { _eC2NetworkInterfaceInstanceIpv6AddressIpv6Address = ipv6Addressarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ec2-networkinterface-instanceipv6address.html#cfn-ec2-networkinterface-instanceipv6address-ipv6address
ecniiiaIpv6Address :: Lens' EC2NetworkInterfaceInstanceIpv6Address (Val Text)
ecniiiaIpv6Address = lens _eC2NetworkInterfaceInstanceIpv6AddressIpv6Address (\s a -> s { _eC2NetworkInterfaceInstanceIpv6AddressIpv6Address = a })
