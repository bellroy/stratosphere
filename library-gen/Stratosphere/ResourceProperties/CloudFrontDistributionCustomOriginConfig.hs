
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cloudfront-distribution-customoriginconfig.html

module Stratosphere.ResourceProperties.CloudFrontDistributionCustomOriginConfig where

import Stratosphere.ResourceImports


-- | Full data type definition for CloudFrontDistributionCustomOriginConfig.
-- See 'cloudFrontDistributionCustomOriginConfig' for a more convenient
-- constructor.
data CloudFrontDistributionCustomOriginConfig =
  CloudFrontDistributionCustomOriginConfig
  { _cloudFrontDistributionCustomOriginConfigHTTPPort :: Maybe (Val Integer)
  , _cloudFrontDistributionCustomOriginConfigHTTPSPort :: Maybe (Val Integer)
  , _cloudFrontDistributionCustomOriginConfigOriginKeepaliveTimeout :: Maybe (Val Integer)
  , _cloudFrontDistributionCustomOriginConfigOriginProtocolPolicy :: Val Text
  , _cloudFrontDistributionCustomOriginConfigOriginReadTimeout :: Maybe (Val Integer)
  , _cloudFrontDistributionCustomOriginConfigOriginSSLProtocols :: Maybe (ValList Text)
  } deriving (Show, Eq)

instance ToJSON CloudFrontDistributionCustomOriginConfig where
  toJSON CloudFrontDistributionCustomOriginConfig{..} =
    object $
    catMaybes
    [ fmap (("HTTPPort",) . toJSON) _cloudFrontDistributionCustomOriginConfigHTTPPort
    , fmap (("HTTPSPort",) . toJSON) _cloudFrontDistributionCustomOriginConfigHTTPSPort
    , fmap (("OriginKeepaliveTimeout",) . toJSON) _cloudFrontDistributionCustomOriginConfigOriginKeepaliveTimeout
    , (Just . ("OriginProtocolPolicy",) . toJSON) _cloudFrontDistributionCustomOriginConfigOriginProtocolPolicy
    , fmap (("OriginReadTimeout",) . toJSON) _cloudFrontDistributionCustomOriginConfigOriginReadTimeout
    , fmap (("OriginSSLProtocols",) . toJSON) _cloudFrontDistributionCustomOriginConfigOriginSSLProtocols
    ]

-- | Constructor for 'CloudFrontDistributionCustomOriginConfig' containing
-- required fields as arguments.
cloudFrontDistributionCustomOriginConfig
  :: Val Text -- ^ 'cfdcocOriginProtocolPolicy'
  -> CloudFrontDistributionCustomOriginConfig
cloudFrontDistributionCustomOriginConfig originProtocolPolicyarg =
  CloudFrontDistributionCustomOriginConfig
  { _cloudFrontDistributionCustomOriginConfigHTTPPort = Nothing
  , _cloudFrontDistributionCustomOriginConfigHTTPSPort = Nothing
  , _cloudFrontDistributionCustomOriginConfigOriginKeepaliveTimeout = Nothing
  , _cloudFrontDistributionCustomOriginConfigOriginProtocolPolicy = originProtocolPolicyarg
  , _cloudFrontDistributionCustomOriginConfigOriginReadTimeout = Nothing
  , _cloudFrontDistributionCustomOriginConfigOriginSSLProtocols = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cloudfront-distribution-customoriginconfig.html#cfn-cloudfront-distribution-customoriginconfig-httpport
cfdcocHTTPPort :: Lens' CloudFrontDistributionCustomOriginConfig (Maybe (Val Integer))
cfdcocHTTPPort = lens _cloudFrontDistributionCustomOriginConfigHTTPPort (\s a -> s { _cloudFrontDistributionCustomOriginConfigHTTPPort = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cloudfront-distribution-customoriginconfig.html#cfn-cloudfront-distribution-customoriginconfig-httpsport
cfdcocHTTPSPort :: Lens' CloudFrontDistributionCustomOriginConfig (Maybe (Val Integer))
cfdcocHTTPSPort = lens _cloudFrontDistributionCustomOriginConfigHTTPSPort (\s a -> s { _cloudFrontDistributionCustomOriginConfigHTTPSPort = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cloudfront-distribution-customoriginconfig.html#cfn-cloudfront-distribution-customoriginconfig-originkeepalivetimeout
cfdcocOriginKeepaliveTimeout :: Lens' CloudFrontDistributionCustomOriginConfig (Maybe (Val Integer))
cfdcocOriginKeepaliveTimeout = lens _cloudFrontDistributionCustomOriginConfigOriginKeepaliveTimeout (\s a -> s { _cloudFrontDistributionCustomOriginConfigOriginKeepaliveTimeout = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cloudfront-distribution-customoriginconfig.html#cfn-cloudfront-distribution-customoriginconfig-originprotocolpolicy
cfdcocOriginProtocolPolicy :: Lens' CloudFrontDistributionCustomOriginConfig (Val Text)
cfdcocOriginProtocolPolicy = lens _cloudFrontDistributionCustomOriginConfigOriginProtocolPolicy (\s a -> s { _cloudFrontDistributionCustomOriginConfigOriginProtocolPolicy = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cloudfront-distribution-customoriginconfig.html#cfn-cloudfront-distribution-customoriginconfig-originreadtimeout
cfdcocOriginReadTimeout :: Lens' CloudFrontDistributionCustomOriginConfig (Maybe (Val Integer))
cfdcocOriginReadTimeout = lens _cloudFrontDistributionCustomOriginConfigOriginReadTimeout (\s a -> s { _cloudFrontDistributionCustomOriginConfigOriginReadTimeout = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cloudfront-distribution-customoriginconfig.html#cfn-cloudfront-distribution-customoriginconfig-originsslprotocols
cfdcocOriginSSLProtocols :: Lens' CloudFrontDistributionCustomOriginConfig (Maybe (ValList Text))
cfdcocOriginSSLProtocols = lens _cloudFrontDistributionCustomOriginConfigOriginSSLProtocols (\s a -> s { _cloudFrontDistributionCustomOriginConfigOriginSSLProtocols = a })
