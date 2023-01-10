
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-wafregional-bytematchset-fieldtomatch.html

module Stratosphere.ResourceProperties.WAFRegionalByteMatchSetFieldToMatch where

import Stratosphere.ResourceImports


-- | Full data type definition for WAFRegionalByteMatchSetFieldToMatch. See
-- 'wafRegionalByteMatchSetFieldToMatch' for a more convenient constructor.
data WAFRegionalByteMatchSetFieldToMatch =
  WAFRegionalByteMatchSetFieldToMatch
  { _wAFRegionalByteMatchSetFieldToMatchData :: Maybe (Val Text)
  , _wAFRegionalByteMatchSetFieldToMatchType :: Val Text
  } deriving (Show, Eq)

instance ToJSON WAFRegionalByteMatchSetFieldToMatch where
  toJSON WAFRegionalByteMatchSetFieldToMatch{..} =
    object $
    catMaybes
    [ fmap (("Data",) . toJSON) _wAFRegionalByteMatchSetFieldToMatchData
    , (Just . ("Type",) . toJSON) _wAFRegionalByteMatchSetFieldToMatchType
    ]

-- | Constructor for 'WAFRegionalByteMatchSetFieldToMatch' containing required
-- fields as arguments.
wafRegionalByteMatchSetFieldToMatch
  :: Val Text -- ^ 'wafrbmsftmType'
  -> WAFRegionalByteMatchSetFieldToMatch
wafRegionalByteMatchSetFieldToMatch typearg =
  WAFRegionalByteMatchSetFieldToMatch
  { _wAFRegionalByteMatchSetFieldToMatchData = Nothing
  , _wAFRegionalByteMatchSetFieldToMatchType = typearg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-wafregional-bytematchset-fieldtomatch.html#cfn-wafregional-bytematchset-fieldtomatch-data
wafrbmsftmData :: Lens' WAFRegionalByteMatchSetFieldToMatch (Maybe (Val Text))
wafrbmsftmData = lens _wAFRegionalByteMatchSetFieldToMatchData (\s a -> s { _wAFRegionalByteMatchSetFieldToMatchData = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-wafregional-bytematchset-fieldtomatch.html#cfn-wafregional-bytematchset-fieldtomatch-type
wafrbmsftmType :: Lens' WAFRegionalByteMatchSetFieldToMatch (Val Text)
wafrbmsftmType = lens _wAFRegionalByteMatchSetFieldToMatchType (\s a -> s { _wAFRegionalByteMatchSetFieldToMatchType = a })
