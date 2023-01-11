
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-waf-webacl-action.html

module Stratosphere.ResourceProperties.WAFWebACLWafAction where

import Prelude
import Stratosphere.ResourceImports


-- | Full data type definition for WAFWebACLWafAction. See
-- 'wafWebACLWafAction' for a more convenient constructor.
data WAFWebACLWafAction =
  WAFWebACLWafAction
  { _wAFWebACLWafActionType :: Val Text
  } deriving (Show, Eq)

instance ToJSON WAFWebACLWafAction where
  toJSON WAFWebACLWafAction{..} =
    object $
    catMaybes
    [ (Just . ("Type",) . toJSON) _wAFWebACLWafActionType
    ]

-- | Constructor for 'WAFWebACLWafAction' containing required fields as
-- arguments.
wafWebACLWafAction
  :: Val Text -- ^ 'wafwaclwaType'
  -> WAFWebACLWafAction
wafWebACLWafAction typearg =
  WAFWebACLWafAction
  { _wAFWebACLWafActionType = typearg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-waf-webacl-action.html#cfn-waf-webacl-action-type
wafwaclwaType :: Lens' WAFWebACLWafAction (Val Text)
wafwaclwaType = lens _wAFWebACLWafActionType (\s a -> s { _wAFWebACLWafActionType = a })
