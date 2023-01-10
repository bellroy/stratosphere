
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-glue-trigger-condition.html

module Stratosphere.ResourceProperties.GlueTriggerCondition where

import Stratosphere.ResourceImports


-- | Full data type definition for GlueTriggerCondition. See
-- 'glueTriggerCondition' for a more convenient constructor.
data GlueTriggerCondition =
  GlueTriggerCondition
  { _glueTriggerConditionCrawlState :: Maybe (Val Text)
  , _glueTriggerConditionCrawlerName :: Maybe (Val Text)
  , _glueTriggerConditionJobName :: Maybe (Val Text)
  , _glueTriggerConditionLogicalOperator :: Maybe (Val Text)
  , _glueTriggerConditionState :: Maybe (Val Text)
  } deriving (Show, Eq)

instance ToJSON GlueTriggerCondition where
  toJSON GlueTriggerCondition{..} =
    object $
    catMaybes
    [ fmap (("CrawlState",) . toJSON) _glueTriggerConditionCrawlState
    , fmap (("CrawlerName",) . toJSON) _glueTriggerConditionCrawlerName
    , fmap (("JobName",) . toJSON) _glueTriggerConditionJobName
    , fmap (("LogicalOperator",) . toJSON) _glueTriggerConditionLogicalOperator
    , fmap (("State",) . toJSON) _glueTriggerConditionState
    ]

-- | Constructor for 'GlueTriggerCondition' containing required fields as
-- arguments.
glueTriggerCondition
  :: GlueTriggerCondition
glueTriggerCondition  =
  GlueTriggerCondition
  { _glueTriggerConditionCrawlState = Nothing
  , _glueTriggerConditionCrawlerName = Nothing
  , _glueTriggerConditionJobName = Nothing
  , _glueTriggerConditionLogicalOperator = Nothing
  , _glueTriggerConditionState = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-glue-trigger-condition.html#cfn-glue-trigger-condition-crawlstate
gtcCrawlState :: Lens' GlueTriggerCondition (Maybe (Val Text))
gtcCrawlState = lens _glueTriggerConditionCrawlState (\s a -> s { _glueTriggerConditionCrawlState = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-glue-trigger-condition.html#cfn-glue-trigger-condition-crawlername
gtcCrawlerName :: Lens' GlueTriggerCondition (Maybe (Val Text))
gtcCrawlerName = lens _glueTriggerConditionCrawlerName (\s a -> s { _glueTriggerConditionCrawlerName = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-glue-trigger-condition.html#cfn-glue-trigger-condition-jobname
gtcJobName :: Lens' GlueTriggerCondition (Maybe (Val Text))
gtcJobName = lens _glueTriggerConditionJobName (\s a -> s { _glueTriggerConditionJobName = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-glue-trigger-condition.html#cfn-glue-trigger-condition-logicaloperator
gtcLogicalOperator :: Lens' GlueTriggerCondition (Maybe (Val Text))
gtcLogicalOperator = lens _glueTriggerConditionLogicalOperator (\s a -> s { _glueTriggerConditionLogicalOperator = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-glue-trigger-condition.html#cfn-glue-trigger-condition-state
gtcState :: Lens' GlueTriggerCondition (Maybe (Val Text))
gtcState = lens _glueTriggerConditionState (\s a -> s { _glueTriggerConditionState = a })
