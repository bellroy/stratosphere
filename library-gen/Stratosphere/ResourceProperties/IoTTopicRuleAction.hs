
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html

module Stratosphere.ResourceProperties.IoTTopicRuleAction where

import Prelude
import Stratosphere.ResourceImports
import Stratosphere.ResourceProperties.IoTTopicRuleCloudwatchAlarmAction
import Stratosphere.ResourceProperties.IoTTopicRuleCloudwatchMetricAction
import Stratosphere.ResourceProperties.IoTTopicRuleDynamoDBAction
import Stratosphere.ResourceProperties.IoTTopicRuleDynamoDBV2Action
import Stratosphere.ResourceProperties.IoTTopicRuleElasticsearchAction
import Stratosphere.ResourceProperties.IoTTopicRuleFirehoseAction
import Stratosphere.ResourceProperties.IoTTopicRuleHttpAction
import Stratosphere.ResourceProperties.IoTTopicRuleIotAnalyticsAction
import Stratosphere.ResourceProperties.IoTTopicRuleIotEventsAction
import Stratosphere.ResourceProperties.IoTTopicRuleIotSiteWiseAction
import Stratosphere.ResourceProperties.IoTTopicRuleKinesisAction
import Stratosphere.ResourceProperties.IoTTopicRuleLambdaAction
import Stratosphere.ResourceProperties.IoTTopicRuleRepublishAction
import Stratosphere.ResourceProperties.IoTTopicRuleS3Action
import Stratosphere.ResourceProperties.IoTTopicRuleSnsAction
import Stratosphere.ResourceProperties.IoTTopicRuleSqsAction
import Stratosphere.ResourceProperties.IoTTopicRuleStepFunctionsAction

-- | Full data type definition for IoTTopicRuleAction. See
-- 'ioTTopicRuleAction' for a more convenient constructor.
data IoTTopicRuleAction =
  IoTTopicRuleAction
  { _ioTTopicRuleActionCloudwatchAlarm :: Maybe IoTTopicRuleCloudwatchAlarmAction
  , _ioTTopicRuleActionCloudwatchMetric :: Maybe IoTTopicRuleCloudwatchMetricAction
  , _ioTTopicRuleActionDynamoDB :: Maybe IoTTopicRuleDynamoDBAction
  , _ioTTopicRuleActionDynamoDBv2 :: Maybe IoTTopicRuleDynamoDBV2Action
  , _ioTTopicRuleActionElasticsearch :: Maybe IoTTopicRuleElasticsearchAction
  , _ioTTopicRuleActionFirehose :: Maybe IoTTopicRuleFirehoseAction
  , _ioTTopicRuleActionHttp :: Maybe IoTTopicRuleHttpAction
  , _ioTTopicRuleActionIotAnalytics :: Maybe IoTTopicRuleIotAnalyticsAction
  , _ioTTopicRuleActionIotEvents :: Maybe IoTTopicRuleIotEventsAction
  , _ioTTopicRuleActionIotSiteWise :: Maybe IoTTopicRuleIotSiteWiseAction
  , _ioTTopicRuleActionKinesis :: Maybe IoTTopicRuleKinesisAction
  , _ioTTopicRuleActionLambda :: Maybe IoTTopicRuleLambdaAction
  , _ioTTopicRuleActionRepublish :: Maybe IoTTopicRuleRepublishAction
  , _ioTTopicRuleActionS3 :: Maybe IoTTopicRuleS3Action
  , _ioTTopicRuleActionSns :: Maybe IoTTopicRuleSnsAction
  , _ioTTopicRuleActionSqs :: Maybe IoTTopicRuleSqsAction
  , _ioTTopicRuleActionStepFunctions :: Maybe IoTTopicRuleStepFunctionsAction
  } deriving (Show, Eq)

instance ToJSON IoTTopicRuleAction where
  toJSON IoTTopicRuleAction{..} =
    object $
    catMaybes
    [ fmap (("CloudwatchAlarm",) . toJSON) _ioTTopicRuleActionCloudwatchAlarm
    , fmap (("CloudwatchMetric",) . toJSON) _ioTTopicRuleActionCloudwatchMetric
    , fmap (("DynamoDB",) . toJSON) _ioTTopicRuleActionDynamoDB
    , fmap (("DynamoDBv2",) . toJSON) _ioTTopicRuleActionDynamoDBv2
    , fmap (("Elasticsearch",) . toJSON) _ioTTopicRuleActionElasticsearch
    , fmap (("Firehose",) . toJSON) _ioTTopicRuleActionFirehose
    , fmap (("Http",) . toJSON) _ioTTopicRuleActionHttp
    , fmap (("IotAnalytics",) . toJSON) _ioTTopicRuleActionIotAnalytics
    , fmap (("IotEvents",) . toJSON) _ioTTopicRuleActionIotEvents
    , fmap (("IotSiteWise",) . toJSON) _ioTTopicRuleActionIotSiteWise
    , fmap (("Kinesis",) . toJSON) _ioTTopicRuleActionKinesis
    , fmap (("Lambda",) . toJSON) _ioTTopicRuleActionLambda
    , fmap (("Republish",) . toJSON) _ioTTopicRuleActionRepublish
    , fmap (("S3",) . toJSON) _ioTTopicRuleActionS3
    , fmap (("Sns",) . toJSON) _ioTTopicRuleActionSns
    , fmap (("Sqs",) . toJSON) _ioTTopicRuleActionSqs
    , fmap (("StepFunctions",) . toJSON) _ioTTopicRuleActionStepFunctions
    ]

-- | Constructor for 'IoTTopicRuleAction' containing required fields as
-- arguments.
ioTTopicRuleAction
  :: IoTTopicRuleAction
ioTTopicRuleAction  =
  IoTTopicRuleAction
  { _ioTTopicRuleActionCloudwatchAlarm = Nothing
  , _ioTTopicRuleActionCloudwatchMetric = Nothing
  , _ioTTopicRuleActionDynamoDB = Nothing
  , _ioTTopicRuleActionDynamoDBv2 = Nothing
  , _ioTTopicRuleActionElasticsearch = Nothing
  , _ioTTopicRuleActionFirehose = Nothing
  , _ioTTopicRuleActionHttp = Nothing
  , _ioTTopicRuleActionIotAnalytics = Nothing
  , _ioTTopicRuleActionIotEvents = Nothing
  , _ioTTopicRuleActionIotSiteWise = Nothing
  , _ioTTopicRuleActionKinesis = Nothing
  , _ioTTopicRuleActionLambda = Nothing
  , _ioTTopicRuleActionRepublish = Nothing
  , _ioTTopicRuleActionS3 = Nothing
  , _ioTTopicRuleActionSns = Nothing
  , _ioTTopicRuleActionSqs = Nothing
  , _ioTTopicRuleActionStepFunctions = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-cloudwatchalarm
ittraCloudwatchAlarm :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleCloudwatchAlarmAction)
ittraCloudwatchAlarm = lens _ioTTopicRuleActionCloudwatchAlarm (\s a -> s { _ioTTopicRuleActionCloudwatchAlarm = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-cloudwatchmetric
ittraCloudwatchMetric :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleCloudwatchMetricAction)
ittraCloudwatchMetric = lens _ioTTopicRuleActionCloudwatchMetric (\s a -> s { _ioTTopicRuleActionCloudwatchMetric = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-dynamodb
ittraDynamoDB :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleDynamoDBAction)
ittraDynamoDB = lens _ioTTopicRuleActionDynamoDB (\s a -> s { _ioTTopicRuleActionDynamoDB = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-dynamodbv2
ittraDynamoDBv2 :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleDynamoDBV2Action)
ittraDynamoDBv2 = lens _ioTTopicRuleActionDynamoDBv2 (\s a -> s { _ioTTopicRuleActionDynamoDBv2 = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-elasticsearch
ittraElasticsearch :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleElasticsearchAction)
ittraElasticsearch = lens _ioTTopicRuleActionElasticsearch (\s a -> s { _ioTTopicRuleActionElasticsearch = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-firehose
ittraFirehose :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleFirehoseAction)
ittraFirehose = lens _ioTTopicRuleActionFirehose (\s a -> s { _ioTTopicRuleActionFirehose = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-http
ittraHttp :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleHttpAction)
ittraHttp = lens _ioTTopicRuleActionHttp (\s a -> s { _ioTTopicRuleActionHttp = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-iotanalytics
ittraIotAnalytics :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleIotAnalyticsAction)
ittraIotAnalytics = lens _ioTTopicRuleActionIotAnalytics (\s a -> s { _ioTTopicRuleActionIotAnalytics = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-iotevents
ittraIotEvents :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleIotEventsAction)
ittraIotEvents = lens _ioTTopicRuleActionIotEvents (\s a -> s { _ioTTopicRuleActionIotEvents = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-iotsitewise
ittraIotSiteWise :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleIotSiteWiseAction)
ittraIotSiteWise = lens _ioTTopicRuleActionIotSiteWise (\s a -> s { _ioTTopicRuleActionIotSiteWise = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-kinesis
ittraKinesis :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleKinesisAction)
ittraKinesis = lens _ioTTopicRuleActionKinesis (\s a -> s { _ioTTopicRuleActionKinesis = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-lambda
ittraLambda :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleLambdaAction)
ittraLambda = lens _ioTTopicRuleActionLambda (\s a -> s { _ioTTopicRuleActionLambda = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-republish
ittraRepublish :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleRepublishAction)
ittraRepublish = lens _ioTTopicRuleActionRepublish (\s a -> s { _ioTTopicRuleActionRepublish = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-s3
ittraS3 :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleS3Action)
ittraS3 = lens _ioTTopicRuleActionS3 (\s a -> s { _ioTTopicRuleActionS3 = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-sns
ittraSns :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleSnsAction)
ittraSns = lens _ioTTopicRuleActionSns (\s a -> s { _ioTTopicRuleActionSns = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-sqs
ittraSqs :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleSqsAction)
ittraSqs = lens _ioTTopicRuleActionSqs (\s a -> s { _ioTTopicRuleActionSqs = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iot-topicrule-action.html#cfn-iot-topicrule-action-stepfunctions
ittraStepFunctions :: Lens' IoTTopicRuleAction (Maybe IoTTopicRuleStepFunctionsAction)
ittraStepFunctions = lens _ioTTopicRuleActionStepFunctions (\s a -> s { _ioTTopicRuleActionStepFunctions = a })
