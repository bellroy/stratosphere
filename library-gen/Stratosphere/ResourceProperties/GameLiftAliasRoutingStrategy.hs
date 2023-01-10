
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-gamelift-alias-routingstrategy.html

module Stratosphere.ResourceProperties.GameLiftAliasRoutingStrategy where

import Stratosphere.ResourceImports


-- | Full data type definition for GameLiftAliasRoutingStrategy. See
-- 'gameLiftAliasRoutingStrategy' for a more convenient constructor.
data GameLiftAliasRoutingStrategy =
  GameLiftAliasRoutingStrategy
  { _gameLiftAliasRoutingStrategyFleetId :: Maybe (Val Text)
  , _gameLiftAliasRoutingStrategyMessage :: Maybe (Val Text)
  , _gameLiftAliasRoutingStrategyType :: Maybe (Val Text)
  } deriving (Show, Eq)

instance ToJSON GameLiftAliasRoutingStrategy where
  toJSON GameLiftAliasRoutingStrategy{..} =
    object $
    catMaybes
    [ fmap (("FleetId",) . toJSON) _gameLiftAliasRoutingStrategyFleetId
    , fmap (("Message",) . toJSON) _gameLiftAliasRoutingStrategyMessage
    , fmap (("Type",) . toJSON) _gameLiftAliasRoutingStrategyType
    ]

-- | Constructor for 'GameLiftAliasRoutingStrategy' containing required fields
-- as arguments.
gameLiftAliasRoutingStrategy
  :: GameLiftAliasRoutingStrategy
gameLiftAliasRoutingStrategy  =
  GameLiftAliasRoutingStrategy
  { _gameLiftAliasRoutingStrategyFleetId = Nothing
  , _gameLiftAliasRoutingStrategyMessage = Nothing
  , _gameLiftAliasRoutingStrategyType = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-gamelift-alias-routingstrategy.html#cfn-gamelift-alias-routingstrategy-fleetid
glarsFleetId :: Lens' GameLiftAliasRoutingStrategy (Maybe (Val Text))
glarsFleetId = lens _gameLiftAliasRoutingStrategyFleetId (\s a -> s { _gameLiftAliasRoutingStrategyFleetId = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-gamelift-alias-routingstrategy.html#cfn-gamelift-alias-routingstrategy-message
glarsMessage :: Lens' GameLiftAliasRoutingStrategy (Maybe (Val Text))
glarsMessage = lens _gameLiftAliasRoutingStrategyMessage (\s a -> s { _gameLiftAliasRoutingStrategyMessage = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-gamelift-alias-routingstrategy.html#cfn-gamelift-alias-routingstrategy-type
glarsType :: Lens' GameLiftAliasRoutingStrategy (Maybe (Val Text))
glarsType = lens _gameLiftAliasRoutingStrategyType (\s a -> s { _gameLiftAliasRoutingStrategyType = a })
