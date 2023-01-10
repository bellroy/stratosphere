
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-apigatewayv2-route-parameterconstraints.html

module Stratosphere.ResourceProperties.ApiGatewayV2RouteParameterConstraints where

import Prelude
import Stratosphere.ResourceImports


-- | Full data type definition for ApiGatewayV2RouteParameterConstraints. See
-- 'apiGatewayV2RouteParameterConstraints' for a more convenient
-- constructor.
data ApiGatewayV2RouteParameterConstraints =
  ApiGatewayV2RouteParameterConstraints
  { _apiGatewayV2RouteParameterConstraintsRequired :: Val Bool
  } deriving (Show, Eq)

instance ToJSON ApiGatewayV2RouteParameterConstraints where
  toJSON ApiGatewayV2RouteParameterConstraints{..} =
    object $
    catMaybes
    [ (Just . ("Required",) . toJSON) _apiGatewayV2RouteParameterConstraintsRequired
    ]

-- | Constructor for 'ApiGatewayV2RouteParameterConstraints' containing
-- required fields as arguments.
apiGatewayV2RouteParameterConstraints
  :: Val Bool -- ^ 'agvrpcRequired'
  -> ApiGatewayV2RouteParameterConstraints
apiGatewayV2RouteParameterConstraints requiredarg =
  ApiGatewayV2RouteParameterConstraints
  { _apiGatewayV2RouteParameterConstraintsRequired = requiredarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-apigatewayv2-route-parameterconstraints.html#cfn-apigatewayv2-route-parameterconstraints-required
agvrpcRequired :: Lens' ApiGatewayV2RouteParameterConstraints (Val Bool)
agvrpcRequired = lens _apiGatewayV2RouteParameterConstraintsRequired (\s a -> s { _apiGatewayV2RouteParameterConstraintsRequired = a })
