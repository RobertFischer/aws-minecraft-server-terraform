module Lib
	( handler
	, module Lib.Types
	) where

import Lib.Types
import AWS.Lambda.RuntimeAPI
import Network.AWS
import Network.AWS.EC2.StartInstances
import Network.AWS.EC2.StopInstances

handler :: GenRequest RequestPayload -> IO ResponsePayload
handler RequestPayload{reqAction,reqInstanceId} = do
	logger <- newLogger Debug stdout
	env  <- newEnv Discover
	runResourceT $ runAWS (env & envLogger .~ logger) $
		case reqAction of
			StartInstance -> doStartInstance reqInstanceId
			StopInstance -> doStopInstance reqInstanceId

doStartInstance (InstanceId instanceId) = StartInstanceResponse <$> send req
	where
		req = startInstances & sInstanceIds .~ [ instanceId ]

doStopInstance (InstanceId instanceId) = StopInstanceResponse <$> send req
	where
		req = stopInstances & siInstanceIds .~ [ instanceId ]
