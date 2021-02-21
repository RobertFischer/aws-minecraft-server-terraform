module Lib
		( someFunc
		) where

import AWS.Lambda.RuntimeAPI ( runLambda )
import AWS.Lambda.RuntimeAPI.Types ( LambdaResult(..), LambdaInvocation(..) )
import Control.Lens
import Control.Monad.Trans.AWS (AWSConstraint)
import Data.Aeson ( encode, Value )
import Types
import Network.AWS
import Network.AWS.EC2
import Network.AWS.EC2.Types
import Network.AWS.EC2.StopInstances
import Network.AWS.EC2.StartInstances
import System.IO
import qualified Data.ByteString.Lazy.Char8 as C8

type AWSIO = ResourceT (AWST IO)

someFunc :: IO ()
someFunc = do
		lgr <- newLogger Debug stdout
		runResourceT . runAWS ( env & envLogger .~ lgr ) $ runLambda handler

handler :: LambdaInvocation IncomingCommand -> AWSIO (LambdaResult ())
handler LambdaInvocation{liPayload} = handleCommand liPayload >> return $ LambdaSuccess ()

handleCommand :: IncomingCommand -> AWSIO ()
handleCommand IncomingCommand{icCommand, icInstanceArn} = void $ send req
	where
		req = case icCommand of
			StartInstance -> startInstances & sInstanceIds .~ [ icInstanceArn ]
			StopInstance -> stopInstances & siInstanceIds .~ [ icInstanceArn ]
