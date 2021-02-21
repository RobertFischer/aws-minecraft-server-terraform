module Types (module Types) where

import Data.Aeson ( FromJSON(..), ToJSON(..), withObject, withText )
import qualified Data.Text as T

data InstanceCommand = StartInstance | StopInstance

instance FromJSON InstanceCommand where
	parseJSON = withText "InstanceCommand" $ \case
		"start" -> StartInstance
		"stop" -> StopInstance
		invalid -> fail . T.unpack $ "Expected 'start' or 'stop' but saw '" <> invalid <> "'"

data IncomingCommand = IncomingCommand
	{ icCommand :: InstanceCommand
	, icInstanceArn :: Text
	}

instance FromJSON IncomingCommand where
	parseJSON = withObject "IncomingCommand" $ \v -> IncomingCommand
		<$> v .: "command"
		<*> v .: "arn"



