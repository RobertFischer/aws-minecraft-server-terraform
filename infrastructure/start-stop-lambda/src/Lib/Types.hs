module Lib.Types
    ( module Lib.Types
    , module Network.AWS.EC2.Types
    , module Control.Monad.IO.Unlift
    ) where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Text ( Text )
import Network.AWS.EC2.Types
import Control.Monad.IO.Unlift

newtype InstanceId = InstanceId Text deriving (Generic, FromJSON, ToJSON)

data RequestAction
    = StartInstance
    | StopInstance
    deriving (Generic, FromJSON, ToJSON)

data RequestPayload = RequestPayload
    { reqAction :: RequestAction
    , reqInstanceId :: Text
    } deriving (Generic, FromJSON, ToJSON)

data ResponsePayload
    = StartResponsePayload StartInstancesResponse
    | StopResponsePayload StopInstancesResponse
    deriving (Generic, FromJSON, ToJSON)
