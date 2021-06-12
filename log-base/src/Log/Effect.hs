module Log.Effect
  ( Log
  , runLog
  , logMessage
  , localData
  , localDomain
  , getLoggerEnv
  ) where

import Data.Aeson.Types
import Data.Time
import Prelude
import qualified Data.Text as T

import Effectful.Internal.Effect
import Effectful.Internal.Monad
import Log.Data
import Log.Logger

newtype Log :: Effect where
  Log :: LoggerEnv -> Log m a

runLog :: T.Text -> Logger -> Eff (Log : es) a -> Eff es a
runLog component logger = evalEffect $ IdE $ Log LoggerEnv
  { leLogger = logger
  , leComponent = component
  , leDomain = []
  , leData = []
  }

logMessage :: Log :> es => LogLevel -> T.Text -> Value -> Eff es ()
logMessage level message data_ = readerEffectM $ \(IdE (Log env)) -> unsafeEff_ $ do
  time <- getCurrentTime
  logMessageIO env time level message data_

localData :: Log :> es => [Pair] -> Eff es a -> Eff es a
localData data_ = localEffect $ \(IdE (Log env)) ->
  IdE $ Log env { leData = data_ ++ leData env }

localDomain :: Log :> es => T.Text -> Eff es a -> Eff es a
localDomain domain = localEffect $ \(IdE (Log env)) ->
  IdE $ Log env { leDomain = leDomain env ++ [domain] }

getLoggerEnv :: Log :> es => Eff es LoggerEnv
getLoggerEnv = do
  IdE (Log env) <- getEffect
  pure env
