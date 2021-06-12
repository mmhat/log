-- | The 'LogT' monad transformer for adding logging capabilities to any monad.
{-# LANGUAGE CPP #-}
module Log.Monad (
    Logger
  , LoggerEnv(..)
  , logMessageIO
  , InnerLogT
  , LogT(..)
  , runLogT
  , mapLogT
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.IO.Unlift
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import Data.Text (Text)
import Data.Time
import Prelude
import qualified Control.Monad.Fail as MF

import Log.Class
import Log.Logger

type InnerLogT = ReaderT LoggerEnv

-- | Monad transformer that adds logging capabilities to the underlying monad.
newtype LogT m a = LogT { unLogT :: InnerLogT m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadBase b, MonadCatch
           ,MonadIO, MonadMask, MonadPlus, MonadThrow, MonadTrans, MF.MonadFail
           ,MonadError e, MonadWriter w, MonadState s)

instance MonadReader r m => MonadReader r (LogT m) where
    ask   = lift ask
    local = mapLogT . local

-- | Run a 'LogT' computation.
--
-- Note that in the case of asynchronous/bulk loggers 'runLogT'
-- doesn't guarantee that all messages are actually written to the log
-- once it finishes. Use 'withPGLogger' or 'withElasticSearchLogger'
-- for that.
runLogT :: Text     -- ^ Application component name to use.
        -> Logger   -- ^ The logging back-end to use.
        -> LogT m a -- ^ The 'LogT' computation to run.
        -> m a
runLogT component logger m = runReaderT (unLogT m) LoggerEnv {
  leLogger = logger
, leComponent = component
, leDomain = []
, leData = []
} -- We can't do synchronisation here, since 'runLogT' can be invoked
  -- quite often from the application (e.g. on every request).

-- | Transform the computation inside a 'LogT'.
mapLogT :: (m a -> n b) -> LogT m a -> LogT n b
mapLogT f = LogT . mapReaderT f . unLogT

-- | @'hoist' = 'mapLogT'@
--
-- @since 0.7.2
instance MFunctor LogT where
    hoist f = mapLogT f

instance MonadTransControl LogT where
#if MIN_VERSION_monad_control(1,0,0)
  type StT LogT m = StT InnerLogT m
  liftWith = defaultLiftWith LogT unLogT
  restoreT = defaultRestoreT LogT
#else
  newtype StT LogT m = StLogT { unStLogT :: StT InnerLogT m }
  liftWith = defaultLiftWith LogT unLogT StLogT
  restoreT = defaultRestoreT LogT unStLogT
#endif
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (LogT m) where
#if MIN_VERSION_monad_control(1,0,0)
  type StM (LogT m) a = ComposeSt LogT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
#else
  newtype StM (LogT m) a = StMLogT { unStMLogT :: ComposeSt LogT m a }
  liftBaseWith = defaultLiftBaseWith StMLogT
  restoreM     = defaultRestoreM unStMLogT
#endif
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadUnliftIO m => MonadUnliftIO (LogT m) where
  withRunInIO inner = LogT $ withRunInIO $ \run -> inner (run . unLogT)
  {-# INLINE withRunInIO #-}

instance MonadBase IO m => MonadLog (LogT m) where
  logMessage level message data_ = LogT . ReaderT $ \logEnv -> liftBase $ do
    time <- getCurrentTime
    logMessageIO logEnv time level message data_

  localData data_ =
    LogT . local (\e -> e { leData = data_ ++ leData e }) . unLogT

  localDomain domain =
    LogT . local (\e -> e { leDomain = leDomain e ++ [domain] }) . unLogT

  getLoggerEnv = LogT ask
