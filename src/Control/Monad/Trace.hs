module Control.Monad.Trace
  ( MonadTrace(..)
  , MonadTraceSink(..)
  , TraceT
  , TraceKey
  , TraceChunk
  , TraceSink
  , TraceFilter
  , runTraceT
  , mapTraceT
  , withTraceKey
  , withTraceFilter
  , traceLog
  , traceLogLn
  , traceLogStr
  , traceLogStrLn
  , traceIndent
  , newTraceKey
  ) where

import            Control.Applicative
import            Control.Monad (MonadPlus)
import            Control.Monad.Base
import            Control.Monad.Catch
import            Control.Monad.Except
import            Control.Monad.Reader
import            Control.Monad.Trans
import            Control.Monad.Trans.Control
import            Data.Default
import qualified  Data.ByteString.Char8 as BS
import            Data.Time

type TraceChunk = BS.ByteString
type TraceKey = BS.ByteString
type TraceSink m = TraceKey -> [TraceChunk] -> m ()
type TraceFilter = [TraceChunk] -> [TraceChunk]

data TraceSettings = TraceSettings {
    traceKey :: TraceKey
  , traceFilter :: TraceFilter
  }

instance Default TraceSettings where
  def = TraceSettings (BS.pack "") id

class Monad m => MonadTraceSink m where
  traceSink :: TraceSink m

class MonadTraceSink m => MonadTrace m where
  traceLogs :: [TraceChunk] -> m ()
  withTraceSettings :: (TraceSettings -> TraceSettings) -> m a -> m a

withTraceKey :: MonadTrace m => (TraceKey -> TraceKey) -> m a -> m a
withTraceKey f =
  withTraceSettings $ \settings -> settings { traceKey = f $ traceKey settings }

withTraceFilter :: MonadTrace m => (TraceFilter -> TraceFilter) -> m a -> m a
withTraceFilter f =
  withTraceSettings $ \settings -> settings { traceFilter = f $ traceFilter settings }

newtype TraceT m a = TraceT { unTraceT :: ReaderT TraceSettings m a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

deriving instance MonadBase b m => MonadBase b (TraceT m)
deriving instance MonadError e m => MonadError e (TraceT m)

instance MonadBaseControl b m => MonadBaseControl b (TraceT m) where
  type StM (TraceT m) a = StM m a

  liftBaseWith f = TraceT $ liftBaseWith $ \runInBase ->
                    f (\(TraceT m) -> runInBase m)

  restoreM stm = TraceT (restoreM stm)

instance MonadTrans TraceT where
  lift = TraceT . lift

instance MonadTraceSink m => MonadTraceSink (TraceT m) where
  traceSink key logs = TraceT $ lift $ traceSink key logs

instance MonadTraceSink m => MonadTrace (TraceT m) where
  traceLogs logs = TraceT $ do
    settings <- ask
    lift $ traceFunc settings logs

  withTraceSettings f = TraceT . local f . unTraceT

traceFunc :: MonadTraceSink m => TraceSettings -> [TraceChunk] -> m ()
traceFunc (TraceSettings {..}) = traceSink traceKey . traceFilter

runTraceT :: (MonadCatch m, MonadTraceSink m) => TraceT m a -> m a
runTraceT traceT =
    runReaderT (unTraceT traceT) def `catch` logError
  where
    logError e = do
      traceFunc def [BS.pack $ "\n=== Error ===\n" ++ show e]
      throwM (e :: SomeException)

mapTraceT :: (m a -> n b) -> TraceT m a -> TraceT n b
mapTraceT f = TraceT . mapReaderT f . unTraceT

-- Helpers
--
traceLog :: MonadTrace m => TraceChunk -> m ()
traceLog s = traceLogs [s]

traceLogLn :: MonadTrace m => TraceChunk-> m ()
traceLogLn s = traceLogs [s, BS.pack "\n"]

traceLogStr :: MonadTrace m => String -> m ()
traceLogStr = traceLog . BS.pack

traceLogStrLn :: MonadTrace m => String -> m ()
traceLogStrLn = traceLogLn . BS.pack

traceIndent :: MonadTrace m => Int -> m a -> m a
traceIndent n =
    withTraceFilter addIndent
  where
    addIndent f chunks = f (indentation : chunks)
    indentation = BS.replicate n ' '

newTraceKey :: MonadIO m => BS.ByteString -> BS.ByteString -> m TraceKey
newTraceKey prefix suffix = do
  now <- liftIO getCurrentTime
  let nowStr = formatTime defaultTimeLocale "%F-%T.%q-%Z" now
  pure $ BS.concat [prefix, BS.pack nowStr, suffix]

