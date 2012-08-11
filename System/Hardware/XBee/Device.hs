{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}

module System.Hardware.XBee.Device (
    -- * Device
    XBee,
    newDevice,
    execute,
    execute',
    -- * Device Interface
    XBeeInterface(..),
    Scheduled(..),
    -- * Actions
    XBeeCmd,
    XBeeCmdAsync,
    -- ** Fire (without response)
    fire,
    -- ** Send (with response)
    send,
    CommandHandler,
    CommandResponse(..),
    FrameCmd(..),
    fetch,
    -- ** Source
    rawInSource,
    ReceivedMessage(..),
    messagesSource,
    modemStatusSource,
    -- * Reexports
    Future,
    liftIO,
    await,
    awaitAny,
    afterUs,
    instantly,
    fasterOf,
    fastestOf
) where

import System.Hardware.XBee.Frame
import System.Hardware.XBee.Command
import Data.List
import Data.Word
import Data.Time.Units
import Control.Concurrent.STM
import Control.Concurrent.Future
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT)
import Control.Applicative
import Control.RequestResponseCorrelator
import Data.SouSiT
import Data.SouSiT.STM
import qualified Data.SouSiT.Trans as T


-- | Answers received to a command sent to the XBee.
data CommandResponse = CRData CommandIn
                     | CRPurged
                     | CRTimeout deriving (Show, Eq)

-- | Handler for the answers to a single command sent to the XBee.
type CommandHandler a = ResponseM CommandResponse a

-- | A command that expects an answer from the XBee.
data FrameCmd a   = FrameCmd (FrameId -> CommandOut) (CommandHandler a)
instance Functor FrameCmd where
    fmap f (FrameCmd a handler) = FrameCmd a (fmap f handler)



-- | Task to be scheduled. Delay in microseconds and the action to execute after the delay.
data Scheduled = Scheduled Int (IO ())

type XBeeCorrelator =  Correlator FrameId CommandResponse

-- | Opaque representation of the locally attached XBee device.
data XBee = XBee { scheduleQueue :: TChan Scheduled,
                   outQueue  :: TChan CommandOut,
                   inQueue   :: TChan CommandIn,
                   correlator :: XBeeCorrelator }


-- | Interface to an XBee. This is 'side' of the xbee that needs to be attached to the
--   actual device. See i.e. the HandleDeviceConnector module.
data XBeeInterface = XBeeInterface {
                    -- | Commands that are sent to the xbee device (serial port).
                    outgoing   :: BasicSource2 IO CommandOut,
                    -- | Commands received from the xbee device (serial port).
                    incoming   :: Sink CommandIn IO (),
                    -- | Actions to be scheduled with the specified delay.
                    --   This is used mainly for timeouts.
                    toSchedule :: BasicSource2 IO Scheduled }

--- | Create a new XBee device along with the corresponding interface.
newDevice :: STM (XBee, XBeeInterface)
newDevice = do
        inQ  <- newTChan
        outQ <- newTChan
        scdQ <- newTChan
        corr <- newCorrelator CRPurged
        let xbee = XBee scdQ outQ inQ corr
        let xif  = mkXif inQ outQ scdQ corr
        return (xbee, xif)

mkXif inQ outQ scdQ corr = XBeeInterface o i s
    where o = tchanSource outQ
          s = tchanSource scdQ
          i = stmSink' $ processIn corr inQ

processIn :: XBeeCorrelator -> TChan CommandIn -> CommandIn -> STM ()
processIn corr subs msg = handle (frameIdFor msg) >> writeTChan subs msg
    where handle (Just frame) = push corr frame (CRData msg)
          handle Nothing = return ()


newtype XBeeCmd a = XBeeCmd { runXBeeCmd :: ReaderT XBee IO a }
    deriving (Monad, MonadIO, MonadReader XBee, Functor, Applicative)

type XBeeCmdAsync a = XBeeCmd (Future a)


-- | Execute an async xbee command.
execute :: XBee -> XBeeCmdAsync a -> IO a
execute x m = runReaderT (runXBeeCmd m) x >>= await

-- | Execute an async xbee command and catches any exception that occured.
execute' :: XBee -> XBeeCmdAsync a -> IO (Either String a)
execute' x m = E.catch (liftM Right $ execute x m) (return . Left . showE)
    where showE :: SomeException -> String
          showE = show


-- | Sends a command without waiting for a response.
-- Use noFrameId if the command supports frames.
fire :: CommandOut -> XBeeCmd ()
fire cmd = liftM outQueue ask >>= enqueue cmd
    where enqueue cmd q = liftIO $ atomically $ writeTChan q cmd


-- | Sends a command.
send :: TimeUnit time => time -> FrameCmd a -> XBeeCmdAsync a
send tmo cmd = do
        x   <- ask
        fut <- liftIO $ atomically $ sendCommand x tmo cmd
        return $ stmFuture fut

sendCommand :: TimeUnit time => XBee -> time -> FrameCmd a -> STM (STM a)
sendCommand x tmo (FrameCmd cmd h) = do
        (frame, future, feedFun) <- request (correlator x) h
        writeTChan (outQueue x) (cmd frame)
        writeTChan (scheduleQueue x) $ Scheduled tmoUs (atomically $ feedFun CRTimeout)
        return future
    where tmoUs = fromIntegral $ toMicroseconds tmo


-- | Source for all incoming commands from the XBee. This includes replies to framed command
-- that are also handled by a CommandHandler from send.
rawInSource :: BasicSource2 XBeeCmd CommandIn
rawInSource = BasicSource2 first
    where first s = liftM inQueue ask >>= liftIO . atomically . dupTChan >>= (flip step) s
          step :: TChan a -> Sink a XBeeCmd r -> XBeeCmd (Sink a XBeeCmd r)
          step q (SinkCont next _) = dequeue q >>= next >>= step q
          step q done = return done

dequeue :: TChan a -> XBeeCmd a
dequeue = liftIO . atomically . readTChan

-- | An incoming message (abstracts over Receive64 and Receive16)
data ReceivedMessage = ReceivedMessage { sender :: XBeeAddress,
                                         signal :: SignalStrength,
                                         addressBroadcast :: Bool,
                                         panBroadcast :: Bool,
                                         messageBody :: [Word8] }

-- Transformer for CommandIn into ReceivedMessage.
commandInToReceivedMessage :: Transform CommandIn ReceivedMessage
commandInToReceivedMessage = T.filterMap step
    where step (Receive16 se si ab pb d) = Just $ ReceivedMessage (XBeeAddress16 se) si ab pb d
          step (Receive64 se si ab pb d) = Just $ ReceivedMessage (XBeeAddress64 se) si ab pb d
          step _ = Nothing

-- | Source for all messages received from remote XBees (Receive16 and Receive64).
messagesSource :: BasicSource XBeeCmd ReceivedMessage
messagesSource = rawInSource $= commandInToReceivedMessage

-- | Source for modem status updates.
modemStatusSource ::  BasicSource XBeeCmd ModemStatus
modemStatusSource = rawInSource $= commandInToModemStatus

commandInToModemStatus :: Transform CommandIn ModemStatus
commandInToModemStatus = T.filterMap step
    where step (ModemStatusUpdate s) = Just s
          step _ = Nothing
