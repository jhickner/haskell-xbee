module System.Hardware.XBee.Command (
    -- * FrameId
    FrameId,
    noFrameId,
    frameId,
    nextFrame,
    -- * Address
    Address64(..),
    Address16(..),
    broadcastAddress,
    disabledAddress,
    -- * Command
    CommandName,
    CommandStatus(..),
    ModemStatus(..),
    DisableAck,
    BroadcastPan,
    TransmitStatus(..),
    Commands(..)
) where

import Data.Word
import Data.Serialize
import qualified Data.ByteString as BS
import Control.Monad


newtype FrameId = FrameId Word8 deriving (Show, Eq)
-- | Don't use a FrameId.
noFrameId = FrameId 0
-- | Initial FrameId.
frameId = FrameId 1
-- | The next FrameId. The ids are looped (after FrameId 255 follows FrameId 1)
nextFrame (FrameId 255) = frameId --overflow
nextFrame (FrameId i)   = FrameId (i+1)
instance Serialize FrameId where
    get = liftM FrameId getWord8
    put (FrameId i) = putWord8 i


-- | Address of an XBee device.
newtype Address64 = Address64 Word64 deriving (Show, Eq)
-- | Address for broadcasts to all XBee devices.
broadcastAddress = Address64 0xFFFF
instance Serialize Address64 where
    get = liftM Address64 getWord64be
    put (Address64 a) = putWord64be a

-- | 16-bit network address of an XBee device.
newtype Address16 = Address16 Word16 deriving (Show, Eq)
-- | Address to disable 16-bit addressing.
disabledAddress  = Address16 0xFFFE
instance Serialize Address16 where
    get = liftM Address16 getWord16be
    put (Address16 a) = putWord16be a



data ModemStatus = HardwareReset
                 | WatchdogTimerReset
                 | Associated
                 | Disassociated
                 | SyncLost
                 | CoordinatorRealignment
                 | CoordinatorStarted deriving (Enum, Show, Eq, Bounded)

newtype CommandName = CommandName (Word8, Word8) deriving (Eq)
instance Show CommandName where
    show (CommandName (c1, c2)) = "Command " ++ [toChar c1,toChar c2]
        where toChar = toEnum . fromIntegral

data CommandStatus = CmdOK
                   | CmdError
                   | CmdInvalidCommand
                   | CmdInvalidParameter deriving (Enum, Show, Eq, Bounded)

-- | Disable acknowledgment.
type DisableAck = Bool
-- | Send packet with broadcast Pan ID.
type BroadcastPan = Bool

-- | Status/Result of a transmitted packet.
data TransmitStatus =
                      -- | Packet delivered to the remote XBee.
                      TransmitSuccess
                      -- | All retries are expired and no ACK is received.
                      -- Does not apply to broadcasts.
                    | TransmitNoAck
                      -- | Clear Channel Assessment failure. The channel was not available for
                      -- transmission within the retries (normally two retries).
                    | TransmitCcaFailure
                      -- |  Coordinator times out of an indirect transmission.
                      -- Timeout is defined as (2.5 x SP (Cyclic Sleep Period) parameter value).
                      -- Does not apply to broadcasts.
                    | TransmitPurged deriving (Enum, Show, Eq, Bounded)

-- | Commands to/from the XBee
data Commands = ModemStatusUpdate ModemStatus 
              | ATCommand FrameId CommandName [Word8]
              | ATQueueCommand FrameId CommandName
              | ATCommandResponse FrameId CommandName CommandStatus [Word8]
              | RemoteATCommand64 FrameId Address64 Bool CommandName [Word8]
              | RemoteATCommand16 FrameId Address16 Bool CommandName [Word8]
              | RemoteATCommandResponse FrameId Address64 Address16 CommandName CommandStatus [Word8]
              | Transmit64 FrameId Address64 DisableAck BroadcastPan [Word8]
              | Transmit16 FrameId Address16 DisableAck BroadcastPan [Word8]
              | TransmitResponse FrameId TransmitStatus

instance Serialize CommandName where
    get = liftM CommandName $ liftM2 (,) getWord8 getWord8
    put (CommandName (b1,b2)) = putWord8 b1 >> putWord8 b2


getEnumWord8 :: (Enum e) => Get e
getEnumWord8 = liftM (toEnum . fromIntegral) getWord8
putEnumWord8 :: (Enum e) => e -> Put
putEnumWord8 = putWord8 . fromIntegral . fromEnum
instance Serialize ModemStatus where
    get = getEnumWord8
    put = putEnumWord8
instance Serialize CommandStatus where
    get = getEnumWord8
    put = putEnumWord8
instance Serialize TransmitStatus where
    get = getEnumWord8
    put = putEnumWord8