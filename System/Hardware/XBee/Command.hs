module System.Hardware.XBee.Command (
    -- * FrameId
    FrameId,
    noFrameId,
    frameId,
    nextFrame,
    frameIdFor,
    -- * Address
    XBeeAddress(..),
    Address64(..),
    Address16(..),
    broadcastAddress,
    disabledAddress,
    -- * Discover
    NodeType(..),
    NodeDeviceType(..),
    DiscoveryOption(..),
    AssociationIndication(..),
    -- * Various
    BaudRate(..),
    -- * Command
    CommandName,
    commandName,
    CommandStatus(..),
    ModemStatus(..),
    ApplyChanges,
    CommandIn(..),
    CommandOut(..),
    DeliveryStatus,
    DiscoveryStatus,
    -- * Conversion to/from frame
    commandToFrame,
    frameToCommand
) where

import Numeric
import Data.Word
import Data.Bits
import Data.Circular
import Data.Maybe (fromJust, fromMaybe)
import Data.Serialize
import Data.Tuple (swap)
import Data.ByteString (ByteString)
import Control.Monad
import Control.Applicative
import System.Hardware.XBee.Frame


newtype FrameId = FrameId Word8 deriving (Show, Eq, Ord)
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
instance Circular FrameId where
    initial = frameId
    next = nextFrame


-- | Address of an XBee device.
newtype Address64 = Address64 Word64 deriving (Eq)
-- | Address for broadcasts to all XBee devices.
broadcastAddress = Address64 0xFFFF
instance Show Address64 where
    show (Address64 a) = "Address64 0x" ++ showHex a ""
instance Serialize Address64 where
    get = liftM Address64 getWord64be
    put (Address64 a) = putWord64be a

-- | 16-bit network address of an XBee device.
newtype Address16 = Address16 Word16 deriving (Eq)
-- | Address to disable 16-bit addressing.
disabledAddress  = Address16 0xFFFE
instance Show Address16 where
    show (Address16 a) = "Address16 0x" ++ showHex a ""
instance Serialize Address16 where
    get = liftM Address16 getWord16be
    put (Address16 a) = putWord16be a

-- | A 64- or 16-bit network address of an XBee device.
data XBeeAddress 
    = XBeeAddress64 Address64
    | XBeeAddress16 Address16
    deriving (Show, Eq)


data DiscoveryOption
    = NoDiscoveryOption
    | AppendDD
    | IncludeSelf
    | AppendDDAndIncludeSelf
    deriving (Show, Eq, Enum)

data AssociationIndication 
    = JoinSucceeded
    | NoPANsFound
    | NoValidPANsFound
    | JoinFailedNotAllowed
    | NoJoinableBeaconsFound
    | UnexpectedState
    | JoinFailed
    | CoordinatorStartFailed
    | CheckingForExistingCoordinator
    | LeaveFailed
    | JoinFailedDeviceDidNotRespond
    | SecureJoinFailedUnsecuredKey
    | SecureJoinFailedKeyNotReceived
    | SecureJoinFailedIncorrectPreconfiguredLinkKey
    | ScanningForNetwork
    | UnknownIndication
    deriving (Show, Eq)

associationTable :: [(Word8, AssociationIndication)]
associationTable = 
    [ (0x00, JoinSucceeded) 
    , (0x21, NoPANsFound)
    , (0x22, NoValidPANsFound)
    , (0x23, JoinFailedNotAllowed)
    , (0x24, NoJoinableBeaconsFound)
    , (0x25, UnexpectedState)
    , (0x27, JoinFailed)
    , (0x2A, CoordinatorStartFailed)
    , (0x2B, CheckingForExistingCoordinator)
    , (0x2C, LeaveFailed)
    , (0xAB, JoinFailedDeviceDidNotRespond)
    , (0xAC, SecureJoinFailedUnsecuredKey)
    , (0xAD, SecureJoinFailedKeyNotReceived)
    , (0xAF, SecureJoinFailedIncorrectPreconfiguredLinkKey)
    , (0xFF, ScanningForNetwork)
    ]

instance Serialize AssociationIndication where
    get = liftM (fromMaybe UnknownIndication . flip lookup associationTable) getWord8
    put = putWord8 . fromJust . flip lookup (map swap associationTable) 

data NodeType
    = Coordinator
    | Router
    | Endpoint
    deriving (Show, Eq, Enum)

data NodeDeviceType
    = UnknownDeviceType
    | ConnectPortX8Gateway
    | ConnectPortX4Gateway
    | ConnectPortX2Gateway
    | RS232Adapter
    | RS485Adapter
    | XBeeSensorAdapter
    | WallRouter
    | DigitalIOAdapter
    | AnalogIOAdapter
    | XStick
    | SmartPlug
    | XBeeLargeDisplay
    | XBeeSmallDisplay
    deriving (Show, Eq)

deviceTypeTable :: [(Word32, NodeDeviceType)]
deviceTypeTable = 
    [ (0x30000, UnknownDeviceType)
    , (0x30001, ConnectPortX8Gateway)
    , (0x30002, ConnectPortX4Gateway)
    , (0x30003, ConnectPortX2Gateway)
    , (0x30005, RS232Adapter)
    , (0x30006, RS485Adapter)
    , (0x30007, XBeeSensorAdapter)
    , (0x30008, WallRouter)
    , (0x3000A, DigitalIOAdapter)
    , (0x3000B, AnalogIOAdapter)
    , (0x3000C, XStick)
    , (0x3000F, SmartPlug)
    , (0x30011, XBeeLargeDisplay)
    , (0x30012, XBeeSmallDisplay)
    ]

instance Serialize NodeDeviceType where
    get = liftM (fromMaybe UnknownDeviceType . flip lookup deviceTypeTable) getWord32be
    put = putWord32be . fromJust . flip lookup (map swap deviceTypeTable) 

data BaudRate
    = B1200
    | B2400
    | B4800
    | B9600
    | B19200
    | B38400
    | B57600
    | B115200
    deriving (Show, Eq, Enum)

data ModemStatus 
    = HardwareReset
    | WatchdogTimerReset
    | Associated
    | Disassociated
    | SyncLost
    | CoordinatorRealignment
    | CoordinatorStarted 
    deriving (Enum, Show, Eq, Bounded)

newtype CommandName = CommandName (Word8, Word8) deriving (Eq)
instance Show CommandName where
    show (CommandName (c1, c2)) = "Command " ++ [toChar c1,toChar c2]
        where toChar = toEnum . fromIntegral
commandName :: Char -> Char -> CommandName
commandName a b = CommandName (c2w a, c2w b)
    where c2w = fromIntegral . fromEnum

data CommandStatus 
    = CmdOK
    | CmdError
    | CmdInvalidCommand
    | CmdInvalidParameter 
    deriving (Enum, Show, Eq, Bounded)


-- | Whether to apply to changes on the remote. If set to False then an AC command must be sent.
type ApplyChanges = Bool

{-# ANN module "HLint: ignore Use record patterns" #-}
-- | Commands or responses sent from the XBee to the computer.
data CommandIn 
    = ModemStatusUpdate ModemStatus
    | ATCommandResponse FrameId CommandName CommandStatus ByteString
    | TransmitStatus FrameId Address16 RetryCount DeliveryStatus DiscoveryStatus
    | Receive Address64 Address16 ReceiveOptions ByteString
    | RemoteATCommandResponse FrameId Address64 Address16 CommandName CommandStatus ByteString
    deriving (Show, Eq)

type ReceiveOptions = Word8
{-
0x01 - Packet Acknowledged
0x02 - Packet was a broadcast packet
0x20 - Packet encrypted with APS encryption
0x40 - Packet was sent from an end device (if known) 
Note: Option values can be combined. For example, a
0x40 and a 0x01 will show as a 0x41. Other possible values 0x21, 0x22, 0x41, 0x42, 0x60, 0x61, 0x62.
-}


type RetryCount = Word8
type DeliveryStatus = Word8
{-
0x00 = Success
0x01 = MAC ACK Failure
0x02 = CCA Failure
0x15 = Invalid destination
endpoint
0x21 = Network ACK Failure
0x22 = Not Joined to Network
0x23 = Self-addressed
0x24 = Address Not Found
0x25 = Route Not Found
0x26 = Broadcast source failed to hear a neighbor relay the message
0x2B = Invalid binding table index
0x2C = Resource error lack of free buffers, timers, etc. 
0x2D = Attempted broadcast with APS transmission 
0x2E = Attempted unicast with APS transmission, but EE=0
0x32 = Resource error lack of free buffers, timers, etc. 0x74 = Data payload too large
-}

type DiscoveryStatus = Word8
{-
0x00 = No Discovery Overhead
0x01 = Address Discovery 
0x02 = Route Discovery 
0x03 = Address and Route 
0x40 = Extended Timeout Discovery
-}



-- | Commands sent from to computer to the XBee.
data CommandOut 
    = ATCommand FrameId CommandName ByteString
    | ATQueueCommand FrameId CommandName ByteString
    | Transmit64 FrameId Address64 ByteString
    | Transmit16 FrameId Address16 ByteString
    | RemoteATCommand64 FrameId Address64 ApplyChanges CommandName ByteString
    | RemoteATCommand16 FrameId Address16 ApplyChanges CommandName ByteString
    deriving (Show, Eq)

class FrameIdContainer c where
    frameIdFor :: c -> Maybe FrameId
instance FrameIdContainer CommandIn where
    frameIdFor (ModemStatusUpdate _) = Nothing
    frameIdFor (ATCommandResponse f _ _ _) = Just f
    frameIdFor (TransmitStatus f _ _ _ _) = Just f
    frameIdFor (Receive _ _ _ _) = Nothing
    frameIdFor (RemoteATCommandResponse f _ _ _ _ _) = Just f
instance FrameIdContainer CommandOut where
    frameIdFor (ATCommand f _ _) = Just f
    frameIdFor (ATQueueCommand f _ _) = Just f
    frameIdFor (Transmit64 f _ _) = Just f
    frameIdFor (Transmit16 f _ _) = Just f
    frameIdFor (RemoteATCommand64 f _ _ _ _) = Just f
    frameIdFor (RemoteATCommand16 f _ _ _ _) = Just f

-- | Serializes an (outgoing) command into a frame.
commandToFrame :: CommandOut -> Frame
commandToFrame = frame . encode

-- | Parses a frame into a (incomming) command.
frameToCommand :: Frame -> Either String CommandIn
frameToCommand = decode . frameData


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
instance Serialize DiscoveryOption where
    get = getEnumWord8
    put = putEnumWord8
instance Serialize NodeType where
    get = getEnumWord8
    put = putEnumWord8
instance Serialize BaudRate where
    get = liftM (toEnum . fromIntegral) getWord32be
    put = putWord32be . fromIntegral . fromEnum

    -- | Receive FrameId Address64 Address16 ReceiveOptions ByteString
instance Serialize CommandIn where
    put (ModemStatusUpdate s) = putWord8 0x8A >> put s
    put (ATCommandResponse f cmd st d) = putWord8 0x88 >> put f >> put cmd >> put st >> putByteString d
    put (TransmitStatus f a16 retries deliv disc) = putWord8 0x8B >> put f >> put a16 >> put retries >>
        put deliv >> put disc
    put (Receive a64 a16 ropts d) = putWord8 0x90 >> put a64 >> put a16 >> put ropts >> putByteString d
    put (RemoteATCommandResponse f a64 a16 cmd st d) = putWord8 0x97 >> put f >> put a64 >> put a16 >>
        put cmd >> put st >> putByteString d
    get = getWord8 >>= getCmdIn where
        getCmdIn 0x8A = liftM ModemStatusUpdate get
        getCmdIn 0x88 = liftM4 ATCommandResponse get get get getTillEnd
        getCmdIn 0x8B = TransmitStatus <$> get <*> get <*> get <*> get <*> get
        getCmdIn 0x97 = RemoteATCommandResponse <$> get <*> get <*> get <*> get <*> get <*> getTillEnd
        getCmdIn 0x90 = Receive <$> get <*> get <*> get <*> getTillEnd
        getCmdIn o    = fail $ "undefined XBee->PC command " ++ show o
receiveBitAddress = 1
receiveBitPan = 2

instance Serialize CommandOut where
    put (ATCommand f cmd d) = putWord8 0x08 >> put f >> put cmd >> putByteString d
    put (ATQueueCommand f cmd d) = putWord8 0x09 >> put f >> put cmd >> putByteString d
    put (Transmit64 f a64 d) = putWord8 0x10 >> put f >> put a64 >> putWord16be 0xFFFE >> 
        putWord8 transmitBroadcastRadius >> putWord8 transmitOptions >> putByteString d
    put (Transmit16 f a16 d) = putWord8 0x10 >> put f >> putWord64be 0xFFFF >> put a16 >> 
        putWord8 transmitBroadcastRadius >> putWord8 transmitOptions >> putByteString d
    put (RemoteATCommand64 f adr ac cmd d) = putWord8 0x17 >> put f >> put adr >>
            putWord16be 0xFFFE >> put (bitOpt remoteAtCommandBitAC ac) >> put cmd >> putByteString d
    put (RemoteATCommand16 f adr ac cmd d) = putWord8 0x17 >> put f >> putWord64be 0xFFFF >>
            put adr >> put (bitOpt remoteAtCommandBitAC ac) >> put cmd >> putByteString d
    get = getWord8 >>= getCmdOut where
        getCmdOut 0x08 = ATCommand <$> get <*> get <*> getTillEnd
        getCmdOut 0x09 = ATQueueCommand <$> get <*> get <*> getTillEnd
        getCmdOut 0x10 = do
                f <- get
                a64 <- get
                a16 <- get
                getWord8 >> getWord8 -- Broadcast Radius and Options
                d <- getTillEnd
                return (if a64 /= broadcastAddress then Transmit64 f a64 d
                        else Transmit16 f a16 d)
        getCmdOut 0x17 = do
                f     <- get
                adr64 <- get
                adr16 <- get
                ac    <- liftM (`testBit` remoteAtCommandBitAC) getWord8
                cmd   <- get
                d     <- getTillEnd
                return (if   adr64 /= broadcastAddress then RemoteATCommand64 f adr64 ac cmd d
                        else RemoteATCommand16 f adr16 ac cmd d)
        getCmdOut o    = fail $ "undefined PC->XBee command " ++ show o
transmitBroadcastRadius = 0
transmitOptions = 0
remoteAtCommandBitAC = 1


bitOpt :: Int -> Bool -> Word8
bitOpt i b = if b then bit i else 0

getTillEnd :: Get ByteString
getTillEnd = remaining >>= getBytes
