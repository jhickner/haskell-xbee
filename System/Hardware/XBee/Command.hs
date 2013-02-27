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
    SignalStrength,
    -- * Command
    CommandName,
    commandName,
    CommandStatus(..),
    ModemStatus(..),
    DisableAck,
    BroadcastPan,
    dBm,
    fromDbm,
    TransmitStatus(..),
    ApplyChanges,
    CommandIn(..),
    CommandOut(..),
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

-- | Disable acknowledgment.
type DisableAck = Bool
-- | Send packet with broadcast Pan ID.
type BroadcastPan = Bool
type AddressBroadcast = Bool
type PanBroadcast = Bool

-- | Status/Result of a transmitted packet.
data TransmitStatus 
    -- | Packet delivered to the remote XBee.
    = TransmitSuccess
    -- | All retries are expired and no ACK is received.
    -- Does not apply to broadcasts.
    | TransmitNoAck
    -- | Clear Channel Assessment failure. The channel was not available for
    -- transmission within the retries (normally two retries).
    | TransmitCcaFailure
    -- |  Coordinator times out of an indirect transmission.
    -- Timeout is defined as (2.5 x SP (Cyclic Sleep Period) parameter value).
    -- Does not apply to broadcasts.
    | TransmitPurged 
    deriving (Enum, Show, Eq, Bounded)


newtype SignalStrength = SignalStrength Word8 deriving (Eq)
-- | Signal strength in dBm (negative value, 0 is best).
dBm :: SignalStrength -> Int
dBm (SignalStrength s) = negate $ fromIntegral s
-- | Creates a signal strength. Values >0 and <255 will be truncated to 0 resp. 255.
fromDbm :: Int -> SignalStrength
fromDbm v | v > 0      = SignalStrength 0
          | v < (-255) = SignalStrength 255
          | otherwise  = SignalStrength (fromIntegral (-v))
instance Show SignalStrength where
    show = (++ " dBm") . show . dBm
instance Ord SignalStrength where
    a <= b = dBm a <= dBm b

-- | Whether to apply to changes on the remote. If set to False then an AC command must be sent.
type ApplyChanges = Bool

{-# ANN module "HLint: ignore Use record patterns" #-}
-- | Commands or responses sent from the XBee to the computer.
data CommandIn 
    = ModemStatusUpdate ModemStatus
    | ATCommandResponse FrameId CommandName CommandStatus ByteString
    | RemoteATCommandResponse FrameId Address64 Address16 CommandName CommandStatus ByteString
    | TransmitResponse FrameId TransmitStatus
    | Receive64 Address64 SignalStrength AddressBroadcast PanBroadcast ByteString
    | Receive16 Address16 SignalStrength AddressBroadcast PanBroadcast ByteString
    deriving (Show, Eq)

-- | Commands sent from to computer to the XBee.
data CommandOut 
    = ATCommand FrameId CommandName ByteString
    | ATQueueCommand FrameId CommandName ByteString
    | RemoteATCommand64 FrameId Address64 ApplyChanges CommandName ByteString
    | RemoteATCommand16 FrameId Address16 ApplyChanges CommandName ByteString
    | Transmit64 FrameId Address64 DisableAck BroadcastPan ByteString
    | Transmit16 FrameId Address16 DisableAck BroadcastPan ByteString
    deriving (Show, Eq)

class FrameIdContainer c where
    frameIdFor :: c -> Maybe FrameId
instance FrameIdContainer CommandIn where
    frameIdFor (ModemStatusUpdate _) = Nothing
    frameIdFor (ATCommandResponse f _ _ _) = Just f
    frameIdFor (RemoteATCommandResponse f _ _ _ _ _) = Just f
    frameIdFor (TransmitResponse f _) = Just f
    frameIdFor (Receive64 _ _ _ _ _) = Nothing
    frameIdFor (Receive16 _ _ _ _ _) = Nothing
instance FrameIdContainer CommandOut where
    frameIdFor (ATCommand f _ _) = Just f
    frameIdFor (ATQueueCommand f _ _) = Just f
    frameIdFor (RemoteATCommand64 f _ _ _ _) = Just f
    frameIdFor (RemoteATCommand16 f _ _ _ _) = Just f
    frameIdFor (Transmit64 f _ _ _ _) = Just f
    frameIdFor (Transmit16 f _ _ _ _) = Just f

-- | Serializes an (outgoing) command into a frame.
commandToFrame :: CommandOut -> Frame
commandToFrame = frame . encode

-- | Parses a frame into a (incomming) command.
frameToCommand :: Frame -> Either String CommandIn
frameToCommand = decode . frameData


instance Serialize CommandName where
    get = liftM CommandName $ liftM2 (,) getWord8 getWord8
    put (CommandName (b1,b2)) = putWord8 b1 >> putWord8 b2
instance Serialize SignalStrength where
    get = liftM SignalStrength getWord8
    put (SignalStrength s) = putWord8 s

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
instance Serialize DiscoveryOption where
    get = getEnumWord8
    put = putEnumWord8
instance Serialize NodeType where
    get = getEnumWord8
    put = putEnumWord8
instance Serialize BaudRate where
    get = liftM (toEnum . fromIntegral) getWord32be
    put = putWord32be . fromIntegral . fromEnum

instance Serialize CommandIn where
    put (ModemStatusUpdate s) = putWord8 0x8A >> put s
    put (ATCommandResponse f cmd st d) = putWord8 0x88 >> put f >> put cmd >> put st >> putByteString d
    put (RemoteATCommandResponse f a64 a16 cmd st d) = putWord8 0x97 >> put f >> put a64 >> put a16 >>
        put cmd >> put st >> putByteString d
    put (TransmitResponse f st) = putWord8 0x89 >> put f >> put st
    put (Receive64 a ss adbc panbc d) = putWord8 0x80 >> put a >> put ss >> put opts >> putByteString d
        where opts = bitOpt receiveBitAddress adbc .|. bitOpt receiveBitPan panbc
    put (Receive16 a ss adbc panbc d) = putWord8 0x81 >> put a >> put ss >> put opts >> putByteString d
        where opts = bitOpt receiveBitAddress adbc .|. bitOpt receiveBitPan panbc
    get = getWord8 >>= getCmdIn where
        getCmdIn 0x8A = liftM ModemStatusUpdate get
        getCmdIn 0x88 = liftM4 ATCommandResponse get get get getTillEnd
        getCmdIn 0x97 = RemoteATCommandResponse <$> get <*> get <*> get <*> get <*> get <*> getTillEnd
        getCmdIn 0x89 = liftM2 TransmitResponse get get
        getCmdIn 0x80 = create <$> get <*> get <*> getWord8 <*> getTillEnd
            where create adr ss opts =
                    Receive64 adr ss (testBit opts receiveBitAddress) (testBit opts receiveBitPan)
        getCmdIn 0x81 = create <$> get <*> get <*> getWord8 <*> getTillEnd
            where create adr ss opts =
                    Receive16 adr ss (testBit opts receiveBitAddress) (testBit opts receiveBitPan)
        getCmdIn o    = fail $ "undefined XBee->PC command " ++ show o
receiveBitAddress = 1
receiveBitPan = 2

instance Serialize CommandOut where
    put (ATCommand f cmd d) = putWord8 0x08 >> put f >> put cmd >> putByteString d
    put (ATQueueCommand f cmd d) = putWord8 0x09 >> put f >> put cmd >> putByteString d
    put (RemoteATCommand64 f adr ac cmd d) = putWord8 0x17 >> put f >> put adr >>
            putWord16be 0xFFFE >> put (bitOpt remoteAtCommandBitAC ac) >> put cmd >> putByteString d
    put (RemoteATCommand16 f adr ac cmd d) = putWord8 0x17 >> put f >> putWord64be 0xFFFF >>
            put adr >> put (bitOpt remoteAtCommandBitAC ac) >> put cmd >> putByteString d
    put (Transmit64 f adr dack bc d) = putWord8 0x00 >> put f >> put adr >> put opts >> putByteString d
        where opts = bitOpt transmitBitDisableAck dack .|. bitOpt transmitBitPanBroadcast bc
    put (Transmit16 f adr dack bc d) = putWord8 0x01 >> put f >> put adr >> put opts >> putByteString d
        where opts = bitOpt transmitBitDisableAck dack .|. bitOpt transmitBitPanBroadcast bc
    get = getWord8 >>= getCmdOut where
        getCmdOut 0x08 = ATCommand <$> get <*> get <*> getTillEnd
        getCmdOut 0x09 = ATQueueCommand <$> get <*> get <*> getTillEnd
        getCmdOut 0x17 = do
                f     <- get
                adr64 <- get
                adr16 <- get
                ac    <- liftM (`testBit` remoteAtCommandBitAC) getWord8
                cmd   <- get
                d     <- getTillEnd
                return (if   adr64 /= broadcastAddress then RemoteATCommand64 f adr64 ac cmd d
                        else RemoteATCommand16 f adr16 ac cmd d)
        getCmdOut 0x00 = create <$> get <*> get <*> getWord8 <*> getTillEnd
            where create f adr opts =
                    Transmit64 f adr (testBit opts transmitBitDisableAck) (testBit opts transmitBitPanBroadcast)
        getCmdOut 0x01 = create <$> get <*> get <*> getWord8 <*> getTillEnd
            where create f adr opts =
                    Transmit16 f adr (testBit opts transmitBitDisableAck) (testBit opts transmitBitPanBroadcast)
        getCmdOut o    = fail $ "undefined PC->XBee command " ++ show o
remoteAtCommandBitAC = 1
transmitBitDisableAck = 1
transmitBitPanBroadcast = 3


bitOpt :: Int -> Bool -> Word8
bitOpt i b = if b then bit i else 0

getTillEnd :: Get ByteString
getTillEnd = remaining >>= getBytes
