module System.Hardware.XBee.DeviceCommand (
    -- * Transmitting data
    transmitBytes,
    transmit,
    transmitNoAck,
    broadcast,
    -- * AT Settings
    atCommand,
    ATTransport(..),
    ATSetting,
    atSetting,
    getAT,
    setAT,
    getLocalAT,
    getRemoteAT,
    setLocalAT,
    setRemoteAT,
    -- * Addressing
    address16,
    address64,
    nodeIdentifierMaxLength,
    nodeIdentifier,
    panId,
    -- * Discover
    associationIndication,
    NodeInformation(..),
    discover,
    discoverTimeout,
    discoveryOption,
    -- * Various
    baudRate,
    deviceType,
    hardwareVersion,
    softwareVersion,
    softwareReset,
    write
) where

import Data.Word
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Time.Units
import Data.SouSiT
import qualified Codec.Binary.UTF8.String as S
import Control.Monad
import Control.Applicative
import System.Hardware.XBee.Device
import System.Hardware.XBee.Command

import Debug.Trace


sendLocal  cmd handler = localTimeout  >>= flip send (FrameCmd cmd handler)
sendRemote cmd handler = remoteTimeout >>= flip send (FrameCmd cmd handler)



-- | Maximum number of bytes that can be transmitted with a single transmit call.
transmitBytes = 100

transmitCmd (XBeeAddress64 a) noack bdcst d f = Transmit64 f a noack bdcst (BS.take transmitBytes d)
transmitCmd (XBeeAddress16 a) noack bdcst d f = Transmit16 f a noack bdcst (BS.take transmitBytes d)

-- | Sends up to 100 bytes to another XBee and requests an ack.
transmit :: XBeeAddress -> ByteString -> XBeeCmdAsync TransmitStatus
transmit to d = sendRemote cmd (liftM handle input)
    where handle (CRData (WeirdTransmitResponse _ r)) = r
          handle _ = TransmitNoAck
          cmd = transmitCmd to False False d

-- | Sends up to 100 bytes to another XBee without requesting an acknowledgement. There
--   is no way to tell whether the transmission succeeded or not.
transmitNoAck :: XBeeAddress -> ByteString -> XBeeCmd ()
transmitNoAck to d = fire $ transmitCmd to True False d noFrameId

-- | Broadcasts up to 100 bytes to all XBees within the same network.
broadcast :: ByteString -> XBeeCmd ()
broadcast d = fire $ transmitCmd (XBeeAddress64 broadcastAddress) True True d noFrameId


failOnLeft :: Monad m => Either String a -> m a
failOnLeft (Left err) = fail err
failOnLeft (Right v)  = return v

atCommand :: (Serialize i, Serialize o) => Char -> Char -> i -> ATTransport -> XBeeCmdAsync o
atCommand c1 c2 i LocalAT = sendLocal cmd (liftM handle input >>= failOnLeft)
    where cmd f = ATCommand f (commandName c1 c2) (encode i)
          handle (CRData (ATCommandResponse _ _ CmdOK d)) = decode d
          handle (CRData (ATCommandResponse _ _ status _)) = Left $ "Failed: " ++ show status
          handle _ = Left "Timeout"
atCommand c1 c2 i (RemoteAT a64 ac) = sendRemote cmd (liftM handle input >>= failOnLeft)
    where cmd f = RemoteATCommand64 f a64 ac (commandName c1 c2) (encode i)
          handle (CRData (RemoteATCommandResponse _ _ _ _ CmdOK d)) = decode d
          handle (CRData (RemoteATCommandResponse _ _ _ _ status _)) = Left $ "Failed: " ++ show status
          handle _ = Left "Timeout"

data ATTransport = LocalAT | RemoteAT Address64 ApplyChanges

data ATSetting a = ATSetting
    { getAT :: ATTransport -> XBeeCmdAsync a
    , setAT :: a -> ATTransport -> XBeeCmdAsync ()
    }

getLocalAT s = getAT s LocalAT
getRemoteAT s a64 ac = getAT s $ RemoteAT a64 ac
setLocalAT s v = setAT s v LocalAT
setRemoteAT s v a64 ac = setAT s v $ RemoteAT a64 ac

mapAtSetting :: ATSetting a -> (a -> b) -> (b -> a) -> ATSetting b
mapAtSetting (ATSetting gf sf) f1 f2 = 
    ATSetting (\t -> liftM (fmap f1) (gf t)) 
              (\a t -> sf (f2 a) t)

atSetting :: Serialize a => Char -> Char -> ATSetting a
atSetting c1 c2 = ATSetting (atCommand c1 c2 ()) (atCommand c1 c2)

-- | 16-bit network address of the xbee.
address16 :: ATSetting Address16
address16 = atSetting 'M' 'Y'

-- | Immutable 64-bit network address of the xbee.
address64 :: ATTransport -> XBeeCmdAsync Address64
address64 t = combine <$> getAT (a64p 'L') t <*> getAT (a64p 'H') t
    where
        a64p :: Char -> ATSetting Word32
        a64p = atSetting 'S'
        combine lf hf = do
            l <- lf
            h <- hf
            return $ Address64 $ fromIntegral l .|. fromIntegral h `shift` 32

newtype RawData = RawData BS.ByteString
instance Serialize RawData where
    get = liftM RawData (remaining >>= getByteString)
    put (RawData bs) = putByteString bs

newtype Utf8String = Utf8String { unpackUtf8 :: String }
instance Serialize Utf8String where
    get = liftM (Utf8String . S.decode . BS.unpack) (remaining >>= getByteString)
    put = putByteString . BS.pack . S.encode . unpackUtf8

-- | Shortens the string (removes at the end) to make sure that it can be encoded in utf-8
--   in at most n characters.
takeBytes :: Int -> String -> String
takeBytes n = process . take n
    where process [] = []
          process i = let e = S.encode i in
                if length e > n then process $ init i
                else i


nodeIdentifierMaxLength = 20

-- | String to identify a node.
nodeIdentifier :: ATSetting String
nodeIdentifier = mapAtSetting (atSetting 'N' 'I') unpackUtf8 enc
    where enc = Utf8String . takeBytes nodeIdentifierMaxLength

-- | Used to set and read the PAN (Personal Area Network) ID of the xbee.
--   Only modules with matching PAN IDs can communicate with each other
panId :: ATSetting Word64
panId = atSetting 'I' 'D'

associationIndication :: ATTransport -> XBeeCmdAsync AssociationIndication
associationIndication = atCommand 'A' 'I' ()

data NodeInformation = NodeInformation 
    { nodeNetworkAddress :: Word16
    , nodeAddress64 :: Address64
    , nodeId :: String
    -- , nodeParentNetworkAddress :: Address16
    -- , nodeType :: NodeType
    -- , nodeStatus :: Word8
    -- , nodeProfileId :: Word16
    -- , nodeManufacturerId :: Word16 
    -- , nodeDeviceType :: NodeDeviceType
    } deriving (Show, Eq)
instance Serialize NodeInformation where
    get = NodeInformation <$> get <*> get
                          <*> liftM (takeWhile (/= '\0') . unpackUtf8) get -- TODO: figure out how to takeWhile
    put (NodeInformation nmy n64 nid) = put nmy >> put n64 >> put nid


-- | Discovers all nodes on that can be reached from this xbee. Does not include this
--   xbee.
--   All modules on the current operating channel and PAN ID are found.
discover :: TimeUnit time => time -> XBeeCmdAsync [NodeInformation]
discover tmo = setAT discoverTimeout (convertUnit tmo) LocalAT >>= await >>
               setAT discoveryOption AppendDD LocalAT >>= await >>
               localTimeout >>= discover' . tmo'
    where tmo' lcl = convertUnit tmo + lcl

discover' :: TimeUnit time => time -> XBeeCmdAsync [NodeInformation]
discover' tmout = send tmout $ FrameCmd cmd (input >>= handle [])
    where cmd f = ATCommand f (commandName 'N' 'D') BS.empty
          handle :: [NodeInformation] -> CommandResponse -> CommandHandler [NodeInformation]
          handle soFar (CRData (ATCommandResponse _ _ CmdOK d))
                | BS.null d = return soFar
                | otherwise = do ni <- failOnLeft $ decode d
                                 input >>= handle (ni:soFar)
          handle _     (CRData (ATCommandResponse _ _ status _)) = fail $ "Failed: " ++ show status
          --handle _ _ = fail "Timeout"
          handle soFar _ = traceShow soFar $ return soFar

-- | Set/Get the node discover timeout.
discoverTimeout :: ATSetting Millisecond
discoverTimeout = mapAtSetting (atSetting 'N' 'T') convertToMs (max 1 . min 252 . convertFromMs)
    where convertToMs :: Word16 -> Millisecond
          convertToMs b = fromIntegral (b * 100)
          convertFromMs :: Millisecond -> Word16
          convertFromMs v = round $ (fromIntegral (toMicroseconds v) / 100000 :: Double)

-- | Controls if a node discover dows return the sender as well (only used internally).
discoveryOption :: ATSetting DiscoveryOption
discoveryOption = atSetting 'N' 'O'


baudRate :: ATSetting BaudRate
baudRate = atSetting 'B' 'D'

deviceType :: ATSetting NodeDeviceType
deviceType = atSetting 'D' 'D'

-- | Hardware version of the xbee.
hardwareVersion :: ATTransport -> XBeeCmdAsync Word16
hardwareVersion = atCommand 'H' 'V' ()

softwareVersion :: ATTransport -> XBeeCmdAsync Word16
softwareVersion = atCommand 'V' 'R' ()

-- | Used to force a software reset on the RF module. The reset simutates powering off
-- and then on again the xbee module.
softwareReset :: ATTransport -> XBeeCmdAsync ()
softwareReset = atCommand 'F' 'R' ()

-- | Write parameters to non-volatile memory on the RF module.
write :: ATTransport -> XBeeCmdAsync ()
write = atCommand 'W' 'R' ()
