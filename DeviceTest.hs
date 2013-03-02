module Main (
    main
) where

import Data.Maybe
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Time.Units
import Data.SouSiT
import Data.SouSiT.Sink
import qualified Data.SouSiT.Trans as T
import qualified Codec.Binary.UTF8.String as S
import System.Hardware.Serial hiding (baudRate, BaudRate(..))
import qualified System.Hardware.Serial as Ser
import System.Hardware.XBee.Command
import System.Hardware.XBee.Device
import System.Hardware.XBee.DeviceCommand
import System.Hardware.XBee.Connector.Handle
import Control.Monad.IO.Class as I
import Control.Concurrent.ThreadGroup
import Control.Applicative
import Print

import Data.Modbus
import qualified Data.ByteString as BS

connector  f s = handleConnector $ openSerialPort f s
--connector  f s = handleConnector $ openSerialPort f s
--connector' f s = slowHandleConnector 30000 $ openSerialPort f s
connector1 = connector "/dev/ttyUSB0" $ defaultSettings { Ser.baudRate = Ser.B115200 }
connector2 = connector "/dev/ttyUSB1" $ defaultSettings

-- 254, BD 4
msg = BS.pack $ readHoldingRegisters 254 100 2

--addrs = [Address64 0x13a2004098b28c, Address64 0x13a2004092d041]
addrs = [Address64 0x13a2004092d041]
a64 = XBeeAddress64 $ head addrs
rt = RemoteAT (head addrs) True

remoteTest = do
    peers <- getPeers
    let rs = concat . replicate 100 $ map ((\a -> RemoteAT a False) . nodeAddress64) peers
    mapM (\r -> address64 r >>= await) rs

{-
transmitTest = do
    let rs = map XBeeAddress64 addrs
    mapM (\a -> transmit a (B.pack "test") >>= await) rs
    --outputRaws
    --transmit (head rs) (B.pack "test") >>= await
-}

outputRaws = rawInSource $$ T.map show =$ outSink

outSink :: MonadIO m => Sink String m ()
outSink = actionSink (I.liftIO . printLn)

main = withSysoutPrint $ startThreadGroup [
		runXBee connector1 $ showPeers
            ] >>= awaitThreadGroup >> printLn "Done."
         

initXBee n = do a  <- setLocalAT address16 disabledAddress
                ni <- setLocalAT nodeIdentifier n
                await (a >> ni)

sayHi = do a <- (address64 LocalAT) >>= await
           i <- getLocalAT nodeIdentifier >>= await
           output $ "XBee connected: " ++ show a ++ " (" ++ show i ++ ")"

diagnostics = do
        out "Address64 = " (address64 LocalAT)
        a16 <- getLocalAT address16 >>= await
        output $ "Address16 = " ++ show a16
        out "Association = " (associationIndication LocalAT)
        --setAT address16 (Address16 0x1234) >>= await
        out "New Address16 = " $ getLocalAT address16 
        out "NodeIdentifier = " $ getLocalAT nodeIdentifier 
        out "PAN-ID = " $ getLocalAT panId 
        out "Hardware Version = " (hardwareVersion LocalAT)
        out "Software Version = " (softwareVersion LocalAT)
        --setAT address16 a16 >>= await
        --showPeers


getPeers = discover (3200 :: Millisecond) >>= await

showPeers = do
        -- 3200 is the shortest allowable
        nodes <- discover (3200 :: Millisecond) >>= await
        output $ "Found " ++ show (length nodes) ++ " peers:"
        mapM_ (output . ("   " ++) . formatNode) nodes
    where formatNode n = "- " ++ show (nodeAddress64 n) ++ " called " ++
              show (nodeId n)

{-
sayHiToAll = do
        broadcast $ utfToBs "Hi everybody"
        nodes <- discover (300 :: Millisecond) >>= await
        output $ "Saying hi to all " ++ show (length nodes) ++ " peers"
        c <- mapM sendHi nodes
        mapM_ await c
    where sendHi n = transmit to $ utfToBs $ "Hi, you're " ++ show (nodeAddress64 n)
            where to = XBeeAddress64 $ nodeAddress64 n

utfToBs = BS.pack . S.encode
bsToUtf = S.decode . BS.unpack

sayBye = broadcast endSignal

receiveMsgs = output "Waiting for messages.." >> outputMsgs

outputMsgs = messagesSource $$ T.map format =$ outSink
    where format msg = "Got '" ++ bsToUtf (messageBody msg) ++ "' from " ++ showSender (sender msg)
          showSender (XBeeAddress16 a) = show a
          showSender (XBeeAddress64 a) = show a


outputRaws = rawInSource $$ T.map show =$ outSink


endSignal = BS.pack [0]

echoMsgs = messagesSource $$ T.mapM deco =$ replySink (fun . messageBody)
    where fun body | body == endSignal = Nothing
                   | otherwise         = Just $ utfToBs ("You said " ++ bsToUtf body)
          deco i = (output . ("> echoing to: " ++) . bsToUtf . messageBody) i >> return i


replySink :: (ReceivedMessage -> Maybe ByteString) -> Sink ReceivedMessage XBeeCmd ()
replySink f = do msg <- inputMaybe
                 fromMaybe (return ()) $ do
                    m <- msg
                    a <- f m
                    return $ (return $ transmit (sender m) a >>= await) >> replySink f

outSink :: MonadIO m => Sink String m ()
outSink = actionSink (I.liftIO . printLn)
-}


output = liftIO . printLn

out pre c = c >>= await >>= o
    where o i = output $ pre ++ show i
