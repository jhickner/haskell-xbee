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
import Print


connector  f s = handleConnector $ openSerialPort f s
--connector  f s = handleConnector $ openSerialPort f s
--connector' f s = slowHandleConnector 30000 $ openSerialPort f s
connector1 = connector "/dev/ttyUSB0" $ defaultSettings { Ser.baudRate = Ser.B115200 }
connector2 = connector "/dev/ttyUSB1" $ defaultSettings


{-
remoteTest = do
    peers <- runXBee connector1 getPeers
    let p1 = nodeAddress64 $ head peers
    let rc = remoteATCommand p1 'M' 'Y' () :: XBeeCmd (Future Word16)
    runXBee connector1 $ getAT rc >>= await
    return ()
-}

main = withSysoutPrint $ startThreadGroup [
		runXBee connector1 $ showPeers
                --runXBee connector1 $ initXBee "One" >> sayHi >> showPeers >> sayHiToAll >> sayBye
                --runXBee connector2 $ initXBee "Two" >> sayHi >> echoMsgs
            ] >>= awaitThreadGroup >> printLn "Done."
          

initXBee n = do a  <- setAT address16 disabledAddress
                ni <- setAT nodeIdentifier n
                await (a >> ni)

sayHi = do a <- address64 >>= await
           i <- getAT nodeIdentifier >>= await
           output $ "XBee connected: " ++ show a ++ " (" ++ show i ++ ")"

diagnostics = do
        out "Address64 = " address64
        a16 <- getAT address16 >>= await
        output $ "Address16 = " ++ show a16
        out "Association = " associationIndication
        --setAT address16 (Address16 0x1234) >>= await
        out "New Address16 = " $ getAT address16
        out "NodeIdentifier = " $ getAT nodeIdentifier
        out "PAN-ID = " $ getAT panId
        out "Hardware Version = " hardwareVersion
        out "Software Version = " softwareVersion
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


output = liftIO . printLn

out pre c = c >>= await >>= o
    where o i = output $ pre ++ show i
