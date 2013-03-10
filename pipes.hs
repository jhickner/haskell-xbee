-- move this to pipes.hs in HASKELL

-- Pipes stands alone in some regards: This bidirectional flow of information
-- separates pipes from other streaming libraries which are unable to model
-- Clients, Servers, or Proxys. Using pipes you can define interfaces to RPC
-- interfaces, REST architectures, message buses, chat clients, web servers,
-- network protocols ... you name it!

-- just one operator >-> is super nice

-- runIdentityK takes an arg and returns an IdentityP

-- the remote modbus device needs time between received messages

import System.Hardware.Serial hiding (baudRate, BaudRate(..))
import qualified System.Hardware.Serial as Ser
import System.Hardware.XBee.Command
import System.Hardware.XBee.Frame

import Data.Serialize
--import Data.Modbus

import Data.Word
import qualified Data.ByteString as B

import System.IO
import Control.Concurrent hiding (yield)

import Control.Exception hiding (throw, catch)
import Debug.Trace
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

import Data.Maybe

import Control.Proxy.Prelude
import Control.Proxy
import Control.Proxy.Trans.Either

import Prelude hiding (catch)

import Control.Proxy.TCP

import Modbus

{-
incoming on socket:
  parse modbus message
  get slave id
  lookup a64 by slaveid
  save a corellation between socket and [a64, slaveid]
  use a64 nodeIdentifier to translate to local slaveid
  surgery on modbus packet to switch slaveid
  package modbus message in frame
  xbee send to a64

incoming on xbee:
  parse xbee frame
  if it's a receive, parse modbus message
  lookup up correlation for recived [a64, slaveid]
  send modbus response to correct socket

-}

addrs = [Address64 0x13a2004092d041]

openPort = open' >> open' -- bug
  where open' = openSerialPort "/dev/tty.usbserial-A8003VSL" $ 
          defaultSettings { Ser.baudRate = Ser.B115200 }
 
main = do
    p <- openPort
    forkIO $ runProxy $ handleReaderS p >-> unwrapModResponses
    serveFork (Host "127.0.0.1") "8000" $ \(sock, remoteAddr) ->
        -- serial -> socket
        -- not working because this isn't closed when the socket disconnects
        -- so instead we'd need to register a listener for this address64
        -- paired with this socket, and send responses to it
        {-
        forkIO $ runProxy $ handleReaderS p 
            >-> printD
            >-> unwrapModResponses
            >-> socketWriteD sock
        -}
        -- socket -> serial
        runProxy $ socketReadS 4096 sock 
            >-> wrapModRequests (head addrs) 
            >-> handleWriterD p


encodeTX a64 bs = makeFrame $ Transmit64 noFrameId a64 bs
makeFrame = encode . frame . encode

xbeeCommandIns = decoder (get :: Get Frame) >-> 
                 mapD frameData >->
                 decoder (get :: Get CommandIn)

unwrapModResponses =
        xbeeCommandIns 
    >-> rxPayloads
    >-> decoder (get :: Get ModResponseFrame)
    >-> printD
    >-> mapD encode

wrapModRequests a64 =
        decoder (get :: Get ModRequestFrame)
    >-> printD
    >-> mapD encode
    >-> mapD (encodeTX a64)



decoderSession p = handleReaderS p 
               >-> decoder (get :: Get Frame)
               >-> mapD frameData 
               >-> decoder (get :: Get CommandIn)
               >-> printD




rxPayloads :: Proxy p => () -> Pipe p CommandIn B.ByteString IO ()
rxPayloads () = runIdentityP $ forever $ do
    c <- request ()
    case c of
      (Receive _ _ _ b) -> respond b
      _                 -> return ()


--msg = B.pack $ readHoldingRegisters 254 100 2

--addrs = [Address64 0x13a2004098b28c, Address64 0x13a2004092d041]


toTXCommand :: Proxy p => () -> Pipe p a B.ByteString IO ()
toTXCommand () = runIdentityP $ forever $ do
    _ <- request()
    respond $ encodeTX (head addrs) msg
  where
    msg = encode $ ModRequestFrame 254 (ReadHoldingRegisters 16 1)


toATCommand :: Proxy p => () -> Pipe p String B.ByteString IO ()
toATCommand () = runIdentityP $ forever $ do
    s <- request()
    case s of
      [c1,c2] -> respond $ encodeAT c1 c2
      _       -> return ()

cmds :: Proxy p => () -> Producer p B.ByteString IO ()
cmds () = runIdentityP $ forever $ do
    fromListS [ encodeAT 'B' 'D', encodeAT 'B' 'D', encodeAT 'A' 'I'] ()
    lift $ threadDelay 1000000



encodeAT c1 c2 = makeFrame $ ATCommand frameId (commandName c1 c2) B.empty

-------------------------------------------------------------------------------

handleReaderS :: Proxy p => Handle -> () -> Producer p B.ByteString IO ()
handleReaderS h () = runIdentityP go
  where
    go = do
      eof <- lift $ hIsEOF h
      unless eof $ do
        chunk <- lift $ B.hGetSome h 4096
        respond chunk
        go

handleWriterD:: Proxy p => Handle -> () -> Consumer p B.ByteString IO r
handleWriterD h () = runIdentityP . forever $ request () >>= lift . B.hPut h

-------------------------------------------------------------------------------

chanReaderS :: Proxy p => Chan a -> () -> Producer p a IO ()
chanReaderS c () = runIdentityP . forever $ do
    a <- lift $ readChan c
    respond a

chanWriterD:: Proxy p => Chan a -> () -> Consumer p a IO r
chanWriterD c () = runIdentityP . forever $ request () >>= lift . writeChan c

-------------------------------------------------------------------------------

decoder :: (Proxy p, Serialize a) => Get a -> () -> Pipe p B.ByteString a IO r
decoder g () = runIdentityP $ loop Nothing Nothing
  where
    loop mk mbin = do
        bin <- maybe (request ()) return mbin
        case fromMaybe (runGetPartial g) mk bin of
          Fail reason -> do
              lift $ putStrLn reason -- log the error
              loop Nothing Nothing
          Partial k   -> loop (Just k) Nothing
          Done c bin' -> do
              respond c
              loop Nothing (Just bin')

