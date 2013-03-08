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
import Data.Modbus

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


openPort = open' >> open' -- bug
  where open' = openSerialPort "/dev/tty.usbserial-A8003VSL" $ 
          defaultSettings { Ser.baudRate = Ser.B115200 }
 
socketTest = do
    p <- openPort
    c <- newChan
    forkIO $ runProxy $ decoderSession p
    --forkIO $ runProxy $ chanReaderS c >-> toTXCommand >-> handleWriterD p
    --forkIO $ runProxy $ chanReaderS c >-> printD
    serveFork (Host "127.0.0.1") "8000" $ \(sock, remoteAddr) ->
        runProxy $ socketReadS 4096 sock >-> toTXCommand >-> handleWriterD p

decoderSession p = handleReaderS p 
               >-> decoder getFrame 
               >-> mapD frameData 
               >-> decoder getCommandIn 
               >-> printD


main = do
    openPort -- not sure why this is needed, but doesn't work without it
    p <- openPort
    forkIO $ runProxy $ getLineS >-> toATCommand >-> handleWriterD p
    forkIO $ runProxy $ handleReaderS p >-> decoder getFrame >-> 
                        mapD frameData >-> decoder getCommandIn >-> printD
    forever $ threadDelay 1000000

msg = B.pack $ readHoldingRegisters 254 100 2

--addrs = [Address64 0x13a2004098b28c, Address64 0x13a2004092d041]
addrs = [Address64 0x13a2004092d041]


toTXCommand :: Proxy p => () -> Pipe p a B.ByteString IO ()
toTXCommand () = runIdentityP $ forever $ do
    _ <- request()
    respond $ encodeTX (head addrs) msg


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


makeFrame = encode . frame . encode

encodeTX :: Address64 -> B.ByteString -> B.ByteString
encodeTX a64 bs = makeFrame $ Transmit64 noFrameId a64 bs

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

getFrame :: Get Frame
getFrame = get

getCommandIn :: Get CommandIn
getCommandIn = get

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
