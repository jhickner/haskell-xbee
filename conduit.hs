-- await returns a maybe, which is annoying

import System.Hardware.Serial hiding (baudRate, BaudRate(..))
import qualified System.Hardware.Serial as Ser
import System.Hardware.XBee.Command
import System.Hardware.XBee.Device hiding (await)
import System.Hardware.XBee.Frame
import System.Hardware.XBee.DeviceCommand

import Data.Serialize

import Data.Word
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Cereal
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B

import System.IO
import Control.Concurrent hiding (yield)

import Control.Exception
import Debug.Trace
import Control.Monad
import Data.Maybe


openPort = openSerialPort "/dev/tty.usbserial-A8003VSL" $ 
    defaultSettings { Ser.baudRate = Ser.B115200 }
 
main = do
    openPort -- not sure why this is needed, but doesn't work without it
    p <- openPort
    forkIO $ cmds $$ sinkHandle p
    forkIO $ sourceHandle p $= conduitGet getFrame $$ CL.mapM_ print
    forever $ threadDelay 1000000

getFrame :: Get Frame
getFrame = get

cmds :: Source IO B.ByteString
cmds = forever $ do
    yield $ encodeAT 'B' 'D'
    yield $ encodeAT 'B' 'D'
    yield $ encodeAT 'A' 'I'
    liftIO $ threadDelay 1000000

encodeAT c1 c2 = encode . frame . encode $ 
    ATCommand frameId (commandName c1 c2) B.empty

{-
deserializer :: Conduit B.ByteString IO (Either String Frame)
deserializer = loop Nothing Nothing
  where
    loop mk mbin = do
        -- await is a Conduit but Just . return is just a maybe
        -- can't compose like this
        bin <- maybe await (Just . return) mbin
        case bin of
          Nothing -> return ()
          Just b ->
            case fromMaybe (runGetPartial get) mk b of
              Fail reason -> do
                  yield $ Left reason
                  loop Nothing Nothing
              Partial k   -> loop (Just k) Nothing
              Done c bin' -> do
                  yield $ Right c
                  loop Nothing (Just bin')
-}
