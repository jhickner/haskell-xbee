-- seems weird and not type-safe to use Nothing to indicate end of stream
-- pipes is not that hard, and conduit is even easier...
-- the attoparsec parser is more complicated than a similar parser for
-- pipes/conduit because of the extra maybes

import System.Hardware.Serial hiding (baudRate, BaudRate(..))
import qualified System.Hardware.Serial as Ser
import System.Hardware.XBee.Command
import System.Hardware.XBee.Frame

import Data.Serialize

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import System.IO
import Control.Concurrent hiding (yield)

import Control.Exception
import Debug.Trace
import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe

import qualified System.IO.Streams as S
import qualified System.IO.Streams.Handle as H
import qualified System.IO.Streams.List as L


openPort = openSerialPort "/dev/tty.usbserial-A8003VSL" $ 
    defaultSettings { Ser.baudRate = Ser.B115200 }
 

--main = L.fromList [1, 2, 3] >>= H.handleToOutputStream H.stdout

main = do
    openPort -- not sure why this is needed, but doesn't work without it
    p <- openPort
    --src <- cmds >>= H.handleToOutputStream p
    --forkIO $ S.connect cmds $ H.handleToOutputStream p
    --forkIO $ S.connect (H.handleToInputStream p) (deserializer >>= printer)
    forever $ threadDelay 1000000



cmds = L.fromList
  [ encodeAT 'B' 'D'
  , encodeAT 'B' 'D'
  , encodeAT 'A' 'I'
  ]

encodeAT c1 c2 = encode . frame . encode $ 
    ATCommand frameId (commandName c1 c2) B.empty


deserializer :: S.InputStream B.ByteString -> Either String Frame
deserializer is = loop Nothing Nothing
  where
    loop mk mbin = do
        bin <- maybe (S.read is) return mbin
        case fromMaybe (runGetPartial get) mk bin of
          Fail reason -> do
              return $ Left reason
              loop Nothing Nothing
          Partial k   -> loop (Just k) Nothing
          Done c bin' -> do
              return $ Right c
              loop Nothing (Just bin')

