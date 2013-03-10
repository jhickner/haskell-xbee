import Modbus

import Data.Serialize
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)

import Control.Proxy
import Control.Proxy.Prelude
import Control.Proxy.TCP


main = serveFork (Host "127.0.0.1") "8000" $ \(sock, remoteAddr) ->
           runProxy $ socketReadS 4096 sock >-> printD >-> decoder getMod >-> printD
              >-> mapD encode >-> printD


getMod :: Get ModRequestFrame
getMod = get























decoder :: (Proxy p, Serialize a) => Get a -> () -> Pipe p ByteString a IO r
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
