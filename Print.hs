module Print (
    withSysoutPrint,
    printLn
) where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import System.IO
import System.IO.Unsafe (unsafePerformIO)


data Print = PrintLn ThreadId String
           | Start
           | End deriving (Show)

{-# NOINLINE printChan #-}           
printChan = unsafePerformIO $ newChan

-- | Use to wrap the body of main.
withSysoutPrint :: IO a -> IO a
withSysoutPrint cmd = bracket (start >> tick) stop body
    where start = forkIO $ cleanup printChan >> runPutStr printChan stdout
          stop x = writeChan printChan End >> tick
          body _ = cmd
          tick = threadDelay 100000

debug s = myThreadId >>= putStrLn . (++ "> " ++ s) . show

cleanup c = writeChan c Start >> readChan c >>= step
    where step Start = return ()
          step other = readChan c >>= step

runPutStr c h = readChan c >>= handle
    where handle (PrintLn _ s) = hPutStrLn h s >> runPutStr c h
          handle End = return ()
          handle _ = runPutStr c h


printLn s = do t <- myThreadId
               writeChan printChan $ PrintLn t s