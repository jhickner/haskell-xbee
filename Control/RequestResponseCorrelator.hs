{-# LANGUAGE Rank2Types #-}

-- | Correlates request and responses by means of a Cirular instance.
--   The responses are processed by the ResponseM monad, one request may result in more than
--   one response.

module Control.RequestResponseCorrelator (
    Correlator,
    newCorrelator,
    -- * Request / Push
    request,
    requestAndWait,
    push
) where

import Data.Word
import Data.List
import Data.Circular
import Data.SouSiT
import Data.SouSiT.Source
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative


newtype Id = Id Word64 deriving (Eq,Show)
instance Circular Id where
    initial = Id 0
    next (Id n) | n == maxBound = initial
                | otherwise     = Id (n + 1)

data Entry c i = Entry Id c (TChan i)

data Correlator c i = Correlator i                  -- purge value
                                 (TVar Id)          -- current id
                                 (TVar [Entry c i]) -- in progress


-- | Create a new correlator.
newCorrelator :: Circular c =>
    i -- ^ The value sent as a response if the request is purged from the correlator.
    -> STM (Correlator c i)
newCorrelator pv = liftM2 (Correlator pv) (newTVar initial) (newTVar [])


-- | Sends a request and waits for the response.
requestAndWait :: Circular c => Correlator c i
    -> (c -> IO ())  -- ^ Function used to send the response.
    -> Fetch i a     -- ^ Sink to process the response.
    -> IO a
requestAndWait corr sf rm = do
    (c,future,_) <- atomically $ request corr rm
    sf c
    atomically future

-- | Registers a response processor (Sink) with the correlator and returns a
-- correlation-id (must be used to tag the request with) along with an STM monad that will
-- wait for and get the request.
-- The last value of the tuple allows feeding values directly into the response. A common
-- use case is to trigger a timeout.
request :: Circular c => Correlator c i
    -> Fetch i a
    -> STM (c, STM a, i -> STM ())
request (Correlator pv idV ipV) handler = do
        Entry ident key chan <- nextId idV >>= addEntry ipV pv
        let future = processResponse handler (readTChan chan) <* removeEntry ipV ident
        let feedFun = writeTChan chan
        return (key, future, feedFun)

processResponse :: Fetch i a -> STM i -> STM a
processResponse fetch action = actionSource (liftM Just action) $$ liftFetch fetch


nextId idV = do
        v <- readTVar idV
        writeTVar idV (next v)
        return v

removeEntry inProgressV ident = do
        es <- readTVar inProgressV
        let es' = filter (not . byId ident) es
        writeTVar inProgressV es'
    where byId i (Entry c _ _) = i == c

-- | Create a new entry and add it to the inProgressV. If no slots are available the oldest
-- (=first) request gets fed purgeValue and removed from the inProgressV.
addEntry inProgressV purgeValue ident = do
        chan <- newTChan
        es <- readTVar inProgressV
        let (key,toEvict,es') = allocateKey es
        let entry = Entry ident key chan
        writeTVar inProgressV (es' ++ [entry])
        evict toEvict
        return entry
    where evict Nothing = return ()
          evict (Just (Entry _ _ c)) = writeTChan c purgeValue


-- | Gets a request key to use. If no keys are free then an entry is evicted (snd of return).
allocateKey :: Circular c => [Entry c i] -> (c, Maybe (Entry c i), [Entry c i])
allocateKey es = case findFree keys of 
        (Just k) -> (k, Nothing, es)
        Nothing  -> let evict = head es in (key evict, Just evict, tail es)
    where keys = map key es
          key (Entry _ k _) = k

-- | Let the Correlator process an response.
-- It will forward to response to the correct response processor (Sink).
push :: Eq c => Correlator c i -> c -> i -> STM ()
push (Correlator _ _ ipV) key value = do
        es <- readTVar ipV
        process $ find byKey es
    where byKey (Entry _ k _) = key == k
          process (Just (Entry _ _ c)) = writeTChan c value
          process Nothing = return ()
