{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module AbstractedCommunication where

import Data.Aeson
import Control.Applicative
import Control.Concurrent (ThreadId)
import System.Random
import Control.Concurrent.MVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad.State.Strict
import Control.Monad
import System.Timeout (timeout)
import Control.Concurrent

import Data.IORef
class IsChannel a where
    send ::  (IsMessage b) => a -> b -> IO Bool
    --should add parameter of an IO computation to do in case of a receive error. 
    --come to think of it, probably could use one for send as well.
    receive :: (IsMessage b) => a -> IO (Result b)
    initialize :: a -> IO ()
    initFail :: a -> String
    killChan :: a -> IO ()
    chanTypeOf :: a -> String
    toRequest :: a -> IO Value
    fromRequest :: Value -> a -> IO (Either String a)
    negotiation ::  a -> IO a
    defaultChan :: IO a 
    amend ::  Value -> a -> IO a 
    negotiation c = defaultChan 




data Channel = forall a. (IsChannel a) => Channel {
     internalChan :: a,
     receiveThreadID :: IORef (Maybe ThreadId),
     getMessages :: (TMVar [Value]),
     getUnitMVar :: (MVar ())
     }

instance IsChannel (Channel) where
 send (Channel a _ _ _) = send a
 receive (Channel a _ tmvar mvarUnit) = do 
                                putStrLn "&"
                                mUnit <- timeout defaultTimeout (takeMVar mvarUnit)
                                case mUnit of 
                                  Nothing -> return $ Error $ "Error: Timeout in receive. Waited ~" ++ (show (defaultTimeout `div` 1000000)) ++ " seconds."
                                  Just _  -> do 
                                    (m:ls') <- liftIO $ atomically $ takeTMVar tmvar  
                                    liftIO $ atomically $ putTMVar tmvar ls' --regardless of what is left, put it back. 
                                    if length ls' == 0
                                     then return False --note that we have already taken the unitMVar. We simply make note here that it is still empty. false to match
                                     else tryPutMVar mvarUnit () --non-blocking
                                    return (fromJSON m)
 initFail (Channel a _ _ _) = initFail a
 initialize c@(Channel a tidref _ _) = do
   tr <- readIORef tidref
   case tr of 
     Nothing -> do
       tid <- forkIO $ superReceive c 
       modifyIORef tidref (\_ -> Just tid)
     Just _ -> return () 
   initialize a 
 killChan (Channel a tidref _ _) = do 
   killChan a
   tid <- readIORef tidref
   case tid of 
     Nothing -> return () 
     Just tid' -> killThread tid'
 chanTypeOf (Channel a _ _ _) = chanTypeOf a 
 toRequest (Channel a _ _ _) = toRequest a --(internalChan c)
 fromRequest v (Channel a b c d) = do 
   elilChan <- (fromRequest v a)  --(internalChan c)
   case elilChan of 
     Left err -> return (Left err)
     Right lilc -> do 
       ch <- mkChannel lilc 
       return $ Right ch -- (Channel lilc b c d) 
 amend v (Channel a b c d) = do 
   a' <- amend v a 
   return $ Channel a' b c d
 negotiation (Channel a _ _ _ ) = do 
  c <- negotiation (a)
  mkChannel c 

type IsMessage a = (ToJSON a, FromJSON a)

newtype CommResponse = CommResponse (Integer,Integer) deriving (Show, Eq)
newtype CommFail = CommFail String deriving (Show, Eq)
instance ToJSON CommFail where
  toJSON (CommFail str) = object 
    ["CommFail" .= toJSON str]
instance FromJSON CommFail where
  parseJSON (Object o) = CommFail <$> o .: "CommFail"


instance ToJSON CommResponse where
  toJSON (CommResponse pair) = object
    [ "Nonces" .= toJSON pair]
instance FromJSON CommResponse where
  parseJSON (Object o) = CommResponse <$> o .: "Nonces"
       

defaultTimeout = 10000000 :: Int  -- 10 seconds 
shortTimeout   = 10000000 :: Int  -- 10 second 


mkChannel :: (IsChannel a) => a ->IO Channel
mkChannel littleChan = do 
  initialize littleChan
  putStrLn "MKCHAN"
  tmvar <- newTMVarIO [] 
  mvar  <- newEmptyMVar
  threadIOr <- newIORef Nothing 
  let chan = Channel littleChan threadIOr tmvar mvar 
  tid <- forkIO $ superReceive chan
  modifyIORef threadIOr (\_ -> (Just tid))
  return chan 

mkChannel' :: (IsChannel a) => a -> IO Channel
mkChannel' lil = do 
 tmvar <- newTMVarIO []
 mvar <- newEmptyMVar
 tr <- newIORef Nothing 
 return (Channel lil tr tmvar mvar)

mkSkeletonChannel :: (IsChannel a) => a -> IO Channel
mkSkeletonChannel = mkChannel' 

mkNegotiator x = do 
  initialize x 
  mkChannel' x
  


-- the list of channels need to be 'unitialized' meaning they were made with mkChannel'. 
declareCommunication :: (Channel, [Channel]) ->(Channel -> IO a) -> IO ()
declareCommunication (mc@(Channel a _ _ _),chls) f =  do
 forever $ do
  msg <- receive a :: IO (Result (Value,Integer))   -- forkIO $ stutterReceive mc mv 
  putStrLn $ "I received a message on the communicator negotiator!!: " ++ (show msg)
  case  msg of 
   Error err -> do 
    putStrLn $ "improper comm request. not a pair!: " ++ err 
   Success (req,nonce) -> do 
     let ls = map (fromRequest req) chls  
     ls' <- sequence ls 
     case fstRight ls' of 
       Nothing -> do 
        putStrLn "no matching channels"
        
       Just chan -> do
        putStrLn $ "About to initialize chan and do bChannelInit" 
        initialize chan --because what is in the list should be mkChannel' s 
        echan <- bChannelInit chan nonce 
        case echan of 
         Left err -> do 
          putStrLn $ "Error in bchannel init: " ++ err 
         Right chan' -> do 
          f chan
          putStrLn "success"


fstRight :: [Either a b] -> Maybe b 
fstRight [] = Nothing 
fstRight ((Left _): xs) = fstRight xs 
fstRight ((Right b):_) = Just b 

stutterReceive ::Channel -> MVar Value -> IO () 
stutterReceive c@(Channel a _ _ _ ) mv = do
  putStrLn "stutter Receive"
  tj <- toRequest c  
  --putMVar mv (toJSON tj )
  msg <- receive a :: IO (Result Value)
  case msg of 
   Error err -> do 
     putStrLn err 
   Success v -> do  
    putStrLn $ "Stutter receive got a message!!!: " ++ (show v)
    putMVar mv v
    return ()
  stutterReceive c mv 


superReceive :: Channel -> IO ()
superReceive c@(Channel a _ msgTMVar unitMVar) = do 
  putStrLn "%" 
  msg <- receive a :: IO (Result Value)
  putStrLn "^^^^^^^^^^^^"
  case msg of 
   Error err -> do 
     putStrLn $ "Error superReceive: " ++ err 
   Success val -> do 
     putStrLn $ "SUPER RECEIVED: " ++ (show val) 
     _ <- tryTakeMVar unitMVar 
     msgls <- atomically $ takeTMVar msgTMVar 
     atomically $ putTMVar msgTMVar (msgls ++ [val])
     putMVar unitMVar ()
  superReceive c 
  

mkCommRequest :: (IsChannel a) => a -> IO (Value, Integer)
mkCommRequest c = do 
  reqv <- toRequest c 
  nonce <- liftIO (randomIO :: IO Integer)
  return (reqv,nonce)

establishComm :: Channel -> [Channel] -> IO (Either String Channel)
establishComm _ [] = return $ Left "No more Channels to try. All Failed. you suck."
establishComm neg (x:xs) = do
  initialize x 
  eitherchan <- aChannelInit neg x 
  case eitherchan of 
    Left err -> do 
      killChan x 
      establishComm neg xs
    Right chan -> return $ Right chan 

  

--second channel gets ammended into channel we return. 
aChannelInit :: Channel -> Channel -> IO (Either String (Channel))
aChannelInit neg c = do 
  putStrLn "beginning achannelInit"
  (req,n1) <- mkCommRequest c 
  putStrLn $ "Here is the request I am creating for my request: " ++ (show req)
  send neg (req,n1) 
  info <- receive c :: IO (Result (Value,Integer,Integer))
  case info of 
   Error err -> do 
     putStrLn err 
     return (Left err)
   Success (reqv, n1',nb1) -> do 
     c' <- amend reqv c 
     putStrLn $ " received first message, now about to send 2 nonces on the new channel: "
     n1_2 <- (randomIO :: IO Integer)
     if n1' == n1+1 then do 
       send c' (CommResponse (nb1+1,n1_2))
       putStrLn $ "nonces match, about to receive commResponse" 
       resp <- receive c' :: IO (Result CommResponse)
       case resp of 
        Error err -> do 
         putStrLn err
         return $ Left err
        Success (CommResponse (n1_2',x)) -> do 
         if ((n1_2' == (n1_2 + 1)) &&( x == 0)) then
           return $ Right c' 
          else do
           let str = "final nonces did not match."
           putStrLn str
           return $ Left str            
         else do 
          let str = "n1' did not match n1 + 1."
          putStrLn str
          return $ Left str

bChannelInit :: Channel -> Integer -> IO (Either String Channel)
bChannelInit c na = do 
     putStrLn "bChannelInit triggered!!"
     nb1 <- randomIO :: IO Integer 
     respReq <- toRequest c 
     send c (respReq,na+1,nb1)
     
     --send c (CommResponse (na + 1, nb1))
     resp <-  (receive c :: IO (Result CommResponse))
     putStrLn $ "recieved first message in bchannelInit:" ++ (show resp)
     case resp of 
       Error err -> do 
         putStrLn err 
         return $ Left err 
       Success (CommResponse (nb1',n2))-> do
         if (nb1 + 1) == nb1' 
          then do
             send c (CommResponse (n2+1,0))
             --chan <- mkChannel c 
             return $ Right c --han 
           else do 
             return $ Left "n1' did not match n1."   


