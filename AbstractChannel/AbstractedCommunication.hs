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
import qualified Data.HashMap.Strict as HM (member, lookup)
import Data.IORef
{-
  1. Channels should be able to be created "halfable." What I mean is you 
     should be able to create your channel WITHOUT first knowing to 
     whom you are speaking. This is so your channel can receive before 
     you know where to send.
     This is important when you are the initializer of communication. Your
     channel is required to be ready to receive without knowing yet how the
     other guy wants you to send stuff (i.e. which port? for instance).
     This brings up the important property your channel needs as well:
     amendability.
  2. Amendability:
      Your Channel should be amendable. Meaning after creation, you need to
      provide an implentation of amend that, using your comm request, amends
      the existing channel to reflect the request. 
      
  3. You should never call initialize on  
  -}
  
  
class IsChannel a where
    send ::  (IsMessage b) => a -> b -> IO Bool
    --should add parameter of an IO computation to do in case of a receive error. 
    --come to think of it, probably could use one for send as well.
    
    {-receive implementation should block indefinitely.
      Timeouts are handled in "the big channel" -}
    receive :: (IsMessage b) => a -> IO (Result b)
    {- Put any initialization needed here.-}
    initialize :: a -> IO ()
    {- Put any channel cleanup here. -}
    initializeA :: a -> IO ()
    initializeA = initialize
    initializeB :: a -> IO ()
    initializeB = initialize 
    killChan :: a -> IO ()
    {- Declare how to make a communication request out of an existing channel. -}
    toRequest :: a -> IO Value
    {- Do not initialize your channel when implementing this method,
       just create it. -}
    fromRequest :: Value -> a -> IO (Either String a)
    amend ::  Value -> a -> IO a 
    defaultChan :: IO a 
    initFail :: a -> String
    chanTypeOf :: a -> String
    negotiation ::  a -> IO a
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
   initialize a
   putStrLn "In initialize for Channel"
   tr <- readIORef tidref
   case tr of 
     Nothing -> do
       tid <- forkIO $ superReceive c 
       modifyIORef tidref (\_ -> Just tid)
     Just _ -> return () 
 initializeA c@(Channel a tidref _ _) = do
   initializeA a
   putStrLn "In initializeA for Channel"
   tr <- readIORef tidref
   case tr of 
     Nothing -> do
       tid <- forkIO $ superReceive c 
       modifyIORef tidref (\_ -> Just tid)
     Just _ -> return () 
   
 initializeB c@(Channel a tidref _ _) = do
   initializeB a
   putStrLn $ "In initializeB for Channel"
   tr <- readIORef tidref
   case tr of 
     Nothing -> do
       tid <- forkIO $ superReceive c 
       modifyIORef tidref (\_ -> Just tid)
     Just _ -> return ()
   putStrLn $ "Calling inner initializeB"
 killChan (Channel a tidref _ _) = do 
   killChan a
   tid <- readIORef tidref
   case tid of 
     Nothing -> return () 
     Just tid' -> killThread tid' 
 toRequest (Channel a _ _ _) = toRequest a --(internalChan c)
 fromRequest v (Channel a b c d) = do 
   elilChan <- (fromRequest v a)  --(internalChan c)
   case elilChan of 
     Left err -> return (Left err)
     Right lilc -> do 
       ch <- mkChannel' lilc 
       return $ Right ch -- (Channel lilc b c d) 
 amend v (Channel a b c d) = do 
   a' <- amend v a 
   return $ Channel a' b c d
 negotiation (Channel a _ _ _ ) = do 
  c <- negotiation (a)
  mkChannel c 
 chanTypeOf (Channel a _ _ _) = "Channel: " ++ (chanTypeOf a)
type IsMessage a = (ToJSON a, FromJSON a)

newtype CommResponse = CommResponse (Integer,Integer) deriving (Show, Eq)
newtype CommFail = CommFail String deriving (Show, Eq)
instance ToJSON CommFail where
  toJSON (CommFail str) = object 
    ["CommFail" .= toJSON str]
instance FromJSON CommFail where
  parseJSON (Object o) = CommFail <$> o .: "CommFail"
  parseJSON _          = mzero  

instance ToJSON CommResponse where
  toJSON (CommResponse pair) = object
    [ "Nonces" .= toJSON pair]
instance FromJSON CommResponse where
  parseJSON (Object o) = CommResponse <$> o .: "Nonces"
  parseJSON _          = mzero     

newtype SingleNonce = SingleNonce Integer deriving (Show, Eq, Ord)
instance ToJSON SingleNonce where
  toJSON (SingleNonce n) = object
    ["SingleNonce" .= toJSON n]
instance FromJSON SingleNonce where
  parseJSON (Object o) = SingleNonce <$> o .: "SingleNonce"
  parseJSON _          = mzero  
newtype NOMATCH = NOMATCH () deriving (Show, Eq)
instance ToJSON NOMATCH where
 toJSON (NOMATCH ()) = object
   ["NOMATCH" .= toJSON ()]
instance FromJSON NOMATCH where
 parseJSON (Object o) = NOMATCH <$> o .: "NOMATCH"
 parseJSON _          = mzero
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
  

 
-- the list of channels need to be 'unintialized' meaning they were made with mkChannel'. 
declareCommunication :: (Channel, [Channel]) ->(Channel -> IO a) -> IO ()
declareCommunication (mc@(Channel a _ _ _),chls) f =  do
 forever $ do
  msg <- receive a :: IO (Result (Value,Value,Integer))   -- forkIO $ stutterReceive mc mv 
  putStrLn $ "I received a message on the communicator negotiator!!: " ++ (show msg)
  case  msg of 
   Error err -> do 
    putStrLn $ "Error in declareCommunication: improper comm request. expected (Value,Value,Integer)!: " ++ err 
   Success (negReq,req,nonce) -> do
     neg' <- amend negReq mc 
     let ls = map (fromRequest req) chls  
     ls' <- sequence ls 
     case fstRight ls' of 
       Nothing -> do 
        putStrLn "no matching channels"
        send neg' (NOMATCH ())
        return ()
       Just chan -> do
         putStrLn $ "About to initialize chan and do bChannelInit" 
         initializeB chan --because what is in the list should have been made by mkSkeleton
         reqBack <- toRequest chan
         send neg' (reqBack,nonce+1) 
         echan <- bChannelInit chan nonce 
         case echan of 
           Left err -> do 
             putStrLn $ "Error in bchannel init: " ++ err 
           Right chan' -> do
             putStrLn $ "Channel successfully established: " ++ (chanTypeOf chan')
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
  --initialize x 
  v <- toRequest x 
  ex' <- fromRequest v x
  case ex' of
   Left err -> do
     putStrLn $ "weird error in establishComm. this should always succeed: " ++ err ++ " trying next skelchan in list."
     establishComm neg xs
   Right x' -> do 
    -- putStrLn $ "In establishComm, about to initialize x'"
    -- initialize x' 
     eitherchan <- aChannelInit neg x' 
     case eitherchan of 
       Left err -> do 
         killChan x'
         --PUT THE R
         establishComm neg xs
       Right chan -> do
         putStrLn $ "Channel established: " ++ (chanTypeOf chan)
         return $ Right chan 

  

--second channel gets ammended into channel we return. 
aChannelInit :: Channel -> Channel -> IO (Either String (Channel))
aChannelInit toneg c = do 
  putStrLn "beginning achannelInit"
  (req,n1) <- mkCommRequest c 
  putStrLn $ "Here is the request I am creating for my request: " ++ (show req)
  negreq <- toRequest toneg 
  send toneg (negreq,req,n1)
  wwval <- receive toneg :: IO (Result Value) --just as value to catch the NOMATCH Response (Value,Integer))
  case wwval of
    Error err -> do
      let str = "ERROR in aChannelInit. first receive failed. Couldn't even read the message as a Value"
      putStrLn str
      return $ Left str
    Success val -> do
      case fromJSON val :: Result (NOMATCH) of
        Success _ -> do
          let str = "Target has responded that it does not support the attempted channel."
          putStrLn str
          return $ Left str
        Error _ -> --this is a GOOD thing. We failed to read the response as an error.. so it was what we expected! maybe.. but at least not an error.  
          case fromJSON val :: Result (Value,Integer) of
            Error err -> do
              let str =  "Error in aChannelInit in mess back on neg chan: " ++ (show val)
              putStrLn $ str ++ " " ++ err
              return $ Left (str ++ " " ++ err)
     
            Success (val,n1') ->
               if n1' == (n1 + 1) then do         
                  c' <- amend val c
                  initializeA c'
                  n2 <- randomIO :: IO Integer
                  send c' (SingleNonce n2)
                  wn2' <- receive c' :: IO (Result SingleNonce)
                  case wn2' of
                    Error err -> do
                      let str = "Error getting back n2+1: "
                      putStrLn $ str ++ err
                      return $ Left (str ++ err)
                    Success (SingleNonce n2') ->
                      if (n2' == (n2 + 1)) then do
                        putStrLn "Successfully got n2'==n2+1"
                        send c' (SingleNonce (n2' + 1))
                        return $ Right c' 
                      else do 
                        let str = "Error: n2'!=n2+1"
                        putStrLn str
                        return $ Left str 
               else do
                 let str = "Error: n1' != n1 + 1"
                 putStrLn str
                 return $ Left str 
 

bChannelInit :: Channel -> Integer -> IO (Either String Channel)
bChannelInit c na = do
     putStrLn "bChannelInit triggered!!"

     wn2 <- receive c :: IO (Result (SingleNonce))
     case wn2 of
       Error err -> do
         let str = "Error in bChannelInit receiving n2: " ++ err
         putStrLn str
         return $ Left str
       Success (SingleNonce n2) -> do
         send c (SingleNonce (n2 + 1))
         wn2'' <- receive c :: IO (Result (SingleNonce))
         case wn2'' of
           Error err -> do
             let str = "Error in bChannelInit receiving n2'': " ++ err
             putStrLn str
             return $ Left str
           Success (SingleNonce n2'') -> 
             if n2'' == (n2 + 1 + 1) then do
               return $ Right c
             else do
               let str = "Error in bChannelInit: n2''!= (n2 +1 +1)"
               putStrLn str 
               return $ Left str 
{-
             
     nb1 <- randomIO :: IO Integer 
     respReq <- toRequest c 
     send c (respReq,na+1,nb1)
     
     --send c (CommResponse (na + 1, nb1))
     resp <-  (receive c :: IO (Result CommResponse))
     putStrLn $ "received first message in bchannelInit:" ++ (show resp)
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
-}

