{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module VChanComm where


import AbstractedCommunication
import Data.Aeson
import VChanUtil
--import Tools.CommTools
import Control.Concurrent.MVar
import Data.ByteString.Lazy hiding (putStrLn,length,map)
--import CommTools
import Data.IORef
import Control.Applicative
import Control.Concurrent
data VChannel = VChannel {
     refmTheirID :: IORef (Maybe Int),
     reflibXenVChan :: IORef (Maybe LibXenVChan),
     refIsServer :: IORef (Maybe Bool)
     }


instance IsChannel VChannel where
  send c m = do 
    mchan <- readIORef (reflibXenVChan c)
    case mchan of 
     Nothing -> return False
     Just chan -> do
        logger <- createLogger
        sendChunkedMessageByteString logger chan (toStrict (Data.Aeson.encode m))
        return True
  receive c = do
    mchan <- readIORef (reflibXenVChan c)
    case mchan of 
      Nothing -> return $ Error "ummm.... the libxenvchan didn't exist!"
      Just chan -> do 
        --ctrlWait chan --this blocks indefinately in c! bad for when we need to kill the thread.
        waitForStuff chan 
        logger <- createLogger
        bytes <- readChunkedMessageByteString logger chan
        return $ jsonParse (fromStrict bytes)
        where
        waitForStuff :: LibXenVChan -> IO ()
        waitForStuff c = do
          dat <- dataReady c
          if dat > 0 then
            return ()
          else do
           yield
           waitForStuff c
            
        
  initialize c = do 
    mTheirID <- readIORef ( refmTheirID c )
    case (mTheirID ) of 
     Nothing -> return () 
     Just idd -> do 
       mchan <- readIORef (reflibXenVChan c)
       case mchan of 
         Nothing -> do
           putStrLn $ "REGULAR INITIALIZE SERVERINIT on: " ++ (show idd)
           libchan <- server_init idd
           modifyIORef' (reflibXenVChan c) 
            (\mchan -> case mchan of 
                         Nothing -> Just libchan
                         Just x -> Just x ) 
         Just _ -> return ()
  initializeA c = do
    modifyIORef' (refIsServer c) (\_ -> Just False)
    putStrLn $ "In initializeA in VChan"
    mTheirID <- readIORef ( refmTheirID c )
    case (mTheirID ) of 
     Nothing -> return () 
     Just idd -> do 
       mchan <- readIORef (reflibXenVChan c)
       case mchan of 
         Nothing -> do
           putStrLn $ "DOING CLIENT INIT TO ID: " ++ (show idd)
           libchan <- client_init idd
           modifyIORef' (reflibXenVChan c) 
            (\mchan -> case mchan of 
                         Nothing -> Just libchan
                         Just x -> Just x ) 
         Just _ -> return ()
  initializeB c = do
    modifyIORef' (refIsServer c) (\_ -> Just True)
    putStrLn $ "In initializeB in VChan"
    mTheirID <- readIORef ( refmTheirID c )
    case (mTheirID ) of 
     Nothing -> return () 
     Just idd -> do 
       mchan <- readIORef (reflibXenVChan c)
       case mchan of 
         Nothing -> do
           putStrLn $ "DOING SERVERINIT WITH ID: " ++ (show idd)
           libchan <- server_init idd
           modifyIORef' (reflibXenVChan c) 
            (\mchan -> case mchan of 
                         Nothing -> Just libchan
                         Just x -> Just x ) 
         Just _ -> return ()         
  killChan c = do
      putStrLn "about to delay.."
      threadDelay 70000 --to see if killing the superReceive first on both sides works. 
      putStrLn "Beginning of VChannel killChan"
      mbool <- readIORef (refIsServer c)
      case mbool of
          Nothing -> do
            let str = "I don't know what to do; IsServer was never set which means regular initialize was used."
            putStrLn str
          Just b -> if b then do
                           putStrLn "I am the server, I will not kill the vchannel"
                           return ()
                         else do
                           mchan <- readIORef (reflibXenVChan c)
                           case mchan of
                             Nothing -> do
                               putStrLn "NO VChan to close!!!"
                               return ()
                             Just chan -> do
                               putStrLn "I'M CLOSING VCHAN"
                               close chan 

  toRequest c = do 
    idd <- getDomId 
    return $ toJSON $ VChanRequest idd
  fromRequest r c = do
    case fromJSON r :: Result VChanRequest of 
      Error err -> return $ Left err
      Success (VChanRequest i) ->do 
       if i == (-1) then do
          let str = "Someone requested me to talk on -1."
          putStrLn str 
          return $ Left str 
        else do
          myid <- getDomId 
          if myid == (-1) then do
            let str2 = "I don't have VChan. failing request"
            putStrLn str2
            return $ Left str2 
           else do  
             modifyIORef (refmTheirID c) (\_ -> Just i)
             return $ Right c  
  amend r c = do 
    ei <- fromRequest r c 
    case ei of 
     Left err -> 
       return c 
     Right c -> return c 
  defaultChan = do
    theirid <- newIORef Nothing
    xenvchan <- newIORef Nothing
    isserver <- newIORef Nothing
    return $ (VChannel theirid xenvchan isserver)
  chanTypeOf c = "VChannel"
newtype VChanRequest = VChanRequest {
  id :: Int
  } deriving (Show, Eq)

instance ToJSON VChanRequest where
  toJSON (VChanRequest x) = object
    [ "VChanRequestID" .= x]

instance FromJSON VChanRequest where
  parseJSON (Object o) = VChanRequest <$> o .: "VChanRequestID"    


jsonParse bs = case decode bs of
                Nothing -> Error "Error decoding!!"
                Just x -> Success x 
{-
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
    
    negotiation c = defaultChan 
-}