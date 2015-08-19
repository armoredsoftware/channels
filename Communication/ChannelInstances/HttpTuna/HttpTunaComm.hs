{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module HttpTunaComm  (HttpTunaChannel (..)) where

import AbstractedCommunication 
import Data.Aeson
import qualified Network.Http.Client as HttpClient
import Network.Http.Client
import qualified Web.Scotty as Scotty

import HttpTools
import Control.Concurrent.MVar
import Control.Concurrent
import Web.Scotty hiding ( put)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import Control.Applicative
import Data.ByteString.Lazy hiding (putStrLn,length,map)
import Control.Monad
import Control.Monad.State.Strict
import System.IO.Error (tryIOError)
import System.Timeout

import ByteStringJSON

data HttpTunaChannel = HttpTunaChannel {
    httpchanThreadID         :: MVar ThreadId,
    httpchanMyServingPort    :: HttpClient.Port,
    httpchanTheirServingPort :: Maybe HttpClient.Port,
    httpchanTheirIp 	     :: Maybe HttpClient.Hostname,
    httpchanMaybeConnection  :: (Maybe HttpClient.Connection),
    mvarMess                 :: (MVar Value)
 --   httpchanTMVarMsgList     :: (IsMessage a) => TMVar [a],
--    httpchanTMVarUnit        :: TMVar ()
   }


sendHttp :: (IsMessage a) => a -> HttpTunaChannel -> IO Bool
sendHttp mess c = do
  case (httpchanTheirServingPort c, httpchanTheirIp c) of 
   (Just p,Just ipp) -> do
     putStrLn "Tuna Forward.."
     sendToTuna ipp p (toJSON mess)
     return True
   (_,_) -> do 
     putStrLn $ "Error in sendHttp (tuna version)!!! can't send anything! found nothing for 'their' port or IP"
     return False
   

receiveHttp :: (IsMessage a) => HttpTunaChannel -> IO (Result a)
receiveHttp c = do 
  val <- takeMVar (mvarMess c)
  putStrLn $ "Receiving: " ++ (show val)
  return (fromJSON val)



httpServe :: HttpTunaChannel -> IO ()
httpServe c = do
  let myport = httpchanMyServingPort c 
  let intPort = (fromIntegral myport :: Int)
  Scotty.scotty intPort $ do
  	    Scotty.get "/" $ Scotty.text "serving\n"  --a quick way to check if the CA is 
	    				 	--serving, client should see "foobar" 
	    				 	--(Note: unqualified get is ambiguous).

	    Scotty.post "/" $ do
	      --reads in "potential" msg (parsing it could fail). 
	      --Note: no en/de-crypting is yet taking place.
	      a <- (param "request") :: ActionM LazyText.Text  
	     -- myprint' ("Received (Text):\n" ++ (show a)) 2 --debug show of text.
	     -- myprint' ("Received (UTF8):\n" ++ (show (LazyEncoding.encodeUtf8 a))) 2 --debug printout.
	     -- myprint' ("Data received on port: " ++ (show port)) 1
	      
	      --first converts the Text to UTF8, then then attempts to read a CARequest
	      let jj = jsonEitherDecode (LazyEncoding.encodeUtf8 a) :: Either String Value
              liftIO $ putStrLn $ "RECEIVED: " ++ (show jj) ++ " on port: " ++ (show intPort)
	      case jj of
		(Left err)     -> text (LazyText.pack "ERROR: Could not even parse as Value.")
		(Right mess) -> do
		  liftIO $ forkIO $ putMVar (mvarMess c) mess 
		  Scotty.text "success" --(HttpSuccess (myport)) -- don't know why it won't let me put an empty string in there.
		  
  return ()

defaultport = 55555
maxport =     55655
instance IsChannel HttpTunaChannel where
  send hc mess = sendHttp mess hc 
  receive hc = receiveHttp hc 
  initialize hc = do
    putStrLn $ "INITIALIZING HTTPCHANNEL SERVING ON: " ++ (show (httpchanMyServingPort hc))
    let mv = httpchanThreadID hc 
    b <- isEmptyMVar mv 
    if b then return ()
         else do 
          t <- takeMVar mv 
          killThread t                         
    tid <- forkIO $ httpServe hc 
    putMVar mv tid  
  killChan hc = do 
    let mv = httpchanThreadID hc 
    b <- isEmptyMVar mv 
    if b then return ()
         else do 
          t <- takeMVar mv 
          killThread t 
  toRequest hc = do 
    mip <- getMyIP' 
    return (toJSON (HttpReq mip (httpchanMyServingPort hc)))
  amend reqv hc = do 
    case (fromJSON reqv) :: Result HttpReq of
      Error er -> do 
        putStrLn $ "failed to read as request. Nothing to amend!!"
        -- return $ Left $ "No parse of Req: " ++ er 
        return hc 
      Success req -> do 
        putStrLn $ "successfully amended using: " ++ (show req)
        return $ HttpTunaChannel (httpchanThreadID hc) (httpchanMyServingPort hc)  (Just (httpPort req)) (Just (httpIP req)) (httpchanMaybeConnection hc) (mvarMess hc)
  fromRequest reqv hc = do
     case (fromJSON reqv) :: Result HttpReq of
      Error er -> return $ Left $ "No parse of Req: " ++ er 
      Success req -> do 
        putStrLn $ "fromRequest: " ++ (show req)
        eitherPort <- findOpenPort defaultport maxport
        case eitherPort of 
          Left err -> do 
            let str = "Error finding open port in 'fromRequest': " ++ err 
            putStrLn str
            return $ Left str 
          Right p -> do 
            mv  <- newEmptyMVar
            mv2 <- newEmptyMVar 
            return $ Right $ HttpTunaChannel mv {-(httpchanThreadID hc)-} p {- (httpchanMyServingPort hc)-}  (Just (httpPort req)) (Just (httpIP req)) (httpchanMaybeConnection hc) mv2 {-(mvarMess hc)-}
  chanTypeOf c = "HttpTunaChannel"
    
       
   
  defaultChan = do 
--   ipp <- getMyIP'
   mv1 <- newEmptyMVar
   mv2 <- newEmptyMVar 
   return $ HttpTunaChannel mv1 (fromIntegral defaultport) (Just (fromIntegral defaultport)) Nothing Nothing mv2
data HttpReq = HttpReq {
                httpIP :: HttpClient.Hostname,
                httpPort :: HttpClient.Port
                } deriving (Show, Eq)
instance ToJSON HttpReq where
 toJSON x = object 
  [ "httpIP" .= toJSON (httpIP x)
  , "httpPort" .= toJSON (httpPort x)
  ]

instance FromJSON HttpReq where
  parseJSON (Object o) = HttpReq <$> o .: "httpIP"
                                 <*> o .: "httpPort"


dumdumHttpIO = do 
 mv1 <- newEmptyMVar
 mv2 <- newEmptyMVar
 return $ HttpTunaChannel mv1 0 Nothing Nothing Nothing mv2 

test :: IO ()
test = do 
  putStrLn "begginning test"
  lilNeg <- defaultChan :: IO HttpTunaChannel
  comneg <- mkChannel $ lilNeg 
  dumChan <- defaultChan :: IO HttpTunaChannel
  emptyChan <- mkChannel' dumChan
  declareCommunication (comneg,[emptyChan]) (\x -> putStrLn "succ")

  return ()

--findOpenPort :: HttpClient.Port -> HttpClient.Port -> IO (Either String HttpClient.Port)
findOpenPort x max = do 
  putStrLn $ "Doing find Open port!!"
  if x > max then return $ Left "Max point num reached"
   else do 
    meither <- timeout 250 $ tryIOError $ do  scotty x  (do Web.Scotty.get "/" $ Web.Scotty.text "hello there!\n")
    case meither of 
      Nothing -> return $ Right (fromIntegral x) 
      Just _  -> findOpenPort (x + 1) max






{- EDIT FOR TUNA FORWARDING -}
tunaIP= "129.237.120.39"
tunaport = 55111
sendToTuna :: Hostname -> Port -> Value -> IO Connection
sendToTuna i p v = do
   c <- openConnection tunaIP tunaport
   putStrLn "Just opened a connection to TUNA"
   q <- buildRequest $ do
      http POST "/"
      setAccept "text/html/json"
      setContentType "application/x-www-form-urlencoded"
    --Prelude.putStrLn ( "Request: " ++ (show req))
   mip <- getMyIP'
   let nvs = [("request", (toStrict (Data.Aeson.encode (mip, i,p,v))))]
   --Prelude.putStrLn "about to send request"
   let x = encodedFormBody nvs
   --print "Made it here yaaaaaaaaaaaay"
   sendRequest c q (x)
   putStrLn "Just performed sendRequeset to TUNA' "
   return c
{- ------------------------ -}