{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module DefaultComm where


import AbstractedCommunication
import HttpComm
import HttpTunaComm
import VChanComm
import CommunicationMonad
import Network.Http.Client (Hostname)
import System.IO.Error
declareDefaultComm :: (Channel -> IO a) -> IO ()
declareDefaultComm f = do 
  chans <- mkDefaultChans
  declareCommunication chans f
  
mkDefaultChans :: IO (Channel, [Channel])
mkDefaultChans = do
  vchan <- defaultChan :: IO VChannel
  vChan <- mkSkeletonChannel vchan 
  httpchan <- defaultChan :: IO HttpTunaChannel
  httpChan <- mkSkeletonChannel httpchan 
  httpnegotiator <- defaultChan :: IO HttpTunaChannel 
  httpNegotiator <- mkNegotiator httpnegotiator 
  return (httpNegotiator,[ vChan, httpChan])

declareDefaultComm' :: Converse a -> IO ()
declareDefaultComm' conv = do
  chans@(n,[vchan,httpChan]) <- mkDefaultChans
  declareCommunication' (n,[vchan,httpChan]) conv

talkTo :: Hostname -> Converse a -> IO (Either String a)
talkTo ipaddress conv = do
  eitherChan <- gimmeAChannel ipaddress
  case eitherChan of
    Left err -> return $ Left err
    Right c -> do
     eitherIOErrora <- tryIOError $ runConverse c conv
     case eitherIOErrora of
       Left ioerr -> return $ Left $ ioeGetErrorString ioerr
       Right a -> return $ Right a
     
gimmeAChannel :: Hostname -> IO (Either String Channel)
gimmeAChannel ipaddress = do
  p <- defaultChan :: IO HttpTunaChannel
  r <- toRequest p --how to talk to nego.
  ep' <- fromRequest r p --fromRequest gets an open port. 
  case ep' of
    Left err -> do
      let str = "Error in gimmeAChannel. Failed to create a channel to the neg: " ++ err
      putStrLn str
      return $ Left str
    Right toNeg -> do
      
      let (HttpTunaChannel {..}) = toNeg --defaultChan :: IO HttpChannel
      let chanToThem = HttpTunaChannel { httpchanTheirIp = Just ipaddress, ..}
      chanToThem' <- mkChannel chanToThem
      vchan <- defaultChan :: IO VChannel 
      vChan <- mkSkeletonChannel vchan 
      httpchan <- defaultChan :: IO HttpTunaChannel
      req <- toRequest httpchan 
      eitherNewhttpchan <- fromRequest req httpchan 
      case eitherNewhttpchan of 
        Left err -> do 
          putStrLn $ err
          killChan chanToThem' --this method started it, I better clean it up.
          return $ Left err 
        Right newChan -> do 
          httpChan <- mkSkeletonChannel newChan  
          --change here to skeleton
          httpChan' <- mkSkeletonChannel httpchan
          r <- establishComm chanToThem' [vChan, httpChan']
          killChan chanToThem' --this method started it, I better clean it up.
          return r

  

