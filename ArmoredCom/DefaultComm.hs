{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module DefaultComm where


import AbstractedCommunication
import HttpComm
import VChanComm
--import CommTools
import Network.Http.Client (Hostname) 
declareDefaultComm :: (Channel -> IO a) -> IO ()
declareDefaultComm f = do 
  vchan <- defaultChan :: IO VChannel 
  vChan <- mkSkeletonChannel vchan 
  httpchan <- defaultChan :: IO HttpChannel
  httpChan <- mkSkeletonChannel httpchan 
  httpnegotiator <- defaultChan :: IO HttpChannel 
  httpNegotiator <- mkNegotiator httpnegotiator 
  declareCommunication (httpNegotiator,[vChan, httpChan]) f 

gimmeAChannel :: Hostname -> IO (Either String Channel)
gimmeAChannel ipaddress = do
  p <- defaultChan :: IO HttpChannel
  r <- toRequest p --how to talk to nego.
  ep' <- fromRequest r p --fromRequest gets an open port. 
  case ep' of
    Left err -> do
      let str = "Error in gimmeAChannel. Failed to create a channel to the neg: " ++ err
      putStrLn str
      return $ Left str
    Right toNeg -> do
      
      let (HttpChannel {..}) = toNeg --defaultChan :: IO HttpChannel
      let chanToThem = HttpChannel { httpchanTheirIp = Just ipaddress, ..}
      chanToThem' <- mkChannel chanToThem
      vchan <- defaultChan :: IO VChannel 
      vChan <- mkSkeletonChannel vchan 
      httpchan <- defaultChan :: IO HttpChannel
      req <- toRequest httpchan 
      eitherNewhttpchan <- fromRequest req httpchan 
      case eitherNewhttpchan of 
        Left err -> do 
          putStrLn $ err 
          return $ Left err 
        Right newChan -> do 
          httpChan <- mkSkeletonChannel newChan  
          --change here to skeleton
          httpChan' <- mkSkeletonChannel httpchan
          establishComm chanToThem' [vChan, httpChan']
  

  

