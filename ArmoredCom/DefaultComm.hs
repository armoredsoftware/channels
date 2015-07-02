{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module DefaultComm where


import AbstractedCommunication
import HttpChannel
import VChanComm
import CommTools
import Network.Http.Client (Hostname) 
declareDefaultComm :: (Channel -> IO a) -> IO ()
declareDefaultComm f = do 
  vchan <- defaultChan :: IO VChannel 
  vChan <- mkSkeletonChannel vchan 
  httpchan <- defaultChan :: IO HttpChannel
  httpChan <- mkSkeletonChannel httpchan 
  httpnegotiator <- defaultChan :: IO HttpChannel 
  httpNegotiator <- mkNegotiator httpnegotiator 
  declareCommunication (httpNegotiator,[vChan,httpChan]) f 

gimmeAChannel :: Hostname -> IO (Either String Channel)
gimmeAChannel ipaddress = do 
  (HttpChannel {..}) <- defaultChan :: IO HttpChannel
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
      establishComm chanToThem' [vChan,httpChan]
  

  

