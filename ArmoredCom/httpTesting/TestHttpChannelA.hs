{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module TestHttpChannelA where
import Control.Concurrent
import HttpChannel
import AbstractedCommunication 
import Data.Aeson
import ArmoredTypes
import DefaultComm
myMain :: IO ()
myMain = do
 let f:: Channel -> IO ()  
     f chan = do  
         rx <- receive chan :: IO (Result [EvidenceDescriptor]) -- [(String,String)])
         case rx of 
          Error err -> do 
            putStrLn err
            return False
          Success [D0] -> do 
            send chan [D1]
         putStrLn "success on A side!!!!!!!!"
 forkIO $ declareDefaultComm (\x -> putStrLn "Someone declared a channel with me??")
 eitherChan <- gimmeAChannel "10.100.0.6"
 case eitherChan of 
   Left err -> 
     putStrLn err 
   Right chan -> do 
     f chan 
     putStrLn "Success! I did the channel Stuff"

 {-    
     lilNeg <- defaultChan :: IO HttpChannel
--  r <- toRequest lilNeg
--  lilNeg' <- fromRequest r lilNeg --this puts their port to our serving port
--  case lilNeg' of 
--   Left err -> do 
--     putStrLn $ "Error in creating fromReq of neg chan: " ++ err 
--   Right (HttpChannel {..}) -> do 
     comneg <- mkNegotiator lilNeg --(HttpChannel { httpchanTheirIp = (Just "10.100.0.6"), ..})
     dumchan <- defaultChan :: IO HttpChannel
     emptyChan <- mkSkeletonChannel dumchan
     forkIO $ declareCommunication (comneg, [emptyChan]) (\x -> putStrLn "succ")


     (HttpChannel {..}) <- defaultChan :: IO HttpChannel 
     comneg2 <- mkChannel (HttpChannel { httpchanTheirIp = (Just "10.100.0.6"),httpchanMyServingPort=55999, ..})
     (HttpChannel {..}) <- defaultChan :: IO HttpChannel
     let channelToBe = HttpChannel { httpchanMyServingPort= 55558,
                                     httpchanTheirIp=(Just "10.100.0.6"),
                                     httpchanTheirServingPort=Nothing,..}
     bigChantobe <- mkChannel channelToBe            
     echan <- aChannelInit comneg2 bigChantobe
     case echan of 
       Left err -> do 
         putStrLn err 
       Right chan -> do 
         rx <- receive chan :: IO (Result [EvidenceDescriptor]) -- [(String,String)])
         case rx of 
          Error err -> do 
            putStrLn err
            return False
          Success [D0] -> do 
            send chan [D1]  --(s1++"@@@@",s2 ++ "$$$$$")
         putStrLn "success on A side!!!!!!!!"
-}


  