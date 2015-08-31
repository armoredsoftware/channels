{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module TestHttpChannelB where

import System.Random
import HttpComm
import AbstractedCommunication hiding (send, receive)
import Data.Aeson
--import ProtoTypes
--import Demo3Shared
import DefaultComm 
import Control.Concurrent
import CommunicationMonad
import Control.Monad.IO.Class
{-myMain :: IO ()
myMain = do
     let f = (\x -> do
                send x [(11, 22)::(Int,Int)]
                resp <- receive x :: IO (Result Value)
                putStrLn $ "response: " ++ (show resp))
     declareDefaultComm f
-}

myMain :: IO ()
myMain = do
  declareDefaultComm' testB


  
type Ip = [(Int,Int)]
testB :: Converse ()
testB = do
  let message = [(11,22) :: (Int,Int)]
  send message
  resp <- receive :: Converse [(Int,Int)]
  liftIO $ putStrLn $ "response: " ++ (show resp)
  let message2 = ([(111,222)] :: Ip)
  send message2
  liftIO $ putStrLn $ "End of Converse"
  return ()
{-
     lilNeg <- defaultChan :: IO HttpChannel

     comneg <- mkNegotiator lilNeg --''
     dumchan <- defaultChan :: IO HttpChannel
     c <- defaultChan :: IO HttpChannel
     c' <- mkSkeletonChannel c 
     declareCommunication (comneg, [c']) (\x -> do
       send x [D0] -- [("hellow", "did this work???")]
       resp <- receive x :: IO (Result Value)
       putStrLn $ "response: " ++ (show resp)
      --
       
       putStrLn "succ")
     putStrLn "success on A side!!"
  -} 