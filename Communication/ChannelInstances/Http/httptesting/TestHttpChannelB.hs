{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module TestHttpChannelB where

import System.Random
import HttpComm
import AbstractedCommunication 
import Data.Aeson
--import ProtoTypes
import Demo3Shared
import DefaultComm 
import Control.Concurrent
myMain :: IO ()
myMain = do
     let f = (\x -> do
                send x [("hello", "this be the second field")]
                resp <- receive x :: IO (Result Value)
                putStrLn $ "response: " ++ (show resp))
     declareDefaultComm f 
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