module TestMVar where

import MVarComm
import Data.Aeson
import AbstractedCommunication hiding (send,receive)
import CommunicationMonad

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
main :: IO () 
main = do 
   putStrLn "beginnin"
   (cpaul,cjustin) <- instantiateOnNetwork ("Paul", "Justin")
   (npaul,njustin) <- instantiateOnNetwork ("Paulneg", "Justinneg")
   cjustin' <- mkSkeletonChannel cjustin
   cpaul' <- mkSkeletonChannel cpaul
   npaul' <- mkNegotiator npaul
   njustin' <- mkChannel njustin 
   putStrLn "^"
   forkIO $ declareCommunication' (npaul',[cpaul']) paulSide
   putStrLn ">"
   establishComm' njustin' [cjustin'] justinSide
   return ()
   
   
paulSide :: Converse () 
paulSide = do 
   liftIO $ putStrLn " beginnnnn"
   send ((1,2) :: (Int,Int))
   liftIO $ putStrLn "sent"
   x <- receive :: Converse (Int,Int)
   liftIO $ putStrLn (show x)
   
justinSide :: Converse ()
justinSide = do 
   liftIO $ putStrLn $ "begin"
   x <- receive :: Converse (Int,Int)
   liftIO $ putStrLn $ "Received: " ++ (show x)
   send x 
   liftIO $ putStrLn "done"
   
   
   

   