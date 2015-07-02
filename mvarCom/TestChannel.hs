{-# LANGUAGE InstanceSigs, ConstraintKinds #-}


module TestChannel where

import AbstractedCommunication
import Control.Concurrent.MVar
import Demo3SharedNOVCHAN
import Data.Aeson
import Control.Concurrent 
import System.IO.Unsafe
import qualified  Data.Map.Strict as M



data TestChan = TestChan String String (MVar (Value,String)) 

--instance IsMessage EvidenceDescriptor where


instance IsChannel (TestChan) where
  send (TestChan me they mvar) m = do 
    putMVar mvar (toJSON m, they)
 --   tryPutMVar mvarUnit ()
    return True
  receive (TestChan me they mvar) = do 
    putStrLn "****"
    v <- waitForMe me mvar 
    putStrLn "TESTREC"
    return $ fromJSON v 
  killChan (TestChan me they mvar) = do 
     putStrLn $ "killing channel: " ++ me
  initFail c = "Test Channel has failed"
  chanTypeOf c = "Test Channel"

waitForMe :: String ->  (MVar (Value,String)) -> IO Value
waitForMe me mvar = do 
  (_,n) <- readMVar mvar
  putStrLn "WAIT"
  if n == me 
   then do
    (v,_) <- takeMVar mvar 
    return v   
   else do 
    yield
    threadDelay 100 
    waitForMe me mvar  
    

instantiateOnNetwork :: (MVar (Value,String)) -> [(String,String)] -> M.Map String TestChan 
instantiateOnNetwork net names = M.fromList ( map (\(me,they) -> (me,TestChan me they net)) names)

ionet = newEmptyMVar :: IO (MVar (Value,String))

{-
main' :: IO () 
main' = do 
  network <- newEmptyMVar :: IO (MVar (Value,String))
  let  m =instantiateOnNetwork network [("A","B"), ("B","A")]
       a = m M.! "A"
       b = m M.! "B" 
  amv <- newEmptyMVar
  bmv <- newEmptyMVar

  forkIO $ do 
   b' <- mkChannel b 
   --return ()  
   p <- bChannelInit b' b' 
   case p of 
    Left err -> 
      putStrLn err 
    Right _ -> do
      putStrLn "BBB#"
      putMVar amv ()
  forkIO $ do
   a' <- mkChannel a 
   --return ()
   p <- aChannelInit a' a' 
   case p of 
    Left err -> 
      putStrLn err 
    Right _ -> do
      putStrLn "AAA@"
      putMVar bmv ()
  takeMVar amv
  takeMVar bmv
  putStrLn "success"
  
  return ()

-}