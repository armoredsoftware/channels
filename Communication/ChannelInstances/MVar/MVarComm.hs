{-# LANGUAGE InstanceSigs, ConstraintKinds #-}


module MVarComm where

import AbstractedCommunication
import Control.Concurrent.MVar
--import Demo3SharedNOVCHAN
import Data.Aeson
import Control.Concurrent 
import System.IO.Unsafe
import qualified  Data.Map.Strict as M



data MVarChan = MVarChan String (MVar String) (MVar (Value,String)) 

--instance IsMessage EvidenceDescriptor where


instance IsChannel (MVarChan) where
  send (MVarChan me theym mvar) m = do
    putStrLn "mvar sending"
    they <- readMVar theym 
    putMVar mvar (toJSON m, they)
    -- putMVar theym they
 --   tryPutMVar mvarUnit ()
    return True
  receive (MVarChan me theym mvar) = do 
    putStrLn "mvar receiving"
    v <- waitForMe me mvar 
    putStrLn $ "M:" ++ (show v)
    return $ fromJSON v 
  initialize (MVarChan _ _ _) = return ()
  toRequest (MVarChan me _ _) = return $ toJSON (me)
  fromRequest val (MVarChan me1 theym mv) = do 
    putStrLn $ "F:" ++ (show val)
    case (fromJSON val :: Result String) of 
      Error err -> return (Left err)
      Success newThey -> do 
         _ <- tryTakeMVar theym 
         putMVar theym newThey 
         return $ Right (MVarChan me1 theym mv)
  
  killChan (MVarChan me they mvar) = do               
     putStrLn $ "killing channel: " ++ me
  amend val c = do 
     fr <- fromRequest val c 
     case fr of 
          Left err -> return c 
          Right c' -> return c' 
  defaultChan = do 
     x <- newEmptyMVar
     b <- newMVar "B"
     return (MVarChan "A" b x)
     
  chanTypeOf c = "MVar Channel"

waitForMe :: String ->  (MVar (Value,String)) -> IO Value
waitForMe me mvar = do
  b <- isEmptyMVar mvar 
  if b then do
    yield
    -- putStrLn "E"
    threadDelay 1000 
    waitForMe me mvar
  else do
    (_,n) <- readMVar mvar
    putStrLn "WAIT"
    if n == me 
     then do
      (v,_) <- takeMVar mvar 
      return v   
     else do 
      yield
      threadDelay 1000 
      waitForMe me mvar  
    

instantiateOnNetwork :: (String,String) -> IO (MVarChan,MVarChan)
instantiateOnNetwork (n1,n2) = do
   messageMVar <- newEmptyMVar
   m1 <- newMVar n2
   m2 <- newMVar n1
   let c1 = MVarChan n1 m1 messageMVar
       c2 = MVarChan n2 m2 messageMVar
   return (c1,c2)
   
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
