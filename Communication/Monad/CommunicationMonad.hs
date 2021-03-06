{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, ConstraintKinds #-}
module CommunicationMonad where

import AbstractedCommunication
import Data.Aeson
import Control.Applicative
import Control.Monad.IO.Class

data Converse a = Converse { chat :: (Channel -> IO (a,Channel))}
                | CommError String
instance MonadIO Converse where
  liftIO ioa = Converse (\c -> do
    x <- ioa
    return (x,c) )

--cleans up the channel and returns the result of the conversation.
runConverse :: Channel -> Converse a -> IO a
runConverse c conv = do
  (a,ch) <- chat conv c
  killChan ch
  return a

runConverse' :: Channel -> Converse a -> IO a
runConverse' c conv = do
  (a,ch) <- chat conv c
  return a 

declareCommunication' :: (Channel,[Channel]) -> Converse a -> IO ()
declareCommunication' x conv = declareCommunication x ((flip runConverse) conv)

establishComm' :: Channel -> [Channel] -> Converse a -> IO (Either String a)
establishComm' c cs conv = do
  eitherChan <- establishComm c cs
  case eitherChan of
    Left err -> return $ Left err
    Right c -> do
      r <- runConverse c conv
      return $ Right r
    
instance Functor Converse where
  fmap f conv1 = Converse (\c -> do
                   (x,ch) <- chat conv1 c 
                   return (f x, ch) )

instance Applicative Converse where
  pure x = Converse (\c -> return (x,c))
  fab <*> fa = Converse (\c -> do
                (f,ch) <- chat fab c
                (a,ch2) <- chat fa ch
                return (f a,ch2))
    
instance Monad (Converse) where
  return x = Converse (\c -> return (x,c))
  (CommError err) >>= _ = CommError err
  t >>= f = Converse (\c -> do
               (x,ch) <- (chat t) c
               chat (f x) ch)
  fail str = CommError $ "CommError:" ++str {-Converse (\c -> do
               putStrLn $ "Converse Error: " ++ str
               error str)-}

send :: (IsMessage m) => m -> Converse Bool
send m = Converse (\c -> do
          b <- AbstractedCommunication.send c m
          return (b,c))

receive :: (IsMessage m) => Converse m
receive = Converse (\c -> do
            res <- AbstractedCommunication.receive c
            case res of
              Error err -> fail $ "Monad Receive: " ++ err --error err
              Success m -> return (m,c))

eitherReceive :: (IsMessage m) => Converse (Either String m)
eitherReceive = Converse (\c -> do
  res <- AbstractedCommunication.receive c
  case res of
    Error err -> return (Left err,c)
    Success m -> return (Right m, c)
    )

getChannel :: Converse Channel
getChannel = Converse (\c -> return (c,c))

data FailureChannel = FailureChannel deriving (Show)
instance IsChannel FailureChannel where
 send f _ = do
   putStrLn "FailureChannel Send"
   return False
 receive f = return (Error "Failure Receive")
 initialize f = do
  putStrLn "FailureChannel initialize"
 killChan f = putStrLn "FailureChannel killChan"
 toRequest f = return (Data.Aeson.String "FailureChannel toRequest")
 fromRequest v f = return $ Left "FailureChannel fromRequest"
 amend v f = return f     
 defaultChan = return FailureChannel
 chanTypeOf f = show f 
 
 
test :: Converse ()
test = do
   --send (34,46)
   --x <- receive :: Converse (Int,Int)
   
   
   --x <- CommunicationMonad.receive :: Converse String 
   str <- return "this is a string"
   liftIO $ putStrLn (show str)
   return ()