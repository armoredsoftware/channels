{-# LANGUAGE ViewPatterns, RecordWildCards, ConstraintKinds #-}
module CommunicationMonad where

import Control.Monad.Trans.State.Strict hiding (get, put)
import qualified Control.Monad.Trans.State.Strict as S 
import AbstractedCommunication
import qualified AbstractedCommunication as A 
import Data.Aeson
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class
class (Monad m) => MonadCommunication m where
  send :: (IsMessage b) => b ->  m Bool
  receive :: m a
  getChannel :: m Channel 

--runCommunication :: (ToJSON a) => Channel -> [Channel] -> Maybe a -> (s,b)
--runCommunicatio
newtype CommMonad s a = CommMonad  (StateT (Channel,s) IO a)


instance Monad (CommMonad s) where
  return a = CommMonad $ state $ \s -> (a, s)
  (CommMonad m) >>= k  = CommMonad $ StateT $ \s -> do
        ~(a, s') <- runStateT m s
        let (CommMonad p) = k a 
        runStateT p s'
  fail str = CommMonad $ StateT $ \_ -> fail str

instance  MonadIO (CommMonad m) where
  liftIO (x) = CommMonad ( liftIO x) 

get :: CommMonad a b
get = do
  (c,s) <- S.get
  return s 
instance MonadCommunication (CommMonad s) where
  getChannel = do
   (c,_) <- S.get
   return c 
  send x = do
    c <- getChannel
    r <- liftIO $ A.send c x 
    return r
  receive = do
    c <- getChannel
    mess <- liftIO $ A.receive c 
    return () 


    
