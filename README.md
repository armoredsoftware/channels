# channels
Definition and implementation of various channel models for communication among ArmoredSoftware components

MAIN POINTS:
To create your end of the conversation, use the Converse Monad.
Example:

import CommunicationMonad --(from communicationmonad package)
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
  
The important parts here are of course send and receive. 

send:
Anything that has ToJSON and FromJSON instances can be sent. 

receive:
Simply cast a receive as the thing you expect to receive (wrapped in the 
Converse monad). An improper cast will currently result in an IO error thrown.

METHODS:

  Server side (i.e. what to do when someone starts to talk with me):
     The easiest way to do this is to use the default implementation I've
     provided in the DefaultComm module (from the armoredcommunication package)
     The default channel options here are currently vchan and httpTuna with an
     httpTuna negotiator.
     
     METHOD: declareDefaultComm' :: Converse a -> IO () 
       Arguments:
         Converse a: 
           This method will forever run the provided Converse monad when a 
           channel is successfully established. 
       Result:
         Simply a () 
  Client side (i.e. when you want to talk to someone)
     The easiest way to do this is to use the default implementation I've
     provided in the DefaultComm module (from the armoredcommunication package)
     The default channel options here are currently vchan and httpTuna with an
     httpTuna negotiator.
     
     METHOD: talkTo :: Hostname -> Converse a -> IO (Either String a)
       Arguments: 
         Hostname: 
           Simply the IP address to whom you wish to converse (It's a 
           ByteString, so you'll need the OverloadedStrings flag).
         Converse a:
           Your side of the conversation to be ran upon successful contact. 
           
       Result:
       If this action fails for any reason (will catch IO error from improper
       casting of receives for instance) a "Left String" will be returned 
       reporting the reason. Upon successful completion of the conversation,
       a "Right a" is returned where "a" is the Type from "Converse a"
