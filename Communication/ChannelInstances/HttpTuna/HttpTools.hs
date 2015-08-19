module HttpTools where

import Data.Aeson as DA
import Network.Info
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy hiding (putStrLn)

getMyIP :: IO IPv4
getMyIP = do 
  ls <- getNetworkInterfaces 
  let ipp = getMyIPHelper "eth0" ls 
  if ( (show) ipp) /= "0.0.0.0"
    then return (ipp)
    else do 
     return $ getMyIPHelper "xenbr0" ls 
    where
     getMyIPHelper str ls = let ls' = Prelude.filter (\x -> (name x) == str) ls 
                                ni = Prelude.head ls' in 
                            ipv4 ni 

getMyIP' = do
         ipv4 <- getMyIP 
         return (Char8.pack (show ipv4))


