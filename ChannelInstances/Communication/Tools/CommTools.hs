{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module CommTools where

import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.ByteString.Base64 as Base64
--import Data.Monoid (mconcat)
--import Data.Maybe
--import Network.Http.Client
--import qualified Network.HTTP.Base as Base
--import qualified Network.URI as UR
import Control.Applicative ( (<$>), (<*>), pure )
--import qualified Data.HashMap.Strict as HM (member, lookup)
import VChanUtil (getDomId)
import Control.Monad.State.Strict
--import System.IO.Streams (InputStream, OutputStream, stdout)
--import qualified System.IO.Streams as Streams
import qualified Data.Aeson as DA
import Data.Aeson
import qualified Network.Http.Client as HttpClient
import System.IO
--import Data.Word
--import Control.Concurrent.STM.TMVar
--import Control.Monad.STM

--mport Data.Bits (shiftR)
import Network.Info 

--import System.Timeout
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as Char8

--import Data.Word (Word16)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
--foreign export converseWithScottyCA :: AD.CARequest -> IO (Either String AD.CAResponse)

--import qualified System.IO.Streams.Internal as StreamsI



type ID = String


ip="10.100.0.6" -- "192.168.122.1"







mylift :: a -> IO a
mylift x = return x


logf ::String -> IO ()
logf m = do 
  h <- openFile "log.1" AppendMode
  hPutStrLn h (m ++ "\n") 
  hClose h
--
--logf' :: String -> ArmoredStateTMonad ()
--logf' = (liftIO . logf)


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


getMyDomId :: IO Int
getMyDomId = getDomId 


whoAmI :: Role -> IO Entity 
whoAmI r = do 
  i@(IPv4 myIP) <- getMyIP
  myID <- getMyDomId
  return $ Entity {
       entityName =show r --  "Attester the Magnificent"
     , entityIp = Just (Char8.pack (show i))      
     , entityId = Just myID 
     , entityRole = r 
     , entityNote = Nothing
     } 

whoAmI' :: Role -> IO Entity 
whoAmI' r = do 
  i@(IPv4 myIP) <- getMyIP
--  myID <- getMyDomId
  return $ Entity {
       entityName =show r --  "Attester the Magnificent"
     , entityIp = Just (Char8.pack (show i))      
     , entityId = Nothing 
     , entityRole = r 
     , entityNote = Nothing
     }

jsonEncode :: (ToJSON a) => a -> ByteString
jsonEncode = DA.encode

jsonEitherDecode :: (FromJSON a) => ByteString -> Either String a
jsonEitherDecode = DA.eitherDecode

jsonDecode :: (FromJSON a) => ByteString -> Maybe a
jsonDecode= DA.decode


jsonParse :: (FromJSON a) => ByteString -> Result a
jsonParse bs =case Data.Aeson.eitherDecode bs of 
                Left err -> Error err
                Right x  -> Success x

instance ToJSON B.ByteString where
	toJSON = DA.String . encodeToText
instance FromJSON B.ByteString where
	parseJSON (DA.String str) = pure $ decodeFromText str	
				 		         
encodeToText :: B.ByteString -> T.Text
encodeToText = TE.decodeUtf8 . Base64.encode

decodeFromText :: T.Text -> B.ByteString
decodeFromText = {-either fail return .-} Base64.decodeLenient . TE.encodeUtf8

decodeFromTextL :: (Monad m) => T.Text -> m ByteString
decodeFromTextL x = let bs = decodeFromText x in
		       return (fromStrict bs)  

decodeFromTextLStayStrict :: (Monad m) => T.Text -> m B.ByteString
decodeFromTextLStayStrict x = let bs = decodeFromText x in
		       return (bs)  


decodeFromTextL' :: T.Text -> ByteString
decodeFromTextL' x = let bs = decodeFromText x in
		       fromStrict bs

data Role = Appraiser
    	  | Attester
	  | Measurer
          | PrivacyCA 
          | Undetermined deriving ( Eq, Show)


            
data Entity = Entity {
	        entityName    :: String,
	        entityIp   :: Maybe HttpClient.Hostname,	   
	        entityId   :: Maybe Int,
	        entityRole :: Role,
	        entityNote :: Maybe String
	      }
	     deriving (Eq,  Show)	    

instance ToJSON Entity where
	toJSON (Entity name mip mid role mnote) = object [ "EntityName" .= name
						         , "EntityIp"   .= toJSON mip
						         , "EntityId"   .= mid
						         , "EntityRole" .= toJSON role
						         , "EntityNote" .= mnote
						         ]
instance FromJSON Entity where
	parseJSON (DA.Object o) = Entity <$> o .: "EntityName"
					 <*> o .: "EntityIp"
					 <*> o .: "EntityId"
					 <*> o .: "EntityRole"
					 <*> o .: "EntityNote"
					 
instance ToJSON Role where
	toJSON Appraiser = DA.String "Appraiser"
	toJSON Attester  = DA.String "Attester"
	toJSON Measurer  = DA.String "Measurer"
        toJSON PrivacyCA = DA.String "PrivacyCA"
        toJSON Undetermined = DA.String "Undetermined"				 
        
instance FromJSON Role where
	parseJSON (DA.String "Appraiser") = pure Appraiser
	parseJSON (DA.String "Attester")  = pure Attester
	parseJSON (DA.String "Measurer")  = pure Measurer
	parseJSON (DA.String "PrivacyCA") = pure PrivacyCA        
        parseJSON (DA.String "Undetermined") = pure Undetermined
					                 
