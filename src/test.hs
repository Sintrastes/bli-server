{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- module MyMicroService where

import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.ByteString.Lazy as BS
import Network.Shed.Httpd
import Data.Aeson
import GHC.Generics
import Database.SQLite.Simple
import Data.Time.Clock
import Data.Time.Calendar
import Control.Applicative
import Network.URI

-- Format "getCurrentTime" for my purposes.
getTime :: IO String
getTime = getCurrentTime >>= (\x -> return $ take 19 $ show x)

port :: Int
port = 42069

databasePath :: String
databasePath = "test.db"

data HTTPMethod =
 Get    | Head    | Post    | Put |
 Delete | Connect | Options |
 Trace  | Patch

instance Show HTTPMethod where
  show Get     = "GET"
  show Head    = "HEAD"
  show Post    = "POST"
  show Put     = "PUT"
  show Delete  = "DELETE"
  show Connect = "CONNECT"
  show Options = "OPTIONS"
  show Trace   = "TRACE"
  show Patch   = "PATCH"

parseHTTPMethods :: String -> Maybe HTTPMethod
parseHTTPMethods s
  | s == show Get     = Just Get
  | s == show Head    = Just Head
  | s == show Post    = Just Post
  | s == show Delete  = Just Delete
  | s == show Connect = Just Connect
  | s == show Options = Just Options
  | s == show Trace   = Just Trace
  | s == show Patch   = Just Patch

data JournalEntry = JournalEntry {
      datetime :: String
    , content'  :: String
} deriving (Generic, Show)

instance ToJSON JournalEntry where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JournalEntry

instance ToRow JournalEntry where
  toRow r = toRow (datetime r, content' r)

data JournalEntryNoTime = JournalEntryNoTime {
  content :: String
} deriving (Generic, Show)

instance ToJSON JournalEntryNoTime where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JournalEntryNoTime

-- I'm not entirely sure how this magic works, but it appears to work.
instance FromRow JournalEntry where
  fromRow = JournalEntry <$> field <*> field

requestHandler :: Request -> IO Response
requestHandler r = case reqMethod r of
    "GET"  -> if (uriPath $ reqURI r) == "/journals"
              then (do conn <- open databasePath
                       r <- query_ conn "SELECT * FROM journals" :: IO [JournalEntry]
                       return $ Response { resCode = 0,
                                           resHeaders = [],
                                           resBody = B.toString $ encode r })
              else if (uriPath $ reqURI r) == "journals/"
              then return $ Response { resCode = 0,
                                     resHeaders = [],
                                     resBody = "Returning a specific journal" }
              else return $ Response { resCode = 400,
                                     resHeaders = [],
                                     resBody = "Malformed request" }
    "POST" -> if (uriPath $ reqURI r) == "/journals" then
	      do conn <- open databasePath
    	         let body = decode $ B.fromString $ reqBody r :: Maybe JournalEntryNoTime
                 case body of
		     Just journalEntry -> do
		         t <- getTime
		         let journalEntryWithTime = JournalEntry {
			       datetime = t,
			       content' = content journalEntry
			  }
		         execute conn "INSERT INTO journals (datetime, content) VALUES (?, ?)"
			     journalEntryWithTime
		         return $ Response { resCode = 0,
                                    resHeaders = [],
                                    resBody = "You succesfully published a new journal entry at "
                                       ++ (uriPath $ reqURI r) ++ ". Congrats." }
		     Nothing -> return $
		        Response { resCode = 400,
                                   resHeaders = [],
				   resBody = "" }
	     else return $ Response { resCode = 400,
                                      resHeaders = [],
			              resBody = "Only the resource \"\\journals\" may be posted to." }
    otherwise -> return $ Response { resCode = 501,
		                     resHeaders = [],
                                     resBody = "API feature not implemented" }

main :: IO ()
main = initServer port requestHandler 

