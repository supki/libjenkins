{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Discover Jenkins on the network
module Jenkins.Discover
  ( Discover(..)
  , discover
#ifdef TEST
  , parse
#endif
  ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens hiding (element)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Network.BSD
import           Network.Socket
import           Network.Socket.ByteString as B
import           System.Timeout (timeout)
import           Text.XML
import           Text.XML.Cursor

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- | Jenkins information
data Discover = Discover
  { version   :: Text
  , url       :: Text
  , server_id :: Maybe Text
  } deriving (Show, Eq)


-- | Discover Jenkins on the network
discover
  :: Int           -- ^ timeout
  -> IO [Discover]
discover t = do
  (b, addr) <- broadcastSocket
  B.sendTo b (B.pack [0, 0, 0, 0]) addr -- does not matter what to send

  msgs <- while (timeout t (readAnswer b))

  close b
  return (mapMaybe parse msgs)
 where
  while :: IO (Maybe a) -> IO [a]
  while io = go where
    go = do
      mr <- io
      case mr of
        Nothing -> return []
        Just r  -> (r :) <$> go

broadcastSocket :: IO (Socket, SockAddr)
broadcastSocket = do
  s <- getProtocolNumber "udp" >>= socket AF_INET Datagram
  setSocketOption s Broadcast 1
  return (s, SockAddrInet port (-1) {- 255.255.255.255 -})
 where
  port = 33848

readAnswer :: Socket -> IO ByteString
readAnswer s = fst <$> B.recvFrom s 4096


-- | Parse Jenkins discovery response XML
--
-- The \"Scheme\" is as follows:
--
-- @
-- <hudson>
--   <version>...</version>
--   <url>...</url>
--   <server-id>...</server-id>
-- </hudson>
-- @
parse :: ByteString -> Maybe Discover
parse bs = either (const Nothing) Just (parseLBS def (BL.fromStrict bs)) >>= \doc ->
  let
    cursor = fromDocument doc
    tag t  = preview _head (cursor $/ element t &// content)
  in Discover
    <$> tag "version"
    <*> tag "url"
    <*> pure (tag "server-id")
