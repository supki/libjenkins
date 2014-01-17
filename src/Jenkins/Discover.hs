{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- | Discover Jenkins on the network
module Jenkins.Discover
  ( Discover(..)
  , discover
#ifdef TEST
  , parse
#endif
  ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Network.BSD
import           Network.Socket
import           Network.Socket.ByteString as B
import           System.Timeout (timeout)
import qualified Text.XML as X
import qualified Text.XML.Lens as X

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
parse (X.parseLBS X.def . BL.fromStrict -> bs) = case bs of
  Left  _      -> Nothing
  Right parsed ->
    let v = parsed^?deeper.X.el "version".content
        u = parsed^?deeper.X.el "url".content
        s = parsed^?deeper.X.el "server-id".content
    in Discover <$> v <*> u <*> pure s
 where
  content = X.nodes.traverse.X._Content
  deeper  = X.root.X.nodes.traverse.X._Element
