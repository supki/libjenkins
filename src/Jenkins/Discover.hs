{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}
-- | Discover Jenkins on the network
module Jenkins.Discover
  ( Discover(..)
  , discover
#ifdef TEST
  , parseXml
#endif
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Network.BSD (getProtocolNumber)
import           Network.Socket
import           Network.Socket.ByteString as ByteString
import           System.Timeout (timeout)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- | Jenkins information
data Discover = Discover
  { version  :: Text
  , url      :: Text
  , port     :: Maybe Text
  , serverId :: Maybe Text
  } deriving (Show, Eq)


-- | Discover Jenkins on the network
discover
  :: Int           -- ^ timeout
  -> IO [Discover]
discover t = do
  (b, addr) <- broadcastSocket
  _ <- ByteString.sendTo b (ByteString.pack [0, 0, 0, 0]) addr -- does not matter what to send

  msgs <- while (timeout t (readAnswer b))

  close b
  return (mapMaybe parseXml msgs)
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
  return (s, SockAddrInet p (-1) {- 255.255.255.255 -})
 where
  p = 33848

readAnswer :: Socket -> IO ByteString
readAnswer s = fst <$> ByteString.recvFrom s 4096


-- | Parse Jenkins discovery response XML
parseXml :: ByteString -> Maybe Discover
parseXml = fromMap <=< either (\_ -> Nothing) Just . parseOnly (parser <* endOfInput) . Text.decodeUtf8

fromMap :: Map Text Text -> Maybe Discover
fromMap m = do
  v <- Map.lookup "version" m
  u <- Map.lookup "url" m
  i <- return (Map.lookup "server-id" m)
  p <- return (Map.lookup "slave-port" m)
  return Discover { version = v, url = u, serverId = i, port = p }

parser :: Parser (Map Text Text)
parser = string "<hudson>" *> tags <* string "</hudson>"

tags :: Parser (Map Text Text)
tags = Map.fromList <$> many tag

tag :: Parser (Text, Text)
tag = do
  _ <- char '<'
  k <- takeWhile1 (/= '>')
  _ <- char '>'
  v <- takeWhile1 (/= '<')
  _ <- string ("</" <> k <> ">")
  return (k, v)
