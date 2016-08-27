{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Optics for @http-client@ types
module Network.HTTP.Client.Lens
  ( -- * 'Request' lenses
    method
  , secure
  , host
  , port
  , path
  , queryString
  , requestBody
  , requestHeaders
  , proxy
  , hostAddress
  , rawBody
  , decompress
  , redirectCount
  , checkStatus
  , responseTimeout
  , cookieJar
  , getConnectionWrapper
    -- * 'HttpException' prisms
  , AsHttpException(..)
  ) where

import           Control.Exception (SomeException)
import           Data.ByteString (ByteString)
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.Internal as H
import qualified Network.HTTP.Types as H
import           Network.Socket (HostAddress)

import           Network.HTTP.Client.Lens.Internal


-- | 'H.method' lens
method :: Lens' H.Request H.Method
method f req = f (H.method req) <&> \m' -> req { H.method = m' }
{-# INLINE method #-}

-- | 'H.secure' lens
secure :: Lens' H.Request Bool
secure f req = f (H.secure req) <&> \s' -> req { H.secure = s' }
{-# INLINE secure #-}

-- | 'H.host' lens
host :: Lens' H.Request ByteString
host f req = f (H.host req) <&> \h' -> req { H.host = h' }
{-# INLINE host #-}

-- | 'H.port' lens
port :: Lens' H.Request Int
port f req = f (H.port req) <&> \p' -> req { H.port = p' }
{-# INLINE port #-}

-- | 'H.path' lens
path :: Lens' H.Request ByteString
path f req = f (H.path req) <&> \p' -> req { H.path = p' }
{-# INLINE path #-}

-- | 'H.queryString' lens
queryString :: Lens' H.Request ByteString
queryString f req = f (H.queryString req) <&> \qs' -> req { H.queryString = qs' }
{-# INLINE queryString #-}

-- | 'H.requestBody' lens
requestBody :: Lens' H.Request H.RequestBody
requestBody f req = f (H.requestBody req) <&> \rb' -> req { H.requestBody = rb' }
{-# INLINE requestBody #-}

-- | 'H.requestHeaders' lens
requestHeaders :: Lens' H.Request H.RequestHeaders
requestHeaders f req = f (H.requestHeaders req) <&> \rh' -> req { H.requestHeaders = rh' }
{-# INLINE requestHeaders #-}

-- | 'H.proxy'
proxy :: Lens' H.Request (Maybe H.Proxy)
proxy f req = f (H.proxy req) <&> \mp' -> req { H.proxy = mp' }
{-# INLINE proxy #-}

-- | 'H.hostAddress'
hostAddress :: Lens' H.Request (Maybe HostAddress)
hostAddress f req = f (H.hostAddress req) <&> \ha' -> req { H.hostAddress = ha' }
{-# INLINE hostAddress #-}

-- | 'H.rawBody'
rawBody :: Lens' H.Request Bool
rawBody f req = f (H.rawBody req) <&> \b' -> req { H.rawBody = b' }
{-# INLINE rawBody #-}

-- | 'H.decompress'
decompress :: Lens' H.Request (ByteString -> Bool)
decompress f req = f (H.decompress req) <&> \btb' -> req { H.decompress = btb' }
{-# INLINE decompress #-}

-- | 'H.redirectCount' lens
redirectCount :: Lens' H.Request Int
redirectCount f req = f (H.redirectCount req) <&> \rc' -> req { H.redirectCount = rc' }
{-# INLINE redirectCount #-}

-- | 'H.checkStatus' lens
checkStatus :: Lens' H.Request (H.Status -> H.ResponseHeaders -> H.CookieJar -> Maybe SomeException)
checkStatus f req = f (H.checkStatus req) <&> \cs' -> req { H.checkStatus = cs' }
{-# INLINE checkStatus #-}

-- | 'H.responseTimeout' lens
responseTimeout :: Lens' H.Request (Maybe Int)
responseTimeout f req = f (H.responseTimeout req) <&> \rt' -> req { H.responseTimeout = rt' }
{-# INLINE responseTimeout #-}

-- | 'H.cookieJar'
cookieJar :: Lens' H.Request (Maybe H.CookieJar)
cookieJar f req = f (H.cookieJar req) <&> \mcj' -> req { H.cookieJar = mcj' }
{-# INLINE cookieJar #-}

-- | 'H.getConnectionWrapper'
getConnectionWrapper
  :: Lens' H.Request
      ( Maybe Int
      -> H.HttpException
      -> IO (H.ConnRelease, H.Connection, H.ManagedConn)
      -> IO (Maybe Int, (H.ConnRelease, H.Connection, H.ManagedConn))
      )
getConnectionWrapper f req =
  f (H.getConnectionWrapper req) <&> \wat' -> req { H.getConnectionWrapper = wat' }
{-# INLINE getConnectionWrapper #-}


-- | @http-conduit@ exceptions
class AsHttpException t where
  -- | @http-conduit@ exceptions overloading
  _HttpException :: Prism' t H.HttpException

instance AsHttpException H.HttpException where
  _HttpException = id
  {-# INLINE _HttpException #-}

instance AsHttpException SomeException where
  _HttpException = exception
  {-# INLINE _HttpException #-}
