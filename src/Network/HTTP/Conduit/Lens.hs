-- | Lenses for 'Network.HTTP.Conduit' types
module Network.HTTP.Conduit.Lens
  ( -- * 'Request' lenses
    method, secure
  , host, port
  , path, queryString, requestBody, requestHeaders
  , redirectCount, checkStatus, responseTimeout
  ) where

import           Control.Applicative ((<$>))
import           Control.Exception (SomeException)
import           Control.Lens
import           Data.ByteString (ByteString)
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as H


-- | HTTP request method, eg GET, POST.
method :: Lens' (H.Request m) H.Method
method f req = (\m' -> req { H.method = m' }) <$> f (H.method req)

-- | Whether to use HTTPS (ie, SSL).
secure :: Lens' (H.Request m) Bool
secure f req = (\s' -> req { H.secure = s' }) <$> f (H.secure req)

host :: Lens' (H.Request m) ByteString
host f req = (\h' -> req { H.host = h' }) <$> f (H.host req)

port :: Lens' (H.Request m) Int
port f req = (\p' -> req { H.port = p' }) <$> f (H.port req)

-- | Everything from the host to the query string.
path :: Lens' (H.Request m) ByteString
path f req = (\p' -> req { H.path = p' }) <$> f (H.path req)

queryString :: Lens' (H.Request m) ByteString
queryString f req = (\qs' -> req { H.queryString = qs' }) <$> f (H.queryString req)

requestBody :: Lens' (H.Request m) (H.RequestBody m)
requestBody f req = (\rb' -> req { H.requestBody = rb' }) <$> f (H.requestBody req)

requestHeaders :: Lens' (H.Request m) H.RequestHeaders
requestHeaders f req = (\rh' -> req { H.requestHeaders = rh' }) <$> f (H.requestHeaders req)

-- | How many redirects to follow when getting a resource. 0 means follow no redirects. Default value: 10.
redirectCount :: Lens' (H.Request m) Int
redirectCount f req = (\rc' -> req { H.redirectCount = rc' }) <$> f (H.redirectCount req)

-- | Check the status code. Note that this will run after all redirects are performed.
-- Default: return a StatusCodeException on non-2XX responses.
checkStatus :: Lens' (H.Request m) (H.Status -> H.ResponseHeaders -> H.CookieJar -> Maybe SomeException)
checkStatus f req = (\cs' -> req { H.checkStatus = cs' }) <$> f (H.checkStatus req)

-- | Number of microseconds to wait for a response. If Nothing, will wait indefinitely. Default: 5 seconds.
responseTimeout :: Lens' (H.Request m) (Maybe Int)
responseTimeout f req = (\rt' -> req { H.responseTimeout = rt' }) <$> f (H.responseTimeout req)
