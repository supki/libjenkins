{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Optics for 'Network.HTTP.Conduit' types
module Network.HTTP.Conduit.Lens
  ( -- * 'Request' lenses
    method, secure
  , host, port
  , path, queryString, requestBody, requestHeaders
  , redirectCount, checkStatus, responseTimeout
    -- * 'HttpException' prisms
  , AsHttpException(..)
  , _StatusCodeException
  , _InvalidUrlException
  , _TooManyRedirects
  , _UnparseableRedirect
  , _TooManyRetries
  , _HttpParserException
  , _HandshakeFailed
  , _OverlongHeaders
  , _ResponseTimeout
  , _FailedConnectionException
  , _ExpectedBlankAfter100Continue
  , _InvalidStatusLine
  , _InvalidHeader
  , _InternalIOException
  , _ProxyConnectException
  , _NoResponseDataReceived
  , _TlsException
  , _TlsNotSupported
  , _ResponseBodyTooShort
  , _InvalidChunkHeaders
  , _IncompleteHeaders
  ) where

import           Control.Applicative
import           Control.Exception (SomeException, IOException)
import           Control.Exception.Lens (exception)
import           Control.Lens
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Word (Word64)
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as H


-- | HTTP request method, eg GET, POST.
method :: Lens' H.Request H.Method
method f req = (\m' -> req { H.method = m' }) <$> f (H.method req)
{-# INLINE method #-}

-- | Whether to use HTTPS (ie, SSL).
secure :: Lens' H.Request Bool
secure f req = (\s' -> req { H.secure = s' }) <$> f (H.secure req)
{-# INLINE secure #-}

host :: Lens' H.Request ByteString
host f req = (\h' -> req { H.host = h' }) <$> f (H.host req)
{-# INLINE host #-}

port :: Lens' H.Request Int
port f req = (\p' -> req { H.port = p' }) <$> f (H.port req)
{-# INLINE port #-}

-- | Everything from the host to the query string.
path :: Lens' H.Request ByteString
path f req = (\p' -> req { H.path = p' }) <$> f (H.path req)
{-# INLINE path #-}

queryString :: Lens' H.Request ByteString
queryString f req = (\qs' -> req { H.queryString = qs' }) <$> f (H.queryString req)
{-# INLINE queryString #-}

requestBody :: Lens' H.Request H.RequestBody
requestBody f req = (\rb' -> req { H.requestBody = rb' }) <$> f (H.requestBody req)
{-# INLINE requestBody #-}

requestHeaders :: Lens' H.Request H.RequestHeaders
requestHeaders f req = (\rh' -> req { H.requestHeaders = rh' }) <$> f (H.requestHeaders req)
{-# INLINE requestHeaders #-}

-- | How many redirects to follow when getting a resource. 0 means follow no redirects. Default value: 10.
redirectCount :: Lens' H.Request Int
redirectCount f req = (\rc' -> req { H.redirectCount = rc' }) <$> f (H.redirectCount req)
{-# INLINE redirectCount #-}

-- | Check the status code. Note that this will run after all redirects are performed.
-- Default: return a StatusCodeException on non-2XX responses.
checkStatus :: Lens' H.Request (H.Status -> H.ResponseHeaders -> H.CookieJar -> Maybe SomeException)
checkStatus f req = (\cs' -> req { H.checkStatus = cs' }) <$> f (H.checkStatus req)
{-# INLINE checkStatus #-}

-- | Number of microseconds to wait for a response. If Nothing, will wait indefinitely. Default: 5 seconds.
responseTimeout :: Lens' H.Request (Maybe Int)
responseTimeout f req = (\rt' -> req { H.responseTimeout = rt' }) <$> f (H.responseTimeout req)
{-# INLINE responseTimeout #-}


-- | @http-conduit@ exceptions
class AsHttpException p f t where
  -- | @http-conduit@ exceptions overloading
  _HttpException :: Overloaded' p f t H.HttpException

instance AsHttpException p f H.HttpException where
  _HttpException = id
  {-# INLINE _HttpException #-}

instance (Choice p, Applicative f) => AsHttpException p f SomeException where
  _HttpException = exception
  {-# INLINE _HttpException #-}

-- | 'H.StatusCodeException' exception
_StatusCodeException
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t (H.Status, H.ResponseHeaders, H.CookieJar)
_StatusCodeException = _HttpException . prism' (uncurry3 H.StatusCodeException) go where
  go (H.StatusCodeException s rh cj) = Just (s, rh, cj)
  go _ = Nothing
{-# INLINE _StatusCodeException #-}

-- | 'H.InvalidUrlException' exception
_InvalidUrlException
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t (String, String)
_InvalidUrlException = _HttpException . prism' (uncurry H.InvalidUrlException) go where
  go (H.InvalidUrlException s s') = Just (s, s')
  go _ = Nothing
{-# INLINE _InvalidUrlException #-}

-- | 'H.TooManyRedirects' exception
_TooManyRedirects
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t [H.Response Lazy.ByteString]
_TooManyRedirects = _HttpException . prism' H.TooManyRedirects go where
  go (H.TooManyRedirects rs) = Just rs
  go _ = Nothing
{-# INLINE _TooManyRedirects #-}

-- | 'H.UnparseableRedirect' exception
_UnparseableRedirect
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t (H.Response Lazy.ByteString)
_UnparseableRedirect = _HttpException . prism' H.UnparseableRedirect go where
  go (H.UnparseableRedirect r) = Just r
  go _ = Nothing
{-# INLINE _UnparseableRedirect #-}

-- | 'H.TooManyRetries' exception
_TooManyRetries
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ()
_TooManyRetries = _HttpException . prism' (const H.TooManyRetries) go where
  go H.TooManyRetries = Just ()
  go _ = Nothing
{-# INLINE _TooManyRetries #-}

-- | 'H.HttpParserException' exception
_HttpParserException
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t String
_HttpParserException = _HttpException . prism' H.HttpParserException go where
  go (H.HttpParserException s) = Just s
  go _ = Nothing
{-# INLINE _HttpParserException #-}

-- | 'H.HandshakeFailed' exception
_HandshakeFailed
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ()
_HandshakeFailed = _HttpException . prism' (const H.HandshakeFailed) go where
  go H.HandshakeFailed = Just ()
  go _ = Nothing
{-# INLINE _HandshakeFailed #-}

-- | 'H.OverlongHeaders' exception
_OverlongHeaders
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ()
_OverlongHeaders = _HttpException . prism' (const H.OverlongHeaders) go where
  go H.OverlongHeaders = Just ()
  go _ = Nothing
{-# INLINE _OverlongHeaders #-}

-- | 'H.ResponseTimeout' exception
_ResponseTimeout
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ()
_ResponseTimeout = _HttpException . prism' (const H.ResponseTimeout) go where
  go H.ResponseTimeout = Just ()
  go _ = Nothing
{-# INLINE _ResponseTimeout #-}

-- | 'H.FailedConnectionException' exception
_FailedConnectionException
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t (String, Int)
_FailedConnectionException = _HttpException . prism' (uncurry H.FailedConnectionException) go where
  go (H.FailedConnectionException s i) = Just (s, i)
  go _ = Nothing
{-# INLINE _FailedConnectionException #-}

-- | 'H.ExpectedBlankAfter100Continue' exception
_ExpectedBlankAfter100Continue
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ()
_ExpectedBlankAfter100Continue =
  _HttpException . prism' (const H.ExpectedBlankAfter100Continue) go where
    go H.ExpectedBlankAfter100Continue = Just ()
    go _ = Nothing
{-# INLINE _ExpectedBlankAfter100Continue #-}

-- | 'H.InvalidStatusLine' exception
_InvalidStatusLine
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ByteString
_InvalidStatusLine = _HttpException . prism' H.InvalidStatusLine go where
  go (H.InvalidStatusLine b) = Just b
  go _ = Nothing
{-# INLINE _InvalidStatusLine #-}

-- | 'H.InvalidHeader' exception
_InvalidHeader
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ByteString
_InvalidHeader = _HttpException . prism' H.InvalidHeader go where
  go (H.InvalidHeader b) = Just b
  go _ = Nothing
{-# INLINE _InvalidHeader #-}

-- | 'H.InternalIOException' exception
_InternalIOException
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t IOException
_InternalIOException = _HttpException . prism' H.InternalIOException go where
  go (H.InternalIOException ioe) = Just ioe
  go _ = Nothing
{-# INLINE _InternalIOException #-}

-- | 'H.ProxyConnectException' exception
_ProxyConnectException
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t (ByteString, Int, Either ByteString H.HttpException)
_ProxyConnectException = _HttpException . prism' (uncurry3 H.ProxyConnectException) go where
  go (H.ProxyConnectException b i ebhe) = Just (b, i, ebhe)
  go _ = Nothing
{-# INLINE _ProxyConnectException #-}

-- | 'H.NoResponseDataReceived' exception
_NoResponseDataReceived
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ()
_NoResponseDataReceived = _HttpException . prism' (const H.NoResponseDataReceived) go where
  go H.NoResponseDataReceived = Just ()
  go _ = Nothing
{-# INLINE _NoResponseDataReceived #-}

-- | 'H.TlsException' exception
_TlsException
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t SomeException
_TlsException = _HttpException . prism' H.TlsException go where
  go (H.TlsException se) = Just se
  go _ = Nothing
{-# INLINE _TlsException #-}

-- | 'H.TlsNotSupported' exception
_TlsNotSupported
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ()
_TlsNotSupported = _HttpException . prism' (const H.TlsNotSupported) go where
  go H.TlsNotSupported = Just ()
  go _ = Nothing
{-# INLINE _TlsNotSupported #-}

-- | 'H.ResponseBodyTooShort' exception
_ResponseBodyTooShort
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t (Word64, Word64)
_ResponseBodyTooShort = _HttpException . prism' (uncurry H.ResponseBodyTooShort) go where
  go (H.ResponseBodyTooShort w w') = Just (w, w')
  go _ = Nothing
{-# INLINE _ResponseBodyTooShort #-}

-- | 'H.InvalidChunkHeaders' exception
_InvalidChunkHeaders
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ()
_InvalidChunkHeaders = _HttpException . prism' (const H.InvalidChunkHeaders) go where
  go H.InvalidChunkHeaders = Just ()
  go _ = Nothing
{-# INLINE _InvalidChunkHeaders #-}

-- | 'H.IncompleteHeaders' exception
_IncompleteHeaders
  :: (AsHttpException p f t, Choice p, Applicative f)
  => Overloaded' p f t ()
_IncompleteHeaders = _HttpException . prism' (const H.IncompleteHeaders) go where
  go H.IncompleteHeaders = Just ()
  go _ = Nothing
{-# INLINE _IncompleteHeaders #-}

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
{-# INLINE uncurry3 #-}
