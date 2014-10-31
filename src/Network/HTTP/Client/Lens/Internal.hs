{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK hide #-}
module Network.HTTP.Client.Lens.Internal where

import Control.Applicative
import Control.Exception (Exception(..), SomeException)
import Data.Profunctor (Profunctor(..), Choice(..))


type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a


view :: Lens' s a -> s -> a
view l = getConst . l Const
{-# INLINE view #-}

(^.) :: s -> Lens' s a -> a
(^.) = flip view
{-# INLINE (^.) #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

exception :: Exception a => Prism' SomeException a
exception = prism' toException fromException
{-# INLINE exception #-}

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
{-# INLINE (<&>) #-}
