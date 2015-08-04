{-|
Module      : Text.Microparsec.Prim
Description : The primitives of the Microparsec library
Copyright   : (c) 2015 Dathan Ault-McCoy
License     : GPL-3
Maintainer  : kwarrtz@icloud.com
Portability : POSIX

This module contains the most primitive aspects of the
'Microparsec' library. This includes the definition of
'ParserT' and its instances.
-}

module Text.Microparsec.Prim (ParserT(..)) where

import Control.Monad
import Control.Applicative


-- | Represents a parser. `ParserT s m a` is a parser
--   which parses `[s]` into `m a`. For most purposes,
--   `s` must be a member of `Eq` and `m` a member of
--   `MonadPlus`.
newtype ParserT s m a = Parser {runParser :: [s] -> m (a, [s])}


instance MonadPlus m => Monad (ParserT s m) where
  return a = Parser $ \ s -> return (a, s)
  p >>= g = Parser $ \ s -> runParser p s >>= (\ (a,rs) -> runParser (g a) rs)

instance MonadPlus m => MonadPlus (ParserT s m) where
  mzero = Parser $ const mzero
  mplus (Parser a) (Parser b) = Parser $ \ s -> a s `mplus` b s

instance MonadPlus m => Functor (ParserT s m) where
  fmap  = liftM

instance MonadPlus m => Applicative (ParserT s m) where
  pure  = return
  (<*>) = ap

instance MonadPlus m => Alternative (ParserT s m) where
  empty = mzero
  (<|>) = mplus
