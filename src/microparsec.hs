{-# LANGUAGE TupleSections #-}

module Microparsec where

import Data.Functor
import Control.Monad
import Control.Applicative


-- | Represents a parser. `ParserT s m a` is a parser
--   which parses `[s]` into `m a`. For most purposes,
--   `s` must be a member of `Eq` and `m` a member of
--   `MonadPlus`.
newtype ParserT s m a = P {runParser :: [s] -> m (a, [s])}


instance MonadPlus m => Monad (ParserT s m) where
  return a = P $ \ s -> return (a, s)
  p >>= g = P $ \ s -> runParser p s >>= (\ (a,rs) -> runParser (g a) rs)

instance MonadPlus m => MonadPlus (ParserT s m) where
  mzero = P $ const mzero
  mplus (P a) (P b) = P $ \ s -> a s `mplus` b s

instance MonadPlus m => Functor (ParserT s m) where
  fmap  = liftM

instance MonadPlus m => Applicative (ParserT s m) where
  pure  = return
  (<*>) = ap

instance MonadPlus m => Alternative (ParserT s m) where
  empty = mzero
  (<|>) = mplus


-- | Evaluates a 'ParserT' over an `[s]`. 'parse' differs
--   from 'runParser' only in that 'runParser' returns
--   both the result and the remainder of the list to
--   parse and 'parse' returns only the result.
parse :: Functor m => ParserT s m a -> [s] -> m a
parse p s = fst <$> runParser p s

-- | A type restricted version of 'parse'.
parseMaybe :: ParserT s Maybe a -> [s] -> Maybe a
parseMaybe = parse

-- | Matches any symbol satisfying the provided function
--   and returns the matched symbol.
satisfy ::
  MonadPlus m =>
  (s -> Bool) ->
  ParserT s m s
satisfy p = P f
  where f []     = mzero
        f (x:xs) | p x       = return (x, xs)
                 | otherwise = mzero
                               
-- | Returns the value from the provided 'ParserT' without
--   consuming any input.
peek ::
  Functor m =>
  ParserT s m a ->
  ParserT s m a
peek e = P $ \ ss -> (,ss) <$> parse e ss

-- | Will not consume any input iff the provided parser
--   fails. Provides this functionality only when used
--   on the left hand side of a '<|>' expression.
try ::
  (MonadPlus m, Functor m) =>
  ParserT s m a ->
  ParserT s m a
try e = peek e >> e

-- | Attempts to match the provided pattern first. If
--   that fails it will the default expression.
option ::
  (MonadPlus m, Alternative m) =>
  a
  -> ParserT s m a
  -> ParserT s m a
option d e = try e <|> return d

-- | Matches exactly one instance of the provided symbol.
eq ::
  (MonadPlus m, Eq s) =>
  s ->
  ParserT s m s
eq c = satisfy (==c)

-- | Matches the provided list of symbols.
eqs ::
  (MonadPlus m, Eq s) =>
  [s] ->
  ParserT s m [s]
eqs = mapM eq

-- | Matches any symbol in the provided list.
oneOf ::
  (MonadPlus m, Eq s) =>
  [s] ->
  ParserT s m s
oneOf cs = satisfy (`elem` cs)

-- | Matches any number of symbols followed by a
--   delimiter and returns a list of the matched
--   symbols.
endBy ::
  (MonadPlus m, Alternative m) =>
  ParserT s m s ->
  ParserT s m a' ->
  ParserT s m [s]
endBy e s = many $ e <* s

-- | Matches any number of symbols seperated by a
--   delimiter and returns a list of the matched
--   symbols.
sepBy ::
  (MonadPlus m, Alternative m) =>
  ParserT s m s ->
  ParserT s m a' ->
  ParserT s m [s]
sepBy e s = do { rs <- endBy e s ; (:rs) <$> e }
