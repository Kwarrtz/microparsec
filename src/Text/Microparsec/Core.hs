{-# LANGUAGE TupleSections, AllowAmbiguousTypes #-}

{-|
Module      : Text.Microparsec.Core
Description : The core of the Microparsec library
Copyright   : (c) 2015 Dathan Ault-McCoy
License     : GPL-3
Maintainer  : kwarrtz@icloud.com
Portability : POSIX

This module contains the most core functions for working
with the 'Microparsec' library. This includes the 'parse'
function and any functions that generate a 'ParserT' directly.
-}

module Text.Microparsec.Core
       (
         parse,
         satisfy,
         peek,
         notFollowedBy
       )
       where

import Data.Functor
import Control.Monad

import Text.Microparsec.Prim

-- | Evaluates a 'ParserT' over an @[s]@.
parse :: Functor m => ParserT s m a -> [s] -> m a
parse p s = fst <$> runParser p s

-- | Matches any symbol satisfying the provided function
--   and returns the matched symbol.
satisfy ::
  MonadPlus m =>
  (s -> Bool) ->
  ParserT s m s
satisfy p = Parser f
  where f []     = mzero
        f (x:xs) | p x       = return (x, xs)
                 | otherwise = mzero
                               
-- | Returns the value from the provided 'ParserT' without
--   consuming any input.
peek ::
  Functor m =>
  ParserT s m a ->
  ParserT s m a
peek e = Parser $ \ ss -> (,ss) <$> parse e ss

-- | Succeds only when the expression fails. It consumes no
--   input and returns @()@.
notFollowedBy ::
  (MonadPlus m, Eq (m (a', [s]))) =>
  ParserT s m a' ->
  ParserT s m ()
notFollowedBy e = Parser $ \ ss -> let r = runParser e ss in
                               if r == mzero then return ((), ss) else mzero
