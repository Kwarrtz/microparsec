{-|
Module      : Text.Microparsec.Char
Description : Character matching combinators for the Microparsec library
Copyright   : (c) 2015 Dathan Ault-McCoy
License     : GPL-3
Maintainer  : kwarrtz@icloud.com
Portability : POSIX

This module contains useful functions for matching
various types of characters.
-}

module Text.Microparsec.Char
       (
         space, spaces,
         digit, digits,
         alpha, alphaNum,
         upper, lower
       )
       where

import Control.Monad
import Control.Applicative
import Data.Char

import Text.Microparsec.Prim
import Text.Microparsec.Core

space, digit, alpha, alphaNum, upper, lower ::
  (MonadPlus m) =>
  ParserT Char m Char

spaces, digits ::
  (MonadPlus m, Alternative m) =>
  ParserT Char m [Char]

-- | Whitespace character
space = satisfy isSpace

-- | Many whitespace characters
spaces = many space

-- | A digit
digit = satisfy isDigit

-- | Many digits
digits = many digit

-- | Alphabetical character
alpha = satisfy isAlpha

-- | Alphanumeric character
alphaNum = satisfy isAlphaNum

-- | Uppercase letter
upper = satisfy isUpper

-- | Lowercase letter
lower = satisfy isLower


