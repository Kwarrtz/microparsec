
{-|
Module      : Text.Microparsec
Description : A lightweight parser combinator library inspired by parsec
Copyright   : © 2015 Dathan Ault-McCoy
License     : GPL-3
Maintainer  : kwarrtz@icloud.com
Portability : POSIX

Microparsec is a lightweight parser combinator library sharing
many features with the parsec library. While, on the surface,
microparsec and parsec may seem very similar, there are several
major differences that users should be aware of. First, microparsec
attempts absolutely __no error reporting__. This means that it is
probably not an ideal choice for the parsing of programming languages
or similar applications where error reporting is a critical aspect of
the parser. The second major difference is that the microparsec 'ParserT'
can return /any/ instance of 'MonadPlus', including user defined types.
This allows for somewhat more flexibility in error handling, but it also
requires more care when crafting type declerations. Other than these
differences, those well-versed in parsec should feel at home using
microparsec.

Microparsec is generally very fast, but speed will of course
differ between systems, so I strongly advise running a benchmark
yourself to determine if its performance fits your needs.

(Microparsec, unlike Parsec, does not export type restricted versions
of the 'Alternative' manipulation functions ('many', '<|>', etc.), so
the program must import 'Control.Applicative' to make use of them.)
-}

module Text.Microparsec
       (
         ParserT,
         parse,
         -- * Parser constructors
         satisfy, peek,
         symbol, symbols,
         anySymbol,
         eof,
         try, option, notReq,
         oneOf,
         -- * Parser combinators
         notFollowedBy,
         count,
         choice,
         till,
         endBy1, endBy,
         sepBy1, sepBy,
         sepEndBy1, sepEndBy,
         between,
         -- * Character matching
         space, spaces,
         digit, digits,
         alpha, alphaNum,
         upper, lower
       )
       where

import Data.Functor
import Control.Applicative
import Control.Monad

import Text.Microparsec.Prim
import Text.Microparsec.Core
import Text.Microparsec.Char


-----------------------
-- Parser generators --
-----------------------

-- | Matches a single symbol.
symbol ::
  (MonadPlus m, Eq s) =>
  s ->
  ParserT s m s
symbol c = satisfy (==c)

-- | Matches a list of symbols.
symbols ::
  (MonadPlus m, Eq s) =>
  [s] ->
  ParserT s m [s]
symbols = mapM symbol

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

-- | Parses zero or one occurences of an expression,
--   discarding the result.
notReq ::
  (MonadPlus m, Alternative m) =>
  ParserT s m a' ->
  ParserT s m ()
notReq e = option undefined e >> return ()

-- | Matches any symbol in the provided list.
oneOf ::
  (MonadPlus m, Eq s) =>
  [s] ->
  ParserT s m s
oneOf cs = satisfy (`elem` cs)


------------------------
-- Parser combinators --
------------------------

-- | Matches and returns any symbol.
anySymbol ::
  (MonadPlus m) =>
  ParserT s m s
anySymbol = satisfy $ const True

-- | End of input (does not actually match
--   the unicode eof character, but rather
--   succeeds only when there are no more
--   symbols to parse)
eof ::
  (MonadPlus m, Eq (m (s,[s]))) =>
  ParserT s m ()
eof = notFollowedBy anySymbol >> return ()

-- | Matches one or more expressions followed by a
--   delimiter and returns a list of the matched
--   expressions.
endBy1 ::
  (MonadPlus m, Alternative m) =>
  ParserT s m a ->
  ParserT s m a' ->
  ParserT s m [a]
endBy1 e s = some $ e <* s

-- | Matches zero or more expressions followed by a
--   delimiter and returns a list of the matched
--   expressions.
endBy ::
  (MonadPlus m, Alternative m) =>
  ParserT s m a ->
  ParserT s m a' ->
  ParserT s m [a]
endBy e s = many $ e <* s

-- | Matches one or more expressions seperated by a
--   delimiter and returns a list of the matched
--   expressions.
sepBy1 ::
  (MonadPlus m, Alternative m) =>
  ParserT s m a ->
  ParserT s m a' ->
  ParserT s m [a]
sepBy1 e s = do { rs <- endBy e s ; ((rs++) . (:[])) <$> e }

-- | Matches zero or more expressions seperated by a
--   delimiter and returns a list of the matched
--   expressions.
sepBy ::
  (MonadPlus m, Alternative m) =>
  ParserT s m a ->
  ParserT s m a' ->
  ParserT s m [a]
sepBy e s = sepBy1 e s <|> return []

-- | Matches one or more expressions seperated and
--   optionally ended by a delimeter and returs a
--   list of the matched expressions.
sepEndBy1 ::
  (MonadPlus m, Alternative m) =>
  ParserT s m a ->
  ParserT s m a' ->
  ParserT s m [a]
sepEndBy1 e s = sepBy1 e s <* optional s

-- | Matches zero or more expressions seperated and
--   optionally ended by a delimeter and returs a
--   list of the matched expressions.
sepEndBy ::
  (MonadPlus m, Alternative m) =>
  ParserT s m a ->
  ParserT s m a' ->
  ParserT s m [a]
sepEndBy e s = sepEndBy1 e s <|> return []

-- | Matches an expression surrounded by a symbol.
--   Returns the result of the surrounded expression.
--   Usually used infix.
between ::
  (MonadPlus m, Applicative m) =>
  ParserT s m a ->
  ParserT s m a' ->
  ParserT s m a
between e s = s *> e <* s

-- | Parses a fixed number of occurences of an
--   expression.
count ::
  (MonadPlus m) =>
  Int ->
  ParserT s m a ->
  ParserT s m [a]
count n e = sequence $ replicate n e

-- | Takes a list of parsers, runs each one in order,
--   and returns the value of the first that succeeds.
choice ::
  (MonadPlus m, Alternative m) =>
  [ParserT s m a] ->
  ParserT s m a
choice = foldr (<|>) mzero

-- | Matches zero or more expressions until a termination
--   parser succeeds. The value of the terminator is discarded.
till ::
  (MonadPlus m) =>
  ParserT s m a ->
  ParserT s m a' ->
  ParserT s m [a]
till e x = let go = x *> return [] <|> (:) <$> e <*> go in go
