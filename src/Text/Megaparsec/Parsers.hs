-- |
-- A newtype wrapper for 'Parsec.ParsecT' that has instances of 'Parsing', 'CharParsing',
-- 'LookAheadParsing', and 'TokenParsing'.
--
-- 'Parsing' and 'LookAheadParsing' have instances for any 'Stream' instance.
--
-- 'CharParsing' and 'TokenParsing' only have instances for 'String', strict 'Text.Text',
-- and lazy 'Lazy.Text', because those type classes expect the 'Token' type to be 'Char'

{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
module Text.Megaparsec.Parsers
  ( module Text.Parser.Combinators
  , module Text.Parser.Char
  , module Text.Parser.LookAhead
  , module Text.Parser.Token
  , ParsecT(..)
  )
where

import Control.Applicative (Applicative, Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans (MonadTrans)
import Data.Monoid (Monoid)
import Data.Functor ((<$))
import Data.Semigroup (Semigroup)
import Data.String (fromString)
import Text.Megaparsec (MonadParsec, Stream, Token)
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.LookAhead
import Text.Parser.Token

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Parsec

newtype ParsecT e s m a
  = ParsecT { unParsecT :: Parsec.ParsecT e s m a }
  deriving
    ( Functor, Applicative, Alternative, Monad, MonadPlus
    , MonadParsec e s, MonadError e', MonadReader r, MonadState st
    , MonadTrans, MonadFail, MonadIO, MonadCont, Semigroup, Monoid
    )

-- | Note: 'unexpected' requires a non-empty string
instance (Ord e, Stream s) => Parsing (ParsecT e s m) where
  {-# inline try#-}
  try = Parsec.try

  {-# inline (<?>) #-}
  (<?>) = (Parsec.<?>)

  {-# inline notFollowedBy #-}
  notFollowedBy = Parsec.notFollowedBy

  {-# inline eof #-}
  eof = Parsec.eof

  {-# inline unexpected #-}
  unexpected = Parsec.unexpected . Parsec.Label . NonEmpty.fromList

instance Ord e => CharParsing (ParsecT e String m) where
  {-# inline satisfy #-}
  satisfy = Parsec.satisfy
  {-# inline char #-}
  char = Parsec.char
  {-# inline notChar #-}
  notChar = Parsec.notChar
  {-# inline anyChar #-}
  anyChar = Parsec.anyChar
  {-# inline string #-}
  string = Parsec.string
  {-# inline text #-}
  text t = t <$ string (Text.unpack t)

-- | Lazy 'Lazy.Text'
instance Ord e => CharParsing (ParsecT e Lazy.Text m) where
  {-# inline satisfy #-}
  satisfy = Parsec.satisfy
  {-# inline char #-}
  char = Parsec.char
  {-# inline notChar #-}
  notChar = Parsec.notChar
  {-# inline anyChar #-}
  anyChar = Parsec.anyChar
  {-# inline string #-}
  string t = t <$ Parsec.string (Lazy.pack t)
  {-# inline text #-}
  text t = t <$ Parsec.string (Lazy.fromStrict t)

-- | Strict 'Text.Text'
instance Ord e => CharParsing (ParsecT e Text.Text m) where
  {-# inline satisfy #-}
  satisfy = Parsec.satisfy
  {-# inline char #-}
  char = Parsec.char
  {-# inline notChar #-}
  notChar = Parsec.notChar
  {-# inline anyChar #-}
  anyChar = Parsec.anyChar
  {-# inline string #-}
  string t = t <$ Parsec.string (Text.pack t)
  {-# inline text #-}
  text = Parsec.string

instance (Ord e, Stream s) => LookAheadParsing (ParsecT e s m) where
  {-# inline lookAhead #-}
  lookAhead = Parsec.lookAhead

instance Ord e => TokenParsing (ParsecT e String m)
-- | Lazy 'Lazy.Text'
instance Ord e => TokenParsing (ParsecT e Text.Text m)
-- | Strict 'Text.Text'
instance Ord e => TokenParsing (ParsecT e Lazy.Text m)
