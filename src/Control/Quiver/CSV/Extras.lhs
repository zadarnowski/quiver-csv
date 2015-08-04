> -- | Module:    Control.Quiver.CSV.Extras
> -- Description: Miscellaneous definitions
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- CSV syntax elements as defined in RFC 4180
> -- and a few miscellaneous Quiver combinators.

> {-# LANGUAGE PatternSynonyms, ScopedTypeVariables #-}

> module Control.Quiver.CSV.Extras (
>   CSVError (..),
>   (*>:>),
>   deliverError,
>   pattern LF,
>   pattern CR,
>   pattern QC,
>   pattern CC,
>   isSpecial,
>   fieldDelimiter,
>   fieldSeparator,
>   recordSeparator,
>   quoteSequence
> ) where

> import Control.Quiver.SP
> import Data.ByteString (ByteString)
> import Data.Word

> import qualified Data.ByteString as ByteString

> -- | Codec error type.

> data CSVError =

>   -- | Input ends in a partial cell value.
>   --   For encoder, this means that the last 'Cell' of the input stream specifies the 'EOP' delimiter instead of 'EOR' or 'EOT'.
>   --   For decoder, this means that the last cell of the input stream is missing the closing quote character.

>   IncompleteCell |

>   -- | Input ends in a partial row value.
>   --   For encoder, this means that the last 'Cell' of the input stream specifies the 'EOC' delimiter instead of 'EOR' or 'EOT'.
>   --   For decoder, this means that the last row of the input stream is missing the line break sequence.

>   IncompleteRow
>   deriving (Eq, Ord, Show, Enum)

> infixr 5 *>:>

> -- | Emits a bytestring, filtering out the empty ones.

> (*>:>) :: ByteString -> P a' a ByteString b' f (SPResult e) -> P a' a ByteString b' f (SPResult e)
> y *>:> sp
>   | ByteString.null y = sp
>   | otherwise = produce y (const sp) (deliver SPIncomplete)

> -- | Delivers a simple stream processor failure.

> deliverError :: e -> P a' a b b' f (SPResult e)
> deliverError = deliver . SPFailed

> -- | line byte value
> pattern LF = 0x0A :: Word8

> -- | carriage return byte value
> pattern CR = 0x0D :: Word8

> -- | quote character byte value
> pattern QC = 0x22 :: Word8

> -- | comma character byte value
> pattern CC = 0x2C :: Word8

> -- | Identifies special byte values, which should never appear within unquoted CSV fields.
> isSpecial :: Word8 -> Bool
> isSpecial x = (x == QC || x == CC || x < 0x20 || x > 0x7E)

> -- | The field delimiter string (a quote character.)
> fieldDelimiter :: ByteString
> fieldDelimiter = ByteString.singleton QC

> -- | The field separator string (a comma character.)
> fieldSeparator :: ByteString
> fieldSeparator = ByteString.singleton CC

> -- | The field separator (CR+LF byte sequence.)
> recordSeparator :: ByteString
> recordSeparator = ByteString.pack [ CR, LF ]

> -- | An escaped quote string (double quote character.)
> quoteSequence :: ByteString
> quoteSequence = ByteString.pack [ QC, QC ]

