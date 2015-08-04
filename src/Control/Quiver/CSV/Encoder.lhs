> -- | Module:    Control.Quiver.CSV.Encoder
> -- Description: Streaming CSV encoder
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- Streaming CSV file decoder compliant with RFC 4180.
> -- All fields are quoted except for short fields consisting
> -- entirely of “safe” ASCII characters, i.e., printable 7-bit
> -- characters other than quote and comma. The maximum length
> -- of an unquoted field is supplied explicitly to the encoder,
> -- which allows us to decide whether a given field requires
> -- quoting without unbounded lookahead.

> {-# LANGUAGE RankNTypes #-}

> module Control.Quiver.CSV.Encoder (
>   encodeCSV, encodeLazyCSV
> ) where

> import Data.ByteString (ByteString)
> import Data.Cell
> import Control.Quiver.SP
> import Control.Quiver.CSV.Extras

> import qualified Data.ByteString as ByteString
> import qualified Data.ByteString.Lazy as Lazy

> -- | @encodeCSV n@ is an infinite pipe that converts a stream
> --   of cells into a fragmented strict representation of a CSV file,
> --   unconditionally quoting any field values with length greater than @n@.

> encodeCSV :: Int -> SP (Cell ByteString) ByteString f CSVError
> encodeCSV n = loop
>  where

>   loop = consume () loop' (deliver SPComplete)
>   loop' (Cell x EOP) = fieldDelimiter >:> quotePart x loop1
>   loop' (Cell x EOC) = quoteField x (fieldSeparator  >:> loop2)
>   loop' (Cell x EOR) = quoteField x (recordSeparator >:> loop)
>   loop' (Cell x EOT) = quoteField x (recordSeparator >:> loop)

>   loop1 = consume () loop1' (deliverError IncompleteCell)
>   loop1' (Cell x EOP) = quotePart x loop1
>   loop1' (Cell x EOC) = quotePart x (fieldDelimiter >:> fieldSeparator  >:> loop2)
>   loop1' (Cell x EOR) = quotePart x (fieldDelimiter >:> recordSeparator >:> loop)
>   loop1' (Cell x EOT) = quotePart x (fieldDelimiter >:> recordSeparator >:> loop)

>   loop2 = consume () loop' (deliverError IncompleteRow)

>   quoteField x pk
>     | requiresQuoting x = fieldDelimiter >:> quotePart x (fieldDelimiter >:> pk)
>     | otherwise = x *>:> pk

>   requiresQuoting x = (ByteString.length x > n || ByteString.any isSpecial x)

> -- | @encodeLazyCSV n@ is an infinite pipe that converts a stream
> --   of cells into a fragmented lazy representation of a CSV file,
> --   unconditionally quoting any field values with length greater than @n@.

> encodeLazyCSV :: Int -> SP (Cell Lazy.ByteString) ByteString f CSVError
> encodeLazyCSV n = loop
>  where

>   loop = consume () loop' (deliver SPComplete)
>   loop' (Cell x EOP) = fieldDelimiter >:> quoteParts x loop1
>   loop' (Cell x EOC) = quoteField x (fieldSeparator  >:> loop2)
>   loop' (Cell x EOR) = quoteField x (recordSeparator >:> loop)
>   loop' (Cell x EOT) = quoteField x (recordSeparator >:> loop)

>   loop1 = consume () loop1' (deliverError IncompleteCell)
>   loop1' (Cell x EOP) = quoteParts x loop1
>   loop1' (Cell x EOC) = quoteParts x (fieldDelimiter >:> fieldSeparator  >:> loop2)
>   loop1' (Cell x EOR) = quoteParts x (fieldDelimiter >:> recordSeparator >:> loop)
>   loop1' (Cell x EOT) = quoteParts x (fieldDelimiter >:> recordSeparator >:> loop)

>   loop2 = consume () loop' (deliverError IncompleteRow)

>   quoteParts x pk = foldr quotePart pk (Lazy.toChunks x)

>   quoteField x pk
>     | requiresQuoting n cs = fieldDelimiter >:> foldr quotePart (fieldDelimiter >:> pk) cs
>     | otherwise = foldr (*>:>) pk cs
>    where cs = Lazy.toChunks x

>   requiresQuoting n' (c:cs) = (m > n' || ByteString.any isSpecial c || requiresQuoting (n - m) cs)
>    where m = fromIntegral (ByteString.length c)
>   requiresQuoting _ [] = False

> quotePart x pk =
>   case ByteString.elemIndex QC x of
>     Just i -> ByteString.take i x *>:> quoteSequence >:> quotePart (ByteString.drop (i + 1) x) pk
>     Nothing -> x *>:> pk
