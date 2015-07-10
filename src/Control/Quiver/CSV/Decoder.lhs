> -- | Module:    Control.Quiver.CSV.Decoder
> -- Description: Streaming CSV decoder
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- Streaming CSV file decoder compliant with RFC 4180.
> -- It follows the RFC 4180 quite strictly, with the following
> -- minor extensions:
> --
> -- * arbitrary characters, including ASCII control codes and non-ASCII code points
> --   are accepted anywhere in the input,
> -- * CR and LF are accepted as row separators in addition to the standard CR+LF,
> -- * rows can have varying number of fields,
> -- * final row is not required to end with CR+LF, and
> -- * within quoted field, a quote character that is not followed by another quote,
> --   comma or line break is accepted literally.

> {-# LANGUAGE PatternSynonyms, RankNTypes #-}

> module Control.Quiver.CSV.Decoder (
>   decodeCSV, decodeLazyCSV
> ) where

> import Data.ByteString (ByteString)
> import Data.Cell
> import Control.Quiver
> import Control.Quiver.ByteString
> import Control.Quiver.CSV.Extras

> import qualified Data.ByteString as ByteString
> import qualified Data.ByteString.Lazy as Lazy

> -- | A Quiver processor that parses a fragmented lazy representation
> --   of a CSV file into a stream of cells.

> decodeLazyCSV :: Functor f => SP Lazy.ByteString (Cell Lazy.ByteString) f (ParseResult ParseError)
> decodeLazyCSV = fmap (snd.fst) (toChunks >->> decodeCSV >->> qpure_ (fmap Lazy.fromStrict))

> -- | A Quiver processor that parses a fragmented strict representation
> --   of a CSV file into a stream of cells.

> decodeCSV :: Functor f => SP ByteString (Cell ByteString) f (ParseResult ParseError)
> decodeCSV = decodeC
>  where

    At the beginning of a new cell:

>   decodeC = consume () decodeC' (deliver ParseComplete)
>   decodeC' s =
>     case ByteString.uncons s of
>       Just (c, xs) ->
>         case c of
>           QC -> decodeQ' xs
>           CC -> Cell ByteString.empty EOC >:> decodeC'  xs
>           CR -> Cell ByteString.empty EOR >:> decodeCr' xs
>           LF -> Cell ByteString.empty EOR >:> decodeC'  xs
>           _  -> decodeU' s 1 xs
>       Nothing -> decodeC

    After decodeC detects a CR:

>   decodeCr = consume () decodeCr' (deliver ParseComplete)
>   decodeCr' s =
>     case ByteString.uncons s of
>       Just (c, xs) ->
>         case c of
>           QC -> decodeQ' xs
>           CC -> Cell ByteString.empty EOC >:> decodeC'  xs
>           CR -> Cell ByteString.empty EOR >:> decodeCr' xs
>           LF -> decodeC' xs
>           _  -> decodeU' s 1 xs
>       Nothing -> decodeCr

    Decode a quoted cell:

>   decodeQ = consume () decodeQ' (deliverError IncompleteCell)
>   decodeQ' s = decodeQ1' s 0 s

    Decode a quoted cell, beginning the search for the next
    quote at a given string position:

>   decodeQ1' s i s' =
>     case ByteString.elemIndex QC s' of
>       Just i' -> let i'' = i + i' in i'' `seq` decodeQ2' s i'' (ByteString.drop (i' + 1) s')
>       Nothing
>         | ByteString.null s -> decodeQ
>         | otherwise -> decodeR s

    Decode a part of a quoted cell after a quote character has been located at @s!i@:

>   decodeQ2' s i s' =
>     case ByteString.uncons s' of
>       Just (c, xs) ->
>         case c of
>           QC -> decodeR' (ByteString.take (i + 1) s) xs
>           CC -> Cell (ByteString.take i s) EOC >:> decodeC'  xs
>           CR -> Cell (ByteString.take i s) EOR >:> decodeCr' xs
>           LF -> Cell (ByteString.take i s) EOR >:> decodeC'  xs
>           _  -> let i' = i + 2 in i' `seq` decodeQ2' s i' xs -- accept the quote character literally, as an extension to RFC 4180
>       Nothing -> decodeQ3 s

    Same as decodeQ2, when the quote ends up at the end of the chunk @qs@:

>   decodeQ3 qs = consume () (decodeQ3' qs) (Cell (ByteString.init qs) EOR >:> deliverError IncompleteRow)
>   decodeQ3' qs s =
>     case ByteString.uncons s of
>       Just (c, xs) ->
>         case c of
>           QC -> decodeR' qs xs
>           CC -> Cell (ByteString.init qs) EOC >:> decodeC'  xs
>           CR -> Cell (ByteString.init qs) EOR >:> decodeCr' xs
>           LF -> Cell (ByteString.init qs) EOR >:> decodeC'  xs
>           _  -> decodeR1' qs s 1 xs -- accept the quote character literally, as an extension to RFC 4180
>       Nothing -> decodeQ3 qs

    Decode more quoted cell parts after a non-empty part @ps@ has been identified:

>   decodeR ps = consume () (decodeR' ps) (Cell ps EOP >:> deliverError IncompleteCell)
>   decodeR' ps s = decodeR1' ps s 0 s

    Decode more quoted cell parts after a non-empty part @ps@ has been identified,
    beginning the search for the next quote at a given string position:

>   decodeR1' ps s i s' =
>     case ByteString.elemIndex QC s' of
>       Just i' -> let i'' = i + i' in i'' `seq` decodeR2' ps s i'' (ByteString.drop (i' + 1) s')
>       Nothing
>         | ByteString.null s -> decodeR ps
>         | otherwise -> Cell ps EOP >:> decodeR s

    Decode a part of a quoted cell after a non-empty part @ps@ has been identified
    and after a quote character has been located at @s!i@:

>   decodeR2' ps s i s' =
>     case ByteString.uncons s' of
>       Just (c, xs) ->
>         case c of
>           QC -> Cell ps EOP >:> decodeR' (ByteString.take (i + 1) s) xs
>           CC -> produce' EOC (decodeC'  xs)
>           CR -> produce' EOR (decodeCr' xs)
>           LF -> produce' EOR (decodeC'  xs)
>           _  -> decodeR2' ps s (i + 2) xs -- accept the quote character literally, as an extension to RFC 4180
>       Nothing -> decodeR3 ps s i
>    where
>     produce' d pk
>       | (i > 0) = Cell ps EOP >:> Cell (ByteString.take i s) d >:> pk
>       | otherwise = Cell ps d >:> pk

    Same as @decodeR2@, when the quote ends up at the end of the chunk @qs@:

>   decodeR3 ps qs qi = consume () (decodeR3' ps qs qi) (Cell ps EOP >:> Cell (ByteString.init qs) EOR >:> deliverError IncompleteRow)
>   decodeR3' ps qs qi s =
>     case ByteString.uncons s of
>       Just (c, xs) ->
>         case c of
>           QC -> Cell ps EOP >:> decodeR' qs xs
>           CC -> produce' EOC (decodeC'  xs)
>           CR -> produce' EOR (decodeCr' xs)
>           LF -> produce' EOR (decodeC'  xs)
>           _  -> Cell ps EOP >:> decodeR1' qs s 1 xs -- accept the quote character literally, as an extension to RFC 4180
>       Nothing -> decodeR3 ps qs qi
>    where
>     produce' d pk
>       | (qi > 0) = Cell ps EOP >:> Cell (ByteString.init qs) d >:> pk
>       | otherwise = Cell ps d >:> pk

    Decode an unquoted field:

>   decodeU' s i s' =
>     case ByteString.uncons s' of
>       Just (c, xs) ->
>         case c of
>           CC -> Cell (ByteString.take i s) EOC >:> decodeC'  xs
>           CR -> Cell (ByteString.take i s) EOR >:> decodeCr' xs
>           LF -> Cell (ByteString.take i s) EOR >:> decodeC'  xs
>           _  -> let i' = i + 1 in i' `seq` decodeU' s i' xs
>       Nothing -> decodeV s

    Decode an unquoted field after a non-empty part @ps@ has been identified:

>   decodeV ps = consume () (decodeV' ps) (Cell ps EOR >:> deliverError IncompleteRow)
>   decodeV' ps s =
>     case ByteString.uncons s of
>       Just (c, xs) ->
>         case c of
>           CC -> Cell ps EOC >:> decodeC'  xs
>           CR -> Cell ps EOR >:> decodeCr' xs
>           LF -> Cell ps EOR >:> decodeC'  xs
>           _  -> Cell ps EOP >:> decodeU' s 1 xs
>       Nothing -> decodeV ps
