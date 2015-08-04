> -- | Module:    Control.Quiver.CSV
> -- Description: Quiver CSV codec
> -- Copyright:   © 2015 Patryk Zadarnowski <pat@jantar.org>
> -- License:     BSD3
> -- Maintainer:  pat@jantar.org
> -- Stability:   experimental
> -- Portability: portable
> --
> -- Aan efficient Quiver-based implementation
> -- of a cellular CSV encoder and decoder,
> -- designed for fast streaming of data
> -- with guaranteed constant memory usage.

> {-# LANGUAGE PatternSynonyms #-}

> module Control.Quiver.CSV (
>   CSVError (..),
>   decodeCSV, decodeLazyCSV,
>   encodeCSV, encodeLazyCSV,
> ) where

> import Control.Quiver.CSV.Decoder
> import Control.Quiver.CSV.Encoder
> import Control.Quiver.CSV.Extras
