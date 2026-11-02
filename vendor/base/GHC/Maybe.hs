-- | The Maybe type.
module GHC.Maybe
  ( Maybe (..),
  )
where

data Maybe a = Nothing | Just a
