module Data.Tag (module X) where

import Data.Tag.Internal as X
  ( Has (..),
    STag (..),
    Tag (tag),
    absurd,
    inject,
    project,
    tagSing,
    pattern That,
    pattern This,
  )
import Data.Tag.Internal.Utils as X (Length (..), (:>) (..))
