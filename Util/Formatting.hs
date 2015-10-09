module Util.Formatting
(
    formatZonedTime
) where

import qualified Data.Time.Format as DTF
import qualified Data.Time.LocalTime as LT
import Prelude

formatZonedTime :: LT.ZonedTime -> String
formatZonedTime t =
    DTF.formatTime DTF.defaultTimeLocale "%A %-d %B %Y at %H:%M UTC%z" t

