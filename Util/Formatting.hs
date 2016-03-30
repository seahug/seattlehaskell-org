module Util.Formatting
    ( formatZonedTimeVerbose
    ) where

import qualified Data.Time.Format as DTF
import qualified Data.Time.LocalTime as LT
import           Prelude

formatZonedTimeVerbose :: LT.ZonedTime -> String
formatZonedTimeVerbose =
    DTF.formatTime DTF.defaultTimeLocale "at %H:%M UTC%z on %A %-d %B %Y"
