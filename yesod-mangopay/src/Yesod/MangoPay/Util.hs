-- | utilities functions that could be useful
module Yesod.MangoPay.Util where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Calendar

-- | day to posix time
day2Posix :: Day -> POSIXTime
day2Posix d=utcTimeToPOSIXSeconds $ UTCTime d 0

-- | posix time to Day
posix2Day :: POSIXTime -> Day
posix2Day =utctDay . posixSecondsToUTCTime