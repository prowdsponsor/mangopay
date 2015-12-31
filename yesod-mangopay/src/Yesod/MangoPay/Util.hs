-- | utilities functions that could be useful
module Yesod.MangoPay.Util where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Calendar
import Web.MangoPay (MpTime(..))

-- | day to posix time
dayToMpTime :: Day -> MpTime
dayToMpTime = MpTime . utcTimeToPOSIXSeconds . flip UTCTime 0

-- | posix time to Day
mpTimeToDay :: MpTime -> Day
mpTimeToDay = utctDay . posixSecondsToUTCTime . unMpTime
