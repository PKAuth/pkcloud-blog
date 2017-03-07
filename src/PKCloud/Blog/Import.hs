module PKCloud.Blog.Import where

import Data.Time.Clock (utctDay)
import Data.Time.Calendar (toGregorian)

import PKCloud.Import

type PostYear = Int
type PostMonth = Int
type PostDay = Int

-- JP: Take into account time zones?
splitDate :: UTCTime -> (PostYear, PostMonth, PostDay)
splitDate date = 
    let (year, month, day) = toGregorian $ utctDay date in
    (fromInteger year, month, day)
