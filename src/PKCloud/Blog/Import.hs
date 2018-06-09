module PKCloud.Blog.Import where

import qualified Data.Text as Text
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

-- Two digit integer.
newtype Int2 = Int2 Int
    deriving (Eq)

instance PathPiece Int2 where
    fromPathPiece t = Int2 <$> fromPathPiece t
    toPathPiece i = Text.pack $ show i

instance Read Int2 where
    readsPrec i a = (\(a, b) -> (Int2 a, b)) <$> readsPrec i a

instance Show Int2 where
    show (Int2 i) | i >= 0 && i < 10 = '0':show i
    show (Int2 i) = show i
