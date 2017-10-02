module Task.DayOfWeek where

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

daysInWeek :: Int
daysInWeek = 7

dayNumber :: DayOfWeek -> Int
dayNumber Mon = 0
dayNumber Tue = 1
dayNumber Wed = 2
dayNumber Thu = 3
dayNumber Fri = 4
dayNumber Sat = 5
dayNumber Sun = 6

toDay :: Int -> DayOfWeek
toDay 0 = Mon
toDay 1 = Tue
toDay 2 = Wed
toDay 3 = Thu
toDay 4 = Fri
toDay 5 = Sat
toDay 6 = Sun
toDay n = toDay $ n `mod` daysInWeek

isWeekend :: DayOfWeek -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

afterDays :: Int -> DayOfWeek -> DayOfWeek
afterDays n d = toDay $ dayNumber d + n

nextDay :: DayOfWeek -> DayOfWeek
nextDay = afterDays 1

daysToParty :: DayOfWeek -> Int
daysToParty d = (dayNumber Fri - dayNumber d) `mod` daysInWeek