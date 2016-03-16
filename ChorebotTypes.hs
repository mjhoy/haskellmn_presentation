module ChorebotTypes where

import Text.Regex (matchRegex, mkRegexWithOpts)
import Data.Maybe (isJust, fromJust)
import Data.List
import Data.Time

data Chore = Chore { choreTitle      :: String
                   , choreInterval   :: Int
                   , choreDifficulty :: Int
                   , choreCount      :: Int
                   } deriving (Show, Eq)

dishFairy, emptyCompost, cleanRaptor, watchDradis, checkFTL :: Chore
dishFairy    = Chore "Dish Fairy"    7 2 1
emptyCompost = Chore "Empty compost" 7 2 1
cleanRaptor  = Chore "Clean raptor"  7 7 1
watchDradis  = Chore "Watch DRADIS"  1 4 1
checkFTL     = Chore "Check FTL"    14 4 1

galacticaChores :: [Chore]
galacticaChores = [dishFairy, emptyCompost, cleanRaptor, watchDradis, checkFTL]

type Pattern = String

data Doer = Doer { doerName       :: String
                 , doerVetoes     :: [Pattern]
                 , doerPermanents :: [Pattern]
                 } deriving (Eq, Show)

lee, baltar :: Doer
baltar = Doer "Gaius Baltar" ["compost"] []
lee    = Doer "Lee Adama"    []          ["dish"]
bsgDoers :: [Doer]
bsgDoers = [baltar, lee]

isPermanentlyAssigned :: Doer -> Chore -> Bool
isPermanentlyAssigned doer chore =
  any (matchChore chore) (doerPermanents doer)

hasVetoed :: Doer -> Chore -> Bool
hasVetoed doer chore =
  any (matchChore chore) (doerVetoes doer)

matchChore :: Chore -> Pattern -> Bool
matchChore chore pat =
  let pat' = mkRegexWithOpts pat True False -- case insensitive
  in isJust $ matchRegex pat' (choreTitle chore)

data Assignment = Assignment { assignmentDoer       :: Doer,
                               assignmentDate       :: UTCTime,
                               assignmentChore      :: Chore
                             } deriving (Show, Eq)

instance Ord Assignment where
  a1 `compare` a2 =
    (assignmentDate a1) `compare` (assignmentDate a2)

galacticaAssignments :: [Assignment]
galacticaAssignments =
  [ Assignment lee    (parseDate "2016/03/09") dishFairy
  , Assignment baltar (parseDate "2016/03/09") cleanRaptor
  ]

data Profile = Profile { profileDoer        :: Doer
                       , profileAssignments :: [Assignment]
                       } deriving (Eq, Show)

buildProfile :: [Assignment] -> -- List of all/any chore assignments
                Doer ->
                Profile
buildProfile assignments doer = Profile doer assignments''
  where
    assignments'' = sort assignments'
    assignments' = filter byDoer assignments
    byDoer a = (assignmentDoer a) == doer

parseDate :: String -> UTCTime
parseDate t = fromJust $ parseTimeM True defaultTimeLocale "%Y/%m/%d" t

leeProf, baltarProf :: Profile
leeProf    = buildProfile galacticaAssignments lee
baltarProf = buildProfile galacticaAssignments baltar
galacticaProfiles :: [Profile]
galacticaProfiles = [leeProf, baltarProf]

-- Find the chores with the most recent date
latestChores :: Profile -> [Chore]
latestChores (Profile _d []) = []
latestChores (Profile _d as@(a:_)) =
    let latest = foldl' lateDate (assignmentDate a) as
    in map assignmentChore $ filter (\a' -> (assignmentDate a') == latest) as
  where
    lateDate :: UTCTime -> Assignment -> UTCTime
    lateDate t a' = let t' = assignmentDate a'
                    in if t' > t then t' else t

difficultyPerDay :: UTCTime ->  -- the current time
                    Profile ->
                    Double
difficultyPerDay now (Profile _ assignments) =
  -- get the earliest date in `as'
    let earliest  = foldl' earlyDate now assignments
        diffTime  = max secInDay $ round $ diffUTCTime now earliest
        secInDay = 24 * 60 * 60
        daysSince :: Double
        daysSince = fromIntegral diffTime / fromIntegral secInDay
        totalDifficulty = foldl' (\d a -> d + (choreDifficulty $ assignmentChore a)) 0 assignments
    in fromIntegral totalDifficulty / daysSince
  where
    earlyDate :: UTCTime -> Assignment -> UTCTime
    earlyDate t a = let t' = assignmentDate a
                    in if t' < t then t' else t
