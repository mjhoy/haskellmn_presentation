module ChorebotNaive where

import Data.List
import Control.Monad
import Data.Time
import System.Random
import ChorebotTypes

distribute :: RandomGen g =>

              -- list of profiles to assign chores to
              [Profile] ->

              -- list of possible chores to assign
              [Chore] ->

              -- list of past chore assignments
              [Assignment] ->

              -- current time
              UTCTime ->

              -- random number generator
              g ->

              -- a list of new assignments plus whether any chores
              -- were force assigned, plus a new random gen
              ([Assignment], Bool, g)

distribute profiles chores pastAssignments now gen = (finalAssignments, didForceAssign, finalGen)
  where
    -- past assignments sorted most recent first
    pastAssignments' = reverse $ sort pastAssignments

    -- step 1: remove chores assigned previously within their interval
    chores' = filter (choreNeedsAssignment pastAssignments' now) chores

    -- step 2: distribute permanent chores
    (chores'', newAssignments) = distributePermanent (chores',[]) profiles now

    -- step 3: sort chores by difficulty, hardest first
    (chores''', gen') = sortChoresByDifficulty chores'' gen

    -- step 4: sort profiles in order of least "difficultyPerDay" first
    (profiles', gen'') = sortProfilesByDifficulty profiles gen' now

    -- step 5: distribute the rest of the chores
    (newAssignments', didForceAssign) = distributeAll (chores''', newAssignments, 0) profiles' now

    finalAssignments = newAssignments'
    finalGen = gen''

-- does a chore need doing?
choreNeedsAssignment :: [Assignment] -> UTCTime -> Chore -> Bool
choreNeedsAssignment pastAssignments now c =
  let prevAssignment = find (\a' -> c == (assignmentChore a')) pastAssignments
  in case prevAssignment of

    -- a' is the previous assignment of chore c.
    --
    -- calculate whether the time since last defined is greater
    -- than the interval.
    Just a' -> let diff = diffUTCTime now (assignmentDate a')
                   secInDay = 24 * 60 * 60
                   intervalSeconds = fromIntegral $ (choreInterval c) * secInDay
               in diff >= intervalSeconds

    -- chore c has never been assigned before, so we should
    -- definitely assign it.
    Nothing -> True

-- assign all permanent chores
distributePermanent :: ([Chore], [Assignment]) -> [Profile] -> UTCTime -> ([Chore], [Assignment])
distributePermanent (chores, assignments) profiles now = (chores', assignments')
  where
    (chores', assignments') =
      foldl' assignPermForDoer (chores, assignments) (map profileDoer profiles)

    assignPermForDoer ([], as) _doer = ([], as)
    assignPermForDoer (cs, as) doer  =  (cs \\ assignedChores,
                                         as ++ (map (Assignment doer now) assignedChores))
      where
        assignedChores = filter (isPermanentlyAssigned doer) cs

randomNRs :: RandomGen g => Int -> g -> ([Int], g)
randomNRs n g = foldl' fn ([], g) (take n $ repeat ())
  where fn (acc,g') _ = let (a, g'') = randomR (1,10000) g'
                        in (a:acc, g'')

-- most difficult chores first; randomize chores with equal
-- difficulties
sortChoresByDifficulty :: RandomGen g => [Chore] -> g -> ([Chore], g)
sortChoresByDifficulty chores gen =
  let (rs, gen') = randomNRs (length chores) gen
      cRandomWeight = zip rs chores
      sortFn :: (Int, Chore) -> (Int, Chore) -> Ordering
      sortFn (r1, c1) (r2, c2) = case choreDifficulty c1 `compare` choreDifficulty c2 of
        EQ -> r1 `compare` r2
        a  -> a
  in ((map snd $ reverse $ sortBy sortFn cRandomWeight), gen')

-- profiles with the least "difficultyPerDay" first; randomize when
-- equal.
sortProfilesByDifficulty :: RandomGen g => [Profile] -> g -> UTCTime -> ([Profile], g)
sortProfilesByDifficulty profiles gen now =
  let (rs, gen') = randomNRs (length profiles) gen
      pRandomWeight = zip rs profiles
      sortFn :: (Int, Profile) -> (Int, Profile) -> Ordering
      sortFn (r1, p1) (r2, p2) = case difficultyPerDay now p1 `compare` difficultyPerDay now p2 of
        EQ -> r1 `compare` r2
        a -> a
  in ((map snd $ sortBy sortFn pRandomWeight), gen')

hitSCLim :: [Profile] -> Int -> Bool
hitSCLim profiles sc = sc >= (length profiles * 50)


-- distribute all remaining chores
distributeAll :: ([Chore], [Assignment], Int) -> [Profile] -> UTCTime -> ([Assignment], Bool)

-- no more pending chores: return assignments.
distributeAll ([], acc, sc) profiles _now = (acc, hitSCLim profiles sc)

-- otherwise
distributeAll (chores, acc, sc) profiles now = distributeAll (chores', acc', sc') profiles now
  where
    overLimit = hitSCLim profiles sc
    (chores', acc', sc') = foldl' (mkAssignment overLimit now) (chores, acc, sc) profiles


mkAssignment ::
  -- went over sc limit; if so, "force assign"
  Bool ->

  -- current time
  UTCTime ->

  -- state
  ([Chore], [Assignment], Int) ->

  -- profile to assign to
  Profile ->

  -- new state
  ([Chore], [Assignment], Int)

mkAssignment overLimit now (c, a, s) profile =
    mkAssignment' (c, a, s) []
  where
    mkAssignment' :: ([Chore], [Assignment], Int) -> [Chore] -> ([Chore], [Assignment], Int)
    mkAssignment' ([], assignments, sc) acc = (acc, assignments, sc + 1)
    mkAssignment' (chore:cs, assignments, sc) acc =
      let doer = profileDoer profile
          newAssignment = Assignment doer now chore

          -- should we make this assignment?
          shouldAssign = or [
            overLimit,
            (and [(not $ hasVetoed doer chore),
                  (not $ elem chore $ map assignmentChore $ filter ((== doer) . assignmentDoer) assignments),
                  (not $ elem chore $ latestChores profile)])
            ]
      in if shouldAssign
         then (acc ++ cs, newAssignment:assignments, sc + 1)
         else mkAssignment' (cs, assignments, sc) (chore:acc)

testDistribute :: IO ()
testDistribute = do
  g <- newStdGen
  t <- getCurrentTime
  let (as, didHitLimit, _) = distribute galacticaProfiles galacticaChores galacticaAssignments t g
  if didHitLimit
    then putStrLn "Hit limit!"
    else putStrLn "Didn't hit limit"
  forM_ as $ \a -> do
    putStrLn $ (choreTitle . assignmentChore $ a) ++ " -> " ++ (doerName . assignmentDoer $ a)
