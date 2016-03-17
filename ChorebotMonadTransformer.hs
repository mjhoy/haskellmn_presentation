{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}

module ChorebotMonadTransformer where

import System.Random
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Extra

import Data.Time
import Data.List

import ChorebotTypes

data CState = CState
              { pendingChores :: [Chore]
              , newAssignments :: [Assignment]
              , sanityCheck :: Int
              }

data CConf = CConf { confTime :: UTCTime
                   , confPastAssignments :: [Assignment]
                   , confProfiles :: [Profile]
                   , confSanityCheckLimit :: Int
                   }

type C a = RandT StdGen (ReaderT CConf (State CState)) a

runC :: C a -> CConf -> CState -> StdGen -> ((a, StdGen), CState)
runC k conf st gen = runIdentity (runStateT (runReaderT (runRandT k gen) conf) st)

askTime             :: MonadReader CConf m => m UTCTime
askProfiles         :: C [Profile]
askPastAssignments  :: C [Assignment]
askSanityCheckLimit :: C Int

askTime = liftM confTime ask
askProfiles = liftM confProfiles ask
askPastAssignments = liftM confPastAssignments ask
askSanityCheckLimit = liftM confSanityCheckLimit ask

distribute :: [Profile] -> [Chore] -> [Assignment] -> UTCTime -> StdGen -> ([Assignment], Bool, StdGen)
distribute profiles chores assignments now gen =
  let (((as, hitSc), gen'), _) = runC distribute' conf st gen
  in (as, hitSc, gen')
  where
    st = CState { pendingChores = chores
                , newAssignments = []
                , sanityCheck = 0
                }
    conf = CConf now (reverse $ sort assignments) profiles sclimit
    sclimit = (length chores) * (length profiles) + 50

-- The distribution algorithm.
distribute' :: C ([Assignment], Bool)
distribute' = do
  removeUneccessaryChores -- step 1
  distributePermanent     -- step 2
  sortChores              -- step 3
  hitLim <- distributeAll -- step 4
  st <- get
  return (newAssignments st, hitLim)

removeUneccessaryChores :: C ()
removeUneccessaryChores = do
  st <- get
  c' <- filterM choreNeedsAssignment (pendingChores st)
  put $ st { pendingChores = c' }
  return ()

-- does a chore need doing?
choreNeedsAssignment :: Chore -> C Bool
choreNeedsAssignment c = do
  now <- askTime
  past <- askPastAssignments
  let prevAssignment = find (\a' -> c == (assignmentChore a')) past
  case prevAssignment of
    -- a' is the previous assignment of chore c.
    --
    -- calculate whether the time since last defined is greater
    -- than the interval.
    Just a' -> let diff = diffUTCTime now (assignmentDate a')
                   secInDay = 24 * 60 * 60
                   intervalSeconds = fromIntegral $ (choreInterval c) * secInDay
               in return $ diff >= intervalSeconds
    Nothing -> return True

distributePermanent :: C ()
distributePermanent = do
  profiles <- askProfiles

  forM_ profiles $ \p -> do
    -- check the current pending chores that are permanently assigned
    -- to `p`.
    let doer = profileDoer p
    cs <- liftM pendingChores get
    let toAssign = filter (isPermanentlyAssigned doer) cs
    mapM_ (assignChore p) toAssign

  return ()

assignChore :: Profile -> Chore -> C Assignment
assignChore prof chore = do
  st <- get
  now <- askTime
  let doer = profileDoer prof
      assignment = Assignment doer now chore
      assignments' = assignment : (newAssignments st)
      chores' = filter (/= chore) (pendingChores st)

  put $ st { pendingChores = chores'
           , newAssignments = assignments' }
  return assignment

-- Sort the pending chores by difficulty, hardest are first. Chores of
-- equal difficulty are randomly sorted.
sortChores :: C ()
sortChores = do
    st <- get
    let chores = pendingChores st
    chores' <- randomishSort chores choreDifficulty
    put $ st { pendingChores = chores' }

randomSequence :: MonadRandom m => Int -> m [Int]
randomSequence n = sequence $ replicate n $ getRandomR (1,10000)

-- Given a list of a's, and a function Ord b => a -> b that can order
-- those a's, return an ordered list of as that is randomly ordered
-- where two a's would be compared equally. We use this to both sort
-- and "mix up" profiles and chores.
randomishSort :: Ord b => [a] -> (a -> b) -> C [a]
randomishSort as fn = do
    rs <- randomSequence (length as)
    let asRweighted = zip rs as
    return $ map snd $ sortBy rsortfn asRweighted
  where
    rsortfn (r1, a1) (r2, a2) = case (fn a1) `compare` (fn a2) of
      EQ -> r1 `compare` r2
      a -> a

distributeAll :: C Bool
distributeAll = do
    now <- askTime
    profiles <- askProfiles

    -- Randomize profiles.
    sortedProfiles <- randomishSort profiles (difficultyPerDay now)

    lim <- askSanityCheckLimit

    let checkIter :: C Bool
        checkIter = do
          st <- get
          let chores = pendingChores st
              sc = sanityCheck st
          if sc > lim || (length chores == 0)
            then return False
            else return True

    whileM $ do
      mapM_ distributeOne sortedProfiles
      checkIter

    st <- get
    let hitSanityCheck = if (sanityCheck st) > lim
                         then True
                         else False

    return hitSanityCheck

distributeOne :: Profile -> C ()
distributeOne profile = do
  lim <- askSanityCheckLimit
  st <- get
  let chores = pendingChores st
      assignments = newAssignments st
      doer = profileDoer profile
      sc = sanityCheck st
      shouldAssign c =
        or [ sc >= lim, -- force assignment if sanity check is above limit.
             not $ or [ (hasVetoed doer c),
                        (elem c $ map assignmentChore (filter ((== doer) . assignmentDoer) assignments)),
                        (elem c $ latestChores profile)
                      ]
           ]
      -- assign the first chore of `pendingChores' that makes sense to the
      -- doer.
      newChoreToAssign = find shouldAssign chores

  case newChoreToAssign of
    -- we should assign `c` to `profile`
    Just pending -> assignChore profile pending >> return ()

    -- chore could not be assigned, noop
    Nothing -> return ()

  incSc -- ensure sanity check counter is increased
  return ()

incSc :: C ()
incSc = do
  st <- get
  let sc = sanityCheck st
  put $ st { sanityCheck = sc + 1 }

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
