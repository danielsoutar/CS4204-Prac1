module Strategies (
       parList,
       parThreshold,
       parzipwith, parZipWith,
       parmap,
       parfold, parFold,
       parscan, parScan,
       dc, dC,
       bsp, bSP,
       branchandbound, branchAndBound,
       taskfarm, taskFarm,
       parmapreduce, parMapReduce,
       parMapReduceSimple, parMapReduceKey,
       parpipeline, parpipeline2, parpipeline3, 
       parpipeline4, parpipeline5, parpipeline6, 
       parstream, parstream2, parstream3, parstream4, parstream5, parstream6,
       parSearch
    ) where

import Control.Parallel.Strategies hiding (parList)
import Control.Parallel
import Control.DeepSeq

import Control.Concurrent.MVar
import System.IO.Unsafe

import Data.List
import Data.Maybe
import Data.Tree

import Debug.Trace

-- strategic parallel zipWith
parZipWith strat (<>) as bs = zipWith (<>) as bs `using` parList strat

-- strategic parallel zip
parZip :: Strategy (a, b) -> [a] -> [b] -> [(a,b)]
parZip strategy as bs = zip as bs `using` parList strategy

-- non-strategic versions of standard patterns
parzipwith :: NFData c => (a->b->c) -> [a] -> [b] -> [c]
parzipwith = parZipWith rdeepseq

parmap :: NFData b => (a->b) -> [a] -> [b]
parmap = parMap rdeepseq

parfold :: NFData a => (a->a->a) -> a -> [a] -> a
parfold = parFold rdeepseq

-- Left and right folds make less sense as parallel operators...
parfoldl, parfoldr  :: NFData a => (a->a->a) -> a-> [a] -> a
parfoldl = parfold
parfoldr = parfold

-- Improved parList that doesn't return
-- until it's evaluated the last element...
parListx :: Strategy a -> Strategy [a]
parListx strat [] =     return []
parListx strat [x] =    strat x `pseq` return [x]
parListx strat (x:xs) = strat x `par` 
                            (parListx strat xs `pseq` return (x:xs))

-- override the standard parList
parList = parListx

taskFarm :: Strategy [b] -> (a -> [b]) -> Int -> [a] -> [[b]]
taskFarm strat f nWorkers tasks = concat results `using` parList strat
    where results = unshuffle nWorkers (map f tasks)

taskfarm :: NFData b => (a -> [b]) -> Int -> [a] -> [[b]]
taskfarm = taskFarm (evalList rdeepseq)

unshuffle :: Int -> [a] -> [ [a] ]
unshuffle n xs = [ takeEach n (drop i xs) | i <- [0..n-1] ]
  where takeEach :: Int -> [a] -> [a]
        takeEach n [] = []
        takeEach n (x:xs) = x : takeEach n (drop (n-1) xs)

parFold :: Strategy a -> (a -> a -> a) -> a -> [a] -> a
parFold strat f z l = parFold' (length l) f z l
   where 
     parFold' _ f z [] = withStrategy strat z
     parFold' _ f z [x] = withStrategy strat x
     parFold' n f z xs =  
         let n2 = n `div` 2 in
         let (l,r) = splitAt n2 xs in
         let lt = parFold' n2 f z l; 
             rt = parFold' (n2 + n `rem` 2) f z r in
         rt `par` (lt `pseq` f lt rt)


parstream2 f1 f2 = 
   parpipeline3 (map f1) (map f2)

parstream3 f1 f2 f3 = 
   parpipeline3 (map f1) (map f2) (map f3)

parstream4 f1 f2 f3 f4 =
   parpipeline4 (map f1) (map f2) (map f3) (map f4)

parstream5 f1 f2 f3 f4 f5 =
   parpipeline5 (map f1) (map f2) (map f3) (map f4)  (map f5)

parstream6 f1 f2 f3 f4 f5 f6 = 
   parpipeline6 (map f1) (map f2) (map f3) (map f4)  (map f5) (map f6)

parpipeline fs z =
  foldr (\f x -> f $|| rdeepseq $ x) z fs

parstream fs l =
    parpipeline (map map fs) l

parpipeline2 f1 f2 x =
  f2 $|| rdeepseq $
  f1 $|| rdeepseq $
  x

parpipeline3 f1 f2 f3 x =
  f3 $|| rdeepseq $
  f2 $|| rdeepseq $
  f1 $|| rdeepseq $
  x

parpipeline4 f1 f2 f3 f4 x =
  f4 $|| rdeepseq $
  f3 $|| rdeepseq $
  f2 $|| rdeepseq $
  f1 $|| rdeepseq $
  x

parpipeline5 f1 f2 f3 f4 f5 x =
  f5 $|| rdeepseq $
  f4 $|| rdeepseq $
  f3 $|| rdeepseq $
  f2 $|| rdeepseq $
  f1 $|| rdeepseq $
  x

parpipeline6 f1 f2 f3 f4 f5 f6 x =
  f5 $|| rdeepseq $
  f4 $|| rdeepseq $
  f3 $|| rdeepseq $
  f2 $|| rdeepseq $
  f1 $|| rdeepseq $
  x

dc :: NFData b => (a -> [a]) -> (a -> Bool) -> ([b] -> b) ->
                  (a -> [b]) -> a -> b
dc = dC rdeepseq

dC :: Strategy b -> (a -> [a]) -> (a -> Bool) -> ([b] -> b) -> 
                    (a -> [b]) -> a -> b

dC strat split threshold combine worker input = combine results
    where
      results = if threshold input then
                   worker input
                else
                   parMap strat (dC strat split threshold combine worker) 
                                (split input)

bsp :: (NFData l, NFData g) => [ l -> [g] -> (l,g) ] -> [ l ] -> [ g ] -> [ g ]
bsp = bSP rdeepseq

bSP :: Strategy (l,g) -> [ l -> [g] -> (l,g) ] -> [ l ] -> [ g ] -> [ g ]
bSP strat [] ls gs = gs
bSP strat (f:fs) ls gs = 
    let lgs = parMap strat (\l -> f l gs) ls in
    let (ls',gs') = unzip lgs in
    bSP strat fs ls' gs'

parscan :: NFData a => (a -> a -> a) -> a -> [a] -> [a] 
parscan = parScan rdeepseq

parScan :: Strategy a -> (a -> a -> a) -> a -> [a] -> [a] 
parScan = undefined

{-
  This definition of scan is due to Sebastian Fischer, but with parzipwith.
  However, it is essentially sequential: the dependencies need to be broken!
  TODO: get a better definition.
-}

parscanl :: NFData a => (a -> a -> a) -> [a] -> [a]
parscanl (<>) = go 1
    where
      go n l = let (xs,ys) = splitAt n l
                in if null ys then xs
                     else go (2*n) (xs ++ parzipwith (<>) l ys)

parThreshold :: Bool -> Strategy a -> Strategy a
parThreshold threshold strat =
  if threshold then strat `dot` rpar else r0

parMapThreshold :: (a->Bool) -> Strategy b -> (a->b) -> [a]-> [b]
parMapThreshold threshold strat f l =   map strat' l
     where strat' x = let fx = f x in
                      if threshold x then withStrategy (strat `dot` rpar) fx else fx


-- Branch and bound using a semaphore

syncl f sem [] = return ()
syncl f sem (x:xs) = do { sync f sem x `par` syncl f sem xs }

-- synchronise using the semaphore
sync f sem x = x `pseq` unsafePerformIO (f sem x)

decsem (s,m) x = do { modifyMVar_ m (\v -> (if v == 1 then putMVar s () 
                                            else return ()) >> return (v - 1)) }

incsem (s,m) =   do { modifyMVar_ m (\v -> return (v + 1)) }

branchandbound :: NFData a => [a] -> a 
branchandbound = branchAndBound rdeepseq

branchAndBound :: Strategy a -> [a] -> a
branchAndBound strat ts = 
    unsafePerformIO (do { res <- newEmptyMVar; 
                          syncl putMVar res (map (withStrategy strat) ts); 
                          takeMVar res})

--- implementations of mapReduce

parMapReduceSimple
    :: Strategy b    -- evaluation strategy for mapping
    -> (a -> b)      -- map function
    -> Strategy c    -- evaluation strategy for reduction
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c

parMapReduceSimple mapStrat mapFunc reduceStrat reduceFunc input =
    mapResult `pseq` reduceResult
  where mapResult    = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat

parMapReduceKey
    :: Eq k
    => Strategy (k,b)    -- evaluation strategy for mapping
    -> ((k,a) -> (k,b))  -- function to map
    -> Strategy c        -- evaluation strategy for reduction
    -> ([(k,b)] -> c)    -- function to reduce
    -> [(k,a)]           -- list to map over
    -> c

parMapReduceKey = parMapReduceSimple

{- 
   Google flavour of Map-Reduce, see their original OSDI 2004 paper,
   Laemmel's paper in SCP 2007, or Berthold et al. in EuroPar 2009

   This version is due to Jost Berthold and Hans-Wolfgang Loidl.
-}

parMapReduce :: -- keys have to be comparable and NF-reducible
                   (Eq k, NFData k) =>  
                   ---- Parallelism Parameters:
                   Int        -- input packet size 
                -> Int        -- combiner/reducer packet size
                -> Strategy tmp -- Strategy for mapper
                -> Strategy o   -- Strategy for reducer (output)
                   ---- Functionality
                -> (i -> [(k,tmp)])       -- mapper
                -> ((k,[tmp]) -> Maybe o) -- reducer (per key)
                   ---- Resulting mapping
                -> [i] -> [(k,o)]
                   ----------------------------------------

parMapReduce chunksize redsize mapStrat reduceStrat
                mapF reduceF input
    = result
    where -- mapped :: [[(k,tmp)]]
          mapped   = map mapF input 
                        `using` (parListChunk chunksize mStrat)
          mStrat   = evalList (evalTuple2 rdeepseq mapStrat)

          -------- grouping
          -- groups :: [(k,[tmp])]
          groups   = groupByKeys (concat mapped)
          groupByKeys [] = []
          groupByKeys ((key,val):rest)
               = let (thisKey,otherKeys) = partition sameKey rest
                     sameKey (k,_) = k == key
                 in (key, val:map snd thisKey) : groupByKeys otherKeys

          -------- reduction
          result   = mapMaybe (withKey reduceF) groups
                         `using` (parListChunk redsize rStrat)
          rStrat   = evalTuple2 rdeepseq reduceStrat

          -------- technicality: keep keys in map, propagate "Nothing"
          -- Maybe monad. Yields Nothing if f x == Nothing.
          withKey f x = f x >>= \r -> return (fst x, r)

-- normal form (i.e. full) evaluation, the common case:
parmapreduce :: (Eq k, NFData k, NFData tmp, NFData o) =>
                   Int        -- input packet size 
                -> Int        -- reduction packet size
                   ----------------------------------------
                -> (i -> [(k,tmp)])       -- mapper
                -> ((k,[tmp]) -> Maybe o) -- reducer (per key)
                -> [i] -> [(k,o)]  -- resulting mapping
                   ----------------------------------------
parmapreduce n m = parMapReduce n m rdeepseq rdeepseq

parSearch :: NFData val => (val -> Bool) -> Tree val -> Maybe val
parSearch found tree = 
 let node = rootLabel tree in
   if found node then
      Just node
   else
      let results = map (parSearch found) (subForest tree) `using` parList rdeepseq in
        if null results then Nothing
        else head results
