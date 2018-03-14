module MyDFT where
import Control.Parallel.Strategies hiding (parList)
import Control.Parallel
import Control.DeepSeq

import Strategies
import Data.Complex
import Data.Matrix
import Samples
import System.Environment

-- twiddle factors
tw :: Int -> Int -> Complex Float
tw n k = cis (-2 * pi * fromIntegral k / fromIntegral n)


-- summation function (recursive pattern-matching with par and pseq, not very effective as the parallelism 
-- is extremely limited and only applies to a very cheap operation - too fine-grained.)
sum' :: [Complex Float] -> Complex Float
sum' [] = 0.0 :+ 0.0
sum' (x:xs) = x `par` sum xs `pseq` x + sum xs


-- summation function (parallel fold, uses rdeepseq. Much more effective.)
sum'' :: [Complex Float] -> Complex Float
sum'' = parfold (+) (0.0 :+ 0.0)


dft' :: [Complex Float] -> [Complex Float]
dft' xs = [ sum [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]
    where
      n = length xs
      n' = n-1

{------------------------------------------------
 ------------------------------------------------
 Data Parallel Strategies

 First we'll try out individual strategies before
 trying to combine them.
 ------------------------------------------------
 ------------------------------------------------}

-- Parallelising the summation. These are fairly self-explanatory.

dftSumRecursive :: [Complex Float] -> [Complex Float]
dftSumRecursive xs = [ sum' [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]
    where
      n = length xs
      n' = n-1


dftSumFold :: [Complex Float] -> [Complex Float]
dftSumFold xs = [ sum'' [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]
    where
      n = length xs
      n' = n-1

-- Parallelising by MapReduce

-- Parallel DFT (using MapReduceSimple, treat the reduction like another map)
dftMapreduceSimple :: [Complex Float] -> [Complex Float]
dftMapreduceSimple xs = parMapReduceSimple mappingStrategy mapping reductionStrategy reduction indexed_xs
    where 
      mappingStrategy   = parList rdeepseq
      mapping (xs, k)   = [ xs!!j * tw n (j*k) | j <- [0..n']]
      reductionStrategy = parList rdeepseq
      reduction x       = parfold (\e acc -> sum e : acc) [] x
      n                 = length xs
      n'                = n-1
      indexed_xs        = zip (replicate n xs) [0..n']  -- This line is the main issue: overhead! Doing this efficiently
                                                        -- is key to good performance with this particular approach.

dftMapreduceSimpleOnlyk :: [Complex Float] -> [Complex Float]
dftMapreduceSimpleOnlyk xs = parMapReduceSimple mappingStrategy mapping reductionStrategy reduction indices
    where
      mappingStrategy   = parList rdeepseq
      mapping (k)       = [ xs!!j * tw n (j*k) | j <- indices ]
      reductionStrategy = parList rdeepseq
      reduction         = parfold (\e acc -> sum e : acc) []
      n                 = length xs
      n'                = n - 1
      indices           = [0..n'] -- Optimising from above - don't replicate the input! You only need the values of k. 
                                  -- But then you have to have xs as global data


-- Using mapreduce in the Google style
dftMapReduce :: [Complex Float] -> [Complex Float]
dftMapReduce xs = concat $ parmap snd (parmapreduce inputPackSize reducPackSize mapper reducer input)
    where
      inputPackSize     = 100
      reducPackSize     = 100
      mapper (k, xs)    = [(k, [ xs!!j * tw n (j*k) | j <- [0..n']])]
      reducer (k, tmp)  = if length tmp == 0 
                            then Nothing
                            else Just $ parfold (\e acc -> sum e : acc) [] tmp
      input             = zip [0..n'] (replicate n xs)
      n = length xs
      n' = n - 1

dftBSP :: [Complex Float] -> [Complex Float]
dftBSP xs = bsp [dftBSPfunc1] ns xs
    where
      ns = [0..n']
      n' = (length xs) - 1


dftBSPfunc1 k xs = (k, res)
    where
      res = sum [xs!!j * tw n (j*k) | j <- [0..n']]
      n = length xs
      n' = n - 1

-- Another approach: treat the DFT like a gigantic matrix-vector multiplication. Construct the twiddle matrix,
-- multiply it with the input vector.

--dft_matrix_multiply :: [Complex Float] -> [Complex Float]
--dft_matrix_multiply xs = twMatrix * xs
--    where
--      twMatrix = matrix n n $ \(j, k) -> tw n (j*k)
--      n = length xs

{------------------------------------------------
 ------------------------------------------------
 Task Parallel Strategies
 ------------------------------------------------
 ------------------------------------------------}

-- Parallelising by Divide and Conquer

-- Using `par` and `pseq` with a parallel sum on the two halves of the list - this is actually pretty fast!
myDftHalved :: [Complex Float] -> [Complex Float]
myDftHalved xs = firstHalf `par` secondHalf `pseq` (map sum'' firstHalf) ++ (map sum'' secondHalf)
    where
      firstHalf  = [[ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..halfN]]
      secondHalf = [[ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [halfN+1..n']]
      n = length xs
      n' = n-1
      halfN = n' `div` 2


-- Generalising the one above by splitting down recursively - this is too fine-grained and the overhead 
-- dominates. The optimum depth is likely found by a threshold. Note that despite divide and conquer
-- this is not the FFT - we still make n^2 operations.
myDftDCGeneralised :: [Complex Float] -> [Complex Float]
myDftDCGeneralised xs = parmap sum'' result
    where
      result = myDftDCGeneralised' xs (length xs) 0 ((length xs) - 1)

myDftDCGeneralised' :: [Complex Float] -> Int -> Int -> Int -> [[Complex Float]]
myDftDCGeneralised' xs n start stop = a `par` b `pseq` (a ++ b)
    where
      n' = n - 1
      half = (start + stop) `div` 2
      a
        | start == stop = [[ xs!!j * tw n (j*start) | j <- [0..half]]]
        | otherwise     = myDftDCGeneralised' xs n start half
      b 
        | start == stop = [[ xs!!j * tw n (j*start) | j <- [half+1..n']]]
        | otherwise     = myDftDCGeneralised' xs n (half+1) stop


-- Now includes thresholding. This does even better. One optimisation is to find the length of the
-- list, n, once. Then pass it as an argument, dividing it along the way. Avoids having to recalculate
-- it every time.
myDftDCThresholded :: [Complex Float] -> [Complex Float]
myDftDCThresholded xs = parmap sum'' result
    where
      result = myDftDCThresholded' xs (length xs) 0 ((length xs) - 1)

myDftDCThresholded' :: [Complex Float] -> Int -> Int -> Int -> [[Complex Float]]
myDftDCThresholded' xs n start stop = a `par` b `pseq` (a ++ b)
    where
      n' = n - 1
      half = (start + stop) `div` 2
      a
        | (stop - start) < 400 = [[ xs!!j * tw n (j*k) | j <- [0..half]] | k <- [start..stop]]
        | otherwise            = myDftDCThresholded' xs n start half
      b
        | (stop - start) < 400 = [[ xs!!j * tw n (j*k) | j <- [half+1..n']] | k <- [start..stop]]
        | otherwise            = myDftDCThresholded' xs n (half+1) stop


-- Strategic implementation of divide and conquer for DFT.
dftDC :: [Complex Float] -> [Complex Float]
dftDC xs = dc split threshold combine worker [0..n']
    where
      split as     = let (left, right) = halve as in [left, right]
      threshold as = (length as) == 1
      combine      = concat
      worker       = \[k] -> [[sum [xs!!j * tw n (j*k) | j <- [0..n']]]]
      n            = length xs
      n'           = n - 1


--split :: [Complex Float] -> [[Complex Float]]

halve as = splitAt n' as
  where
    n' = div (length as + 1) 2

--threshold :: [Complex Float] -> Bool

--combine :: [[Complex Float]] -> [Complex Float]

--worker :: [Complex Float] -> [[Complex Float]]



--parstream6 f1 f2 f3 f4 f5 f6 = 
--   parpipeline6 (map f1) (map f2) (map f3) (map f4)  (map f5) (map f6)

--parpipeline6 f1 f2 f3 f4 f5 f6 x =
--  f5 $|| rdeepseq $
--  f4 $|| rdeepseq $
--  f3 $|| rdeepseq $
--  f2 $|| rdeepseq $
--  f1 $|| rdeepseq $
--  x

-- dft' xs = [ sum [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]

--dftStream :: [Complex Float] -> [Complex Float]
--dftStream xs = [ sum $ stream k range | k <- range]
--    where
--      range     = [0..n']
--      n         = length xs
--      n'        = n - 1
--      intN      = fromIntegral n
--      f1 k j    = (j*k, j)
--      f2 (t, j) = (fromIntegral t, j)
--      f3 (t, j) = (pi * t, j)
--      f4 (t, j) = ((-2) * t, j)
--      f5 (t, j) = (t / intN, j)
--      f6 (t, j) = xs!!j * (cis t)
--      stream    = \k -> (parstream6 (f1 k) f2 f3 f4 f5 f6)

--parpipeline6 f1 f2 f3 f4 f5 f6 x =
--  f5 $|| rdeepseq $
--  f4 $|| rdeepseq $
--  f3 $|| rdeepseq $
--  f2 $|| rdeepseq $
--  f1 $|| rdeepseq $
--  x

defsize = 500 -- change this to get larger samples
defseed = 0

main = do args <- getArgs
          let arglen = length args
          let n = argval args 0 defsize
          let seed = argval args 1 defseed
          print (sum (dft' (samples seed n)))

argval args n def = if length args > n then
                       read (args !! n)
                     else def





