module MyFFT where 
import Control.Parallel.Strategies hiding (parList)
import Control.Parallel
import Control.DeepSeq

import Strategies
import Data.Complex
import Samples
import System.Environment

-- twiddle factors (basic)
tw :: Int -> Int -> Complex Float
tw n k = cis (-2 * pi * fromIntegral k / fromIntegral n)

-- Fast Fourier Transform.
-- This is the Decimation in Frequency (DIF) radix 2 Cooley-Tukey FFT.


-- Using par - this is far too fine-grained though. 
fftPar :: [Complex Float] -> [Complex Float]
fftPar [a] = [a]
fftPar as = ls `par` rs `pseq` interleave ls rs
  where
    (cs,ds) = bflySP as
    ls = fftPar cs
    rs = fftPar ds

interleave [] bs = bs
interleave (a:as) bs = a : interleave bs as

-- Using parzipwith (which makes little difference) and mapping the tw function onto the list [0..(length ros) - 1]
-- (which makes a big difference).
bflySP :: [Complex Float] -> ([Complex Float], [Complex Float])
bflySP as = (los,rts)
  where
    (ls,rs) = halve as
    los = parzipwith (+) ls rs
    ros = parzipwith (-) ls rs
    rts = parzipwith (*) ros $ parmap (tw (length as)) [0..(length ros) - 1]


-- split the input into two halves
halve as = splitAt n' as
  where
    n' = div (length as + 1) 2

--fftDC :: [Complex Float] -> [Complex Float]
--fftDC as = dc split threshold combine worker as

--split :: [Complex Float] -> [[Complex Float]]
--split as = [left, right]
--    where
--      (left, right) = halve as

--threshold :: [Complex Float] -> Bool
--threshold as = (length as) == 1

--worker :: [Complex Float] -> [[Complex Float]]
--worker as = [lefts, rights]
--    where
--      (lefts, rights) = bflySP as

--combine :: [[Complex Float]] -> [Complex Float]
--combine as = interleave ls rs
--    where
--      ls = as!!0
--      rs = as!!1

--fftMapReduceInner :: [Complex Float] -> [Complex Float]
--fftMapReduceInner as = parMapReduceSimple mappingStrategy mapping reductionStrategy reduction input
--    where
--      mappingStrategy   = parList rdeepseq
--      mapping           = bflySP            -- must be of type (a -> b)
--      reductionStrategy = parList rdeepseq
--      reduction x       = interleave ls rs
--          where
--            (ls, rs) = unzip x 
--      input             = xs

--dftMapreduceSimple :: [Complex Float] -> [Complex Float]
--dftMapreduceSimple xs = parMapReduceSimple mappingStrategy mapping reductionStrategy reduction indexed_xs
--    where 
--      mappingStrategy   = parList rdeepseq
--      mapping (xs, k)   = [ xs!!j * tw n (j*k) | j <- [0..n']]
--      reductionStrategy = parList rdeepseq
--      reduction x       = parfold (\e acc -> sum e : acc) [] x
--      n                 = length xs
--      n'                = n-1
--      indexed_xs        = zip (replicate n xs) [0..n'] 


-- the main function
-- uses Samples.samples to generate some sample data
--   samples :: Int -> Int -> [Complex Float]

defsize = 500 -- change this to get larger samples
defseed = 0

main :: IO()
main = do args <- getArgs
          let arglen = length args
          let n = argval args 0 defsize
          let seed = argval args 1 defseed
          print (sum (fftPar (samples seed n)))

argval args n def = if length args > n then
                       read (args !! n)
                     else def
