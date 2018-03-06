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

-- a summation function
sum' :: [Complex Float] -> Complex Float
sum' = parfold (+) (0.0 :+ 0.0)

-- Discrete Fourier Transform -- O(n^2)
dft :: [Complex Float] -> [Complex Float]
dft xs = [ sum' [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]
  where
    n = length xs
    n' = n-1

-- the main function
-- uses Samples.samples to generate some sample data
--   samples :: Int -> Int -> [Complex Float]

defsize = 1000 -- change this to get larger samples
defseed = 1

main = do args <- getArgs
          let arglen = length args
          let n = argval args 0 defsize
          let seed = argval args 1 defseed
          let fun = if arglen > 2 && args !! 2 == "dft" then dft else fft
          print (sum (fun (samples seed n)))

argval args n def = if length args > n then
                       read (args !! n)
                     else def
