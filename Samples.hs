module Samples where

import System.Random
import Normal
import Data.Complex

generate2DSamplesList ::
                     Int           -- random number seed
                  -> Int           -- number of samples to generate
                  -> Float -> Float    -- X and Y mean
                  -> Float -> Float    -- X and Y standard deviations
                  -> [Complex Float]
generate2DSamplesList seed n mx my sdx sdy  = do
  let gen = mkStdGen seed in
    let (genx, geny) = split gen
        xsamples = normals' (mx,sdx) genx
        ysamples = normals' (my,sdy) geny
    in
      zipWith (:+) (take n xsamples) ysamples

-- generating input for FFT or DFT.
mX, mY, sdX, sdY :: Float
mX = 0
mY = 0
sdX = 0.5
sdY = 1.5    

-- Produce a number of samples
samples seed n = generate2DSamplesList seed n mX mY sdX sdY

samplesi seed max n = 
    let g = mkStdGen seed in
        take n (map (floor . (*) max) (normals g :: [Double]))
