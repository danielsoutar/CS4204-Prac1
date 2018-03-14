import Criterion.Main
import Data.Complex
import Data.String
import FFT hiding (main, argval)
import MyDFT hiding (main, argval)
import MyFFT hiding (main, argval)
import Samples
import System.Environment

defmin = 10
defmax = 1000000
defseed = 0

{--------------
 Testing file to generate performance statistics - if it cannot be measured and isn't reproducible, it isn't science!

 Format:
 ./Tester <n> <seed> <file> <function index>  

 Example:
 ./Tester 0 2000 0 MyDFT 0 
 --------------}

mydftfunctions :: [([Complex Float] -> [Complex Float])]
mydftfunctions = [MyDFT.dftSumRecursive, MyDFT.dftSumFold, MyDFT.dftMapreduceSimple, MyDFT.myDftHalved, 
                  MyDFT.myDftDCGeneralised, MyDFT.myDftDCThresholded, MyDFT.dftBSP, MyDFT.dftMapReduce, 
                  MyDFT.dftDC, MyDFT.dftMapreduceSimpleOnlyk]

myfftfunctions :: [([Complex Float] -> [Complex Float])]
myfftfunctions = [MyFFT.fftPar]

fftfunctions :: [([Complex Float] -> [Complex Float])]
fftfunctions = [FFT.dft, FFT.fft]

main :: IO()
main = do args <- getArgs
          let arglen = length args
          let n = argval args 0 defmin
          let seed = argval args 1 Main.defseed
          let name = args !! 2
          let func = if arglen > 2 && name == "MyDFT" 
                      then mydftfunctions !! (argval args 3 0)
                      else if name == "MyFFT"
                        then myfftfunctions !! (argval args 3 0)
                        else fftfunctions !! (argval args 3 0)
          print ( sum (func (samples seed n)) )
          --let inputs = [samples seed n | n <- [256, 512, 1024, 2048, 4096]]
          --doTesting name inputs func

doTesting name inputs func = defaultMain [ 
                                          bgroup name [ bench "256" $ nf func (inputs !! 0),
                                                        bench "512" $ nf func (inputs !! 1),
                                                        bench "1024" $ nf func (inputs !! 2),
                                                        bench "2048" $ nf func (inputs !! 3),
                                                        bench "4096" $ nf func (inputs !! 4)
                                                      ]
                                         ]

argval args n def = if length args > n 
                      then read (args !! n)
                        else def
