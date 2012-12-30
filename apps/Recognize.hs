module Main where

import           Control.Monad
import           Control.Monad.Random
import           System.Environment
import qualified Data.Vector as V

import Hopfield
import ConvertImage
import BolzmannMachine


data Method = Hopfield | Boltzmann deriving (Eq, Enum, Ord, Show)


transformFunction :: Method -> (Int -> Int)
transformFunction Hopfield  = (\x -> 2 * x - 1)
transformFunction Boltzmann = (\x -> x)

toPattern :: Method -> CBinaryPattern -> Pattern
toPattern m (CBinaryPattern { cPattern = pat }) = V.fromList $ map (transformFunction m . fromIntegral) $ pat


recPic :: Method -> (Int, Int) -> [FilePath] -> FilePath -> IO (Maybe FilePath)
recPic method (width, height) imgPaths queryImgPath = do
  l@(_queryImg:_imgs) <- forM (queryImgPath:imgPaths) (\path -> loadPicture path width height)
  gen <- getStdGen
  let queryPat:imgPats = map (toPattern method) l
      runRandom r = evalRand r gen
      result =  case method of
          Hopfield  -> runRandom $ matchPattern (buildHopfieldData imgPats) queryPat
          Boltzmann -> error "Boltzmann not implemented yet"
          --runRandom $ matchPatternBolzmann (runRandom $ buildBolzmannData imgPats) queryPat
  return $ case result of
             Left _pattern -> Nothing -- TODO apply heuristic if we want (we want)
             Right i       -> Just $ imgPaths !! i


-- code left from trials with Boltzmann. Left here until we merge the
-- 2 methods using one interface (TODO)

  --    res =  case method of
  --        Hopfield  -> error "This is a trial for Bolzmann"
  --        Boltzmann -> matchPatternBolzmann (runRandom $ buildBolzmannData imgPats) queryPat
  -- return $ [ (imgPaths !! i, prob) | (i, prob)  <- res]


main :: IO ()
main = do
  -- TODO use an argument parser (cmdargs)
  methodStr:widthStr:heightStr:queryPath:filePaths <- getArgs
  let method = case methodStr of
                 "hopfield"  -> Hopfield
                 "boltzmann" -> Boltzmann
                 _           -> error "unrecognized method"
      width  = read widthStr
      height = read heightStr
  foundPath <- recPic method (width, height) filePaths queryPath
  putStrLn $ show foundPath
