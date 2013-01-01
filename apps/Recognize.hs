module Main where

import           Control.Monad
import           Control.Monad.Random
import           System.Environment
import qualified Data.Vector as V

import Hopfield
import ConvertImage
import RestrictedBoltzmannMachine
import ClassificationBoltzmannMachine


data Method = Hopfield | Boltzmann | CBoltzmann
  deriving (Eq, Enum, Ord, Show)


transformFunction :: Method -> (Int -> Int)
transformFunction Hopfield  = (\x -> 2 * x - 1)
transformFunction _ = id

toPattern :: Method -> CBinaryPattern -> Pattern
toPattern m (CBinaryPattern { cPattern = pat }) = V.fromList $ map (transformFunction m . fromIntegral) $ pat


recPic :: Method -> (Int, Int) -> [FilePath] -> FilePath -> IO (Maybe FilePath)
recPic method (width, height) imgPaths queryImgPath = do
  l@(_queryImg:_imgs) <- forM (queryImgPath:imgPaths) (\path -> loadPicture path width height)
  gen <- getStdGen
  let queryPat:imgPats = map (toPattern method) l
      runRandom r = evalRand r gen
      result =  case method of
          Hopfield   -> runRandom $ matchPattern (buildHopfieldData Storkey imgPats) queryPat
          Boltzmann  -> Right $ runRandom $ matchPatternBoltzmann (runRandom $ buildBoltzmannData imgPats) queryPat
          CBoltzmann -> Right $ matchPatternCBoltzmann (runRandom $ buildCBoltzmannData imgPats) queryPat
  return $ case result of
             Left _pattern -> Nothing -- TODO apply heuristic if we want (we want)
                                      -- only required for Hopfield
             Right i       -> Just $ imgPaths !! i


main :: IO ()
main = do
  -- TODO use an argument parser (cmdargs)
  methodStr:widthStr:heightStr:queryPath:filePaths <- getArgs
  let method = case methodStr of
                 "hopfield"   -> Hopfield
                 "boltzmann"  -> Boltzmann
                 "cboltzmann" -> CBoltzmann
                 _            -> error "unrecognized method"
      width  = read widthStr
      height = read heightStr
  foundPath <- recPic method (width, height) filePaths queryPath
  putStrLn $ show foundPath
