module Main where


import           System.Environment
import           System.Random


import           Control.Monad
import           Control.Monad.Random
import           Control.Applicative
import qualified Data.Vector as V

import Hopfield
import ConvertImage
import BolzmannMachine

data Method = Hopfield | Boltzmann

toPattern :: CBinaryPattern -> Pattern
toPattern (CBinaryPattern { pattern = pat }) = V.fromList $ map ((\x -> 2 * x - 1) . fromIntegral) $ pat


recPic :: Method -> (Int, Int) -> [FilePath] -> FilePath -> IO (Maybe FilePath)
recPic method (width, height) imgPaths queryImgPath = do
  l@(queryImg:imgs) <- forM (queryImgPath:imgPaths) (\path -> loadPicture path width height)
  gen <- getStdGen
  let queryPat:imgPats = map toPattern l
      runRand r = evalRand r gen
  return $ case method of
    Hopfield -> case runRand $ matchPattern (buildHopfieldData imgPats) queryPat of
                   Left pattern -> Nothing -- TODO apply heuristic if we want
                   Right i      -> Just $ imgPaths !! i
    Boltzmann -> error "Boltzmann not implemented"


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
  putStrLn $ case foundPath of
    Nothing  -> "no pattern found"
    Just path -> path
