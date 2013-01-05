{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Monad
import           Control.Monad.Random
import qualified Data.Vector as V
import           Options.Applicative

import Hopfield.Common
import Hopfield.Hopfield
import Hopfield.ConvertImage
import Hopfield.RestrictedBoltzmannMachine
import Hopfield.ClassificationBoltzmannMachine



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


data RecognizeArgs = RunOptions
                       { method :: String
                       , width :: Int
                       , height :: Int
                       , queryPath :: String
                       , filePaths :: [String]
                       }
                   | BenchmarkOptions
                       { benchmarkPaths :: [String]
                       }
                   deriving (Show)


runOptions :: Parser RecognizeArgs
runOptions = RunOptions <$> argument str  ( metavar "METHOD"     <> help "hopfield, boltzmann or cboltzmann" )
                        <*> argument auto ( metavar "WIDTH"      <> help "width images are resized to" )
                        <*> argument auto ( metavar "HEIGHT"     <> help "height images are resized to" )
                        <*> argument str  ( metavar "QUERY_PATH" <> help "image to match" )
                        <*> arguments str ( metavar "FILE_PATHS" <> help "images to match against (training set)" )


benchmarkOptions :: Parser RecognizeArgs
benchmarkOptions = BenchmarkOptions <$> arguments str ( metavar "FILE_PATHS" <> help "Target for the greeting" )


recognizeOptions :: Parser RecognizeArgs
recognizeOptions = subparser
  ( command "run" ( info (helper <*> runOptions)
                    ( progDesc "Add a file to the repository" ))
  <> command "bench" (info (helper <*> benchmarkOptions)
                      ( progDesc "run benchmark" ))
  )


recognizeArgParser :: ParserInfo RecognizeArgs
recognizeArgParser = info (helper <*> recognizeOptions)
  ( fullDesc <> header "Performs Hopfield/Boltzmann recognition"
             <> progDesc "To see help on individual commands, run --help on them, e.g. recognize run --help." )


main :: IO ()
main = do
  recArgs <- execParser recognizeArgParser

  case recArgs of

    RunOptions { method, width, height, queryPath, filePaths }
      | width < 1       -> error "width must be > 1"
      | height < 1      -> error "height must be > 1"
      | queryPath == "" -> error "empty query path"
      | filePaths == [] -> error "empty query path"
      | otherwise -> do

        let recMethod = case method of
                          "hopfield"   -> Hopfield
                          "boltzmann"  -> Boltzmann
                          "cboltzmann" -> CBoltzmann
                          _            -> error "unrecognized method"

        print =<< recPic recMethod (width, height) filePaths queryPath

    BenchmarkOptions { benchmarkPaths = _bp } -> error "benchmark not implemented"
