{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Codec.Picture
import           Control.Monad
import           Data.Vector ((!))
import qualified Data.Vector as V
import           Options.Applicative
import           Text.Printf
import           System.Directory

import Hopfield.Common
import Hopfield.Hopfield
import Hopfield.ConvertImage
import Hopfield.RestrictedBoltzmannMachine
import Hopfield.ClassificationBoltzmannMachine
import Hopfield.Benchmark
import Hopfield.Util


-- TODO niklas make --fixseed command line option for deterministic results

transformFunction :: Method -> (Int -> Int)
transformFunction Hopfield  = (\x -> 2 * x - 1)
transformFunction _ = id

toPattern :: Method -> CBinaryPattern -> Pattern
toPattern m (CBinaryPattern { cPattern = pat }) = V.fromList $ map (transformFunction m . fromIntegral) $ pat


-- | Generates a black-white pixel value from the given pattern.
-- Returns: 'maxBound' if > 0, otherwise 'minBound' for any numeric output type
-- (e.g. 0/255 for Word8).
genPixelBW :: (Bounded a) => Pattern -> Int -> Int -> Int -> a
genPixelBW pattern x y width | pattern ! (y + x * width) > 0 = maxBound
                             | otherwise                     = minBound


-- | Converts a 'Pattern' to a 8-bit black-white image.
patternToBwImage :: Pattern -> Int -> Int -> Image Pixel8
patternToBwImage pattern width height = generateImage (genPixelBW pattern width) width height


recPic :: Method -> (Int, Int) -> [FilePath] -> FilePath -> IO (Either (Image Pixel8) FilePath)
recPic method (width, height) imgPaths queryImgPath = do
  l@(_queryImg:_imgs) <- forM (queryImgPath:imgPaths) (\path -> loadPicture path width height)
  let queryPat:imgPats = map (toPattern method) l

  result <- case method of
              Hopfield   -> matchPattern (buildHopfieldData Storkey imgPats) queryPat
              Boltzmann  -> do d <- buildBoltzmannData imgPats
                               Right <$> matchPatternBoltzmann d queryPat
              CBoltzmann -> do d <- buildCBoltzmannData imgPats
                               return . Right $ matchPatternCBoltzmann d queryPat

  return $ case result of
             -- TODO apply heuristic instead of returning pattern as image (only required for Hopfield)
             Left pattern -> Left $ patternToBwImage pattern width height
             Right i      -> Right $ imgPaths !! i


saveChain :: Method -> (Int, Int) -> [FilePath] -> FilePath -> IO ()
saveChain method (width, height) imgPaths queryImgPath = do
  l@(_queryImg:_imgs) <- forM (queryImgPath:imgPaths) (\path -> loadPicture path width height)
  let queryPat:imgPats = map (toPattern method) l

  case method of
    Hopfield -> do chain <- updateChain (buildHopfieldData Storkey imgPats) queryPat
                   mapM_ (putStrLn . patternToAsciiArt width) chain
                   cleanupDir
                   mapM_ save $ zip [(0::Int)..] chain
    m        -> error $ "saving convergence chains for method " ++ show m ++ " not yet implemented"

  where
    save (number, pattern) = do let filename = printf "converged-images/%.6d.bmp" number

                                createDirectoryIfMissing True "converged-images"
                                writeBitmap filename (patternToBwImage pattern width height)

    cleanupDir = removeDirectoryRecursive "converged-images"


data RecognizeArgs = RunOptions
                       { method :: String
                       , width :: Int
                       , height :: Int
                       , queryPath :: String
                       , filePaths :: [String]
                       , saveAllPatterns :: Bool
                       }
                   | BenchmarkOptions
                       { benchmarkPaths :: [String]
                       }
                   | InbuiltBenchmarkOptions
                       { benchmarkName :: String
                       }
                   deriving (Show)


runOptions :: Parser RecognizeArgs
runOptions = RunOptions <$> argument str  ( metavar "METHOD"     <> help "hopfield, boltzmann or cboltzmann" )
                        <*> argument auto ( metavar "WIDTH"      <> help "width images are resized to" )
                        <*> argument auto ( metavar "HEIGHT"     <> help "height images are resized to" )
                        <*> argument str  ( metavar "QUERY_PATH" <> help "image to match" )
                        <*> arguments str ( metavar "FILE_PATHS" <> help "images to match against (training set)" )
                        <*> switch ( long "save-all-patterns" <> help "save all intermediate patterns to harddisk" )


benchmarkOptions :: Parser RecognizeArgs
benchmarkOptions = BenchmarkOptions <$> arguments str ( metavar "FILE_PATHS" <> help "Target for the greeting" )


inbuiltBenchmarkOptions :: Parser RecognizeArgs
inbuiltBenchmarkOptions = InbuiltBenchmarkOptions <$> argument str ( metavar "NAME" <> help "Name of the inbuilt benchmark" )


recognizeOptions :: Parser RecognizeArgs
recognizeOptions = subparser
  ( command "run" ( info (helper <*> runOptions)
                    ( progDesc "Add a file to the repository" ))
  <> command "bench" (info (helper <*> benchmarkOptions)
                      ( progDesc "run benchmark" ))
  <> command "inbuiltbench" (info (helper <*> inbuiltBenchmarkOptions)
                      ( progDesc "run inbuilt benchmark" ))
  )


recognizeArgParser :: ParserInfo RecognizeArgs
recognizeArgParser = info (helper <*> recognizeOptions)
  ( fullDesc <> header "Performs Hopfield/Boltzmann recognition"
             <> progDesc "To see help on individual commands, run --help on them, e.g. recognize run --help." )


main :: IO ()
main = do
  recArgs <- execParser recognizeArgParser

  case recArgs of

    RunOptions { method, width, height, queryPath, filePaths, saveAllPatterns }
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

        if saveAllPatterns
          then
            saveChain recMethod (width, height) filePaths queryPath
          else do
            foundPathOrImage <- recPic recMethod (width, height) filePaths queryPath
            case foundPathOrImage of
              Right path    -> putStrLn path
              Left image -> do let convergedPath = "converged.bmp"
                                -- TODO handle return
                               _ <- writeBitmap convergedPath image
                               putStrLn $ "no pattern found, wrote coverged image to " ++ convergedPath


    BenchmarkOptions { benchmarkPaths = _bp } -> error "benchmark not implemented"

    InbuiltBenchmarkOptions { benchmarkName } -> case benchmarkName of
      "bench1" -> bench1
      "bench2" -> bench2
      _        -> error "unknown benchmark name"
