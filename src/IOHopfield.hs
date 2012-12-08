module IOHopfield(
  getPatternsFromPicture
  ) where

import           Hopfield
import           ConvertImage
import           Data.Functor
import qualified Data.Vector as V
import           Control.Monad.Random


toIOPattern :: IO CBinaryPattern -> IO Pattern
toIOPattern io_cpat = toPattern <$> io_cpat
  where
    toPattern cpat = V.fromList $ map ((\x -> 2 * x - 1) . fromIntegral) $ pattern cpat

buildHopfieldFromStrings w h = buildIO . (mapM $ getPatternsFromPicture w h)

--getPatternsFromPictures:: [String] -> IO [Pattern]
--getPatternsFromPictures pics =


getPatternsFromPicture :: Int -> Int -> String -> IO Pattern
getPatternsFromPicture w h = toIOPattern. (\x -> loadPicture x w h)

buildIO :: IO [Pattern] -> IO HopfieldData
buildIO = fmap buildHopfieldData


matchPatternIO :: IO HopfieldData -> IO Pattern -> IO (Either Pattern Int)
matchPatternIO io_hd io_pat = do
    pat  <- io_pat
    hd   <- io_hd
    gen  <- getStdGen
    return $ evalRand (matchPattern hd pat) gen



