module Main where


import           Options.Applicative

import qualified Hopfield.Experiment
import qualified Hopfield.SmallExperiments
import qualified Hopfield.ClusterExperiments
import qualified Hopfield.ExperimentSuper2


data ExperimentArgs = ExperimentArgs { experimentName :: String
                                     , experimentArgs :: [String]
                                     } deriving (Show)


argParser :: ParserInfo ExperimentArgs
argParser = info (helper <*> options) ( fullDesc <> header "Runs Hopfield experiments" )
  where
    options = ExperimentArgs <$> argument str ( metavar "EXPERIMENT" <> help "the name of the experiment to run" )
                             <*> arguments str ( metavar "EXPERIMENT_ARGS" <> help "experiment parameters" )


main :: IO ()
main = do

  ExperimentArgs expName args <- execParser argParser

  case expName of
    "experiment" -> Hopfield.Experiment.main
    "small"      -> Hopfield.SmallExperiments.main
    "cluster"    -> Hopfield.ClusterExperiments.run args
    "super"      -> Hopfield.ExperimentSuper2.main
