module Main where


import           Options.Applicative

import qualified Hopfield.Experiments.Experiment
import qualified Hopfield.Experiments.SmallExperiments
import qualified Hopfield.Experiments.ClusterExperiments
import qualified Hopfield.Experiments.Experiment2SuperAttractors


data ExperimentArgs = ExperimentArgs { experimentName :: String
                                     , experimentArgs :: [String]
                                     } deriving (Show)


argParser :: ParserInfo ExperimentArgs
argParser = info (helper <*> options) ( fullDesc <> header "Runs Hopfield experiments" )
  where
    options = ExperimentArgs <$>       argument str ( metavar "EXPERIMENT" <> help "the name of the experiment to run" )
                             <*> many (argument str ( metavar "EXPERIMENT_ARGS" <> help "experiment parameters" ))


main :: IO ()
main = do

  ExperimentArgs expName args <- execParser argParser

  case expName of
    "experiment" -> Hopfield.Experiments.Experiment.main
    "small"      -> Hopfield.Experiments.SmallExperiments.main
    "cluster"    -> Hopfield.Experiments.ClusterExperiments.run args
    "super"      -> Hopfield.Experiments.Experiment2SuperAttractors.main
    _            -> error "unknown experiment"
