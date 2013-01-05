module Hopfield.Benchmark where


import Hopfield.Hopfield
import Hopfield.Clusters
import Hopfield.ClusterExperiments


bench1 = print =<< experimentUsingT1NoAvg Hebbian 10 10
bench2 = print =<< performAndPrint T2 20 5 0.0 0.5 0.5 1
