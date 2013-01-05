module Hopfield.Benchmark where


import Hopfield.Hopfield
import Hopfield.Clusters


bench1 = print =<< experimentUsingT1NoAvg Hebbian 10 10
