	Sat Jan  5 20:09 2013 Time and Allocation Profiling Report  (Final)

	   experiments +RTS -p -RTS

	total time  =        5.91 secs   (5905 ticks @ 1000 us, 1 processor)
	total alloc = 7,444,305,000 bytes  (excludes profiling overheads)

COST CENTRE           MODULE      %time %alloc

randomElem            Util         25.8   10.2
computeH_.weighted    Hopfield     25.6   47.9
*.                    Util          9.2   12.1
shuffle               Util          9.0    8.3
validWeights          Hopfield      7.2    1.8
update_.flipAtIndex   Hopfield      5.8    9.1
getUpdatables_        Hopfield      3.5    3.9
repeatUntilNothing    Util          1.7    0.4
getUpdatables_.new    Hopfield      1.7    1.2
train.w               Hopfield      1.5    1.7
sampleHammingDistance Measurement   1.4    1.1
shuffle.shuffledArray Util          1.2    0.2
computeH_             Hopfield      1.2    0.0
update_.valueAtIndex  Hopfield      1.2    0.0
update_               Hopfield      1.1    0.5


                                                                                                 individual     inherited
COST CENTRE                                 MODULE                             no.     entries  %time %alloc   %time %alloc

MAIN                                        MAIN                               134           0    0.0    0.0   100.0  100.0
 main                                       Main                               269           0    0.0    0.0   100.0  100.0
  experimentUsingT1NoAvg                    Clusters                           376           0    0.0    0.0    95.1   97.9
   avgBasinsGivenPats                       Clusters                           389           6    0.0    0.0     0.1    0.1
    avgBasinsGivenPats.hopfield             Clusters                           408           6    0.0    0.0     0.0    0.0
     buildHopfieldData                      Hopfield                           409           6    0.0    0.0     0.0    0.0
      train                                 Hopfield                           413           6    0.0    0.0     0.0    0.0
       vector2D                             Util                               416           6    0.0    0.0     0.0    0.0
       train.ws                             Hopfield                           415           6    0.0    0.0     0.0    0.0
        ./.                                 Util                               419         546    0.0    0.0     0.0    0.0
        train.w                             Hopfield                           418         600    0.0    0.0     0.0    0.0
       train.n                              Hopfield                           414           6    0.0    0.0     0.0    0.0
      buildHopfieldData.trainingFunction    Hopfield                           412           6    0.0    0.0     0.0    0.0
      buildHopfieldData.\                   Hopfield                           411          60    0.0    0.0     0.0    0.0
      buildHopfieldData.first_len           Hopfield                           410           6    0.0    0.0     0.0    0.0
    measurePatternBasin                     Measurement                        391          60    0.0    0.0     0.1    0.1
     samplePatternBasin                     Measurement                        393          60    0.0    0.0     0.1    0.1
      samplePatternRing                     Measurement                        396         600    0.0    0.0     0.1    0.1
       repeatedUpdate                       Hopfield                           404         600    0.0    0.0     0.0    0.0
       sampleHammingDistance                Measurement                        398         600    0.0    0.0     0.1    0.1
        sampleHammingDistance.multByPat     Measurement                        432           0    0.0    0.0     0.0    0.0
        sampleHammingDistance.n             Measurement                        429         600    0.0    0.0     0.0    0.0
        sampleHammingDistance.basePerm      Measurement                        426         600    0.0    0.0     0.0    0.0
         toArray                            Util                               427         600    0.0    0.0     0.0    0.0
          toArray.l                         Util                               428         600    0.0    0.0     0.0    0.0
        sampleHammingDistance.coeffSamples  Measurement                        400         600    0.0    0.1     0.0    0.1
         shuffle                            Util                               402         600    0.0    0.0     0.0    0.0
          shuffle.len                       Util                               425         600    0.0    0.0     0.0    0.0
          shuffle.shuffledArray             Util                               424           0    0.0    0.0     0.0    0.0
      samplePatternBasin.n                  Measurement                        394          60    0.0    0.0     0.0    0.0
   basinsGivenProbabilityT1                 Clusters                           380           0    0.0    0.0    95.0   97.8
    avgBasinsGivenPats                      Clusters                           390           0    0.0    0.0    94.9   97.8
     average                                Util                               450           6    0.0    0.0     0.0    0.0
     measurePatternBasin                    Measurement                        392           0    0.0    0.0    94.9   97.8
      samplePatternBasin                    Measurement                        395           0    0.2    0.1    94.9   97.8
       samplePatternRing                    Measurement                        397           0    0.4    0.0    94.8   97.7
        ./.                                 Util                               452         114    0.0    0.0     0.0    0.0
        samplePatternRing.numConverging     Measurement                        451         114    0.0    0.0     0.0    0.0
        repeatedUpdate                      Hopfield                           434           0    0.0    0.0    77.0   85.4
         repeatedUpdate_                    Hopfield                           435       60000    0.1    0.1    77.0   85.4
          repeatUntilNothing                Util                               436      281761    1.7    0.4    76.8   85.3
           update_                          Hopfield                           437      281761    1.1    0.5    75.1   84.9
            update_.flipAtIndex             Hopfield                           447      221761    5.8    9.1     6.9    9.2
             update_.valueAtIndex           Hopfield                           448     2217610    1.2    0.0     1.2    0.0
            randomElem                      Util                               446      221761   25.8   10.2    25.8   10.2
            update_.updatables              Hopfield                           438      281761    0.1    0.0    41.2   65.1
             getUpdatables_                 Hopfield                           439      281761    3.5    3.9    41.1   65.1
              getUpdatables_.new            Hopfield                           441     2817610    1.7    1.2    37.6   61.2
               computeH_                    Hopfield                           443     2817610    1.2    0.0    35.9   60.0
                computeH_.weighted          Hopfield                           444     2817610   25.5   47.8    34.6   60.0
                 *.                         Util                               445    28176100    9.1   12.1     9.1   12.1
               computeH_.p                  Hopfield                           442     2817610    0.0    0.0     0.0    0.0
        checkWsPat                          Hopfield                           405       60000    0.2    0.0     4.6    1.7
         validWeightsPatternSize            Hopfield                           433       60000    0.0    0.0     0.0    0.0
         validPattern                       Hopfield                           422       60000    0.2    0.0     0.2    0.0
         validWeights                       Hopfield                           406       60000    4.2    1.7     4.2    1.7
          validWeights.\                    Hopfield                           417      600000    0.0    0.0     0.0    0.0
          validWeights.n                    Hopfield                           407       60000    0.0    0.0     0.0    0.0
        sampleHammingDistance               Measurement                        399           0    1.4    1.1    12.8   10.6
         sampleHammingDistance.multByPat    Measurement                        423       60000    0.5    1.0     0.5    1.0
         sampleHammingDistance.coeffSamples Measurement                        401           0    0.2    0.0    10.9    8.5
          shuffle                           Util                               403           0    8.9    8.3    10.8    8.5
           shuffle.shuffledArray            Util                               430       60000    1.2    0.2     1.8    0.2
            shuffle.shuffledArray.\         Util                               431      600000    0.6    0.0     0.6    0.0
    getCluster                              Clusters                           385           6    0.0    0.0     0.0    0.0
     getPatternInCluster                    Clusters                           386           6    0.0    0.0     0.0    0.0
      getPatternInCluster.transformBit      Clusters                           387          60    0.0    0.0     0.0    0.0
       getPatternInCluster.transformBit.bit Clusters                           420         600    0.0    0.0     0.0    0.0
        flipBit                             Common                             449          45    0.0    0.0     0.0    0.0
       gibbsSampling                        Util                               388           6    0.0    0.0     0.0    0.0
    randomSignVector                        Util                               382           0    0.0    0.0     0.0    0.0
     randomSignVector.\                     Util                               421          60    0.0    0.0     0.0    0.0
     randomBinaryVector                     Util                               384           0    0.0    0.0     0.0    0.0
  main.originIndex                          Main                               347           1    0.0    0.0     0.0    0.0
  doCheckFixed                              ExpUtil                            340           1    0.0    0.0     3.2    0.2
   prettyList                               Util                               374           1    0.0    0.0     0.0    0.0
   doCheckFixed.msg                         ExpUtil                            371           1    0.0    0.0     0.0    0.0
   doCheckFixed.patErrs                     ExpUtil                            341           1    0.0    0.0     3.2    0.2
    checkFixed                              Measurement                        348           6    0.0    0.0     3.2    0.2
     checkFixed.\                           Measurement                        349         551    0.0    0.0     3.2    0.2
      compTerm                              Measurement                        350         551    0.0    0.0     3.2    0.2
       computeH                             Hopfield                           352         551    0.0    0.0     3.2    0.2
        checkWsPat                          Hopfield                           353         551    0.0    0.0     3.2    0.2
         computeH.\                         Hopfield                           366         551    0.0    0.0     0.2    0.1
          computeH_                         Hopfield                           368         551    0.0    0.0     0.2    0.1
           computeH_.weighted               Hopfield                           369         551    0.1    0.1     0.2    0.1
            *.                              Util                               370       55100    0.1    0.0     0.1    0.0
          computeH_.p                       Hopfield                           367         551    0.0    0.0     0.0    0.0
         validWeightsPatternSize            Hopfield                           365         551    0.0    0.0     0.0    0.0
         validPattern                       Hopfield                           364         551    0.0    0.0     0.0    0.0
         validWeights                       Hopfield                           354         551    3.0    0.1     3.0    0.1
          validWeights.\                    Hopfield                           361       55100    0.0    0.0     0.0    0.0
          validWeights.n                    Hopfield                           355         551    0.0    0.0     0.0    0.0
       compTerm.pat                         Measurement                        351           6    0.0    0.0     0.0    0.0
  main.nets                                 Main                               338           1    0.0    0.0     1.6    1.8
   buildNetworks                            SuperAttractors                    339           1    0.0    0.0     1.6    1.8
    oneSuperAttr                            SuperAttractors                    344           6    0.0    0.0     0.0    0.0
    buildHopfieldData                       Hopfield                           342           6    0.0    0.0     1.6    1.8
     train                                  Hopfield                           357           6    0.0    0.0     1.6    1.8
      vector2D                              Util                               360           6    0.1    0.0     0.1    0.0
      train.ws                              Hopfield                           359           6    0.1    0.1     1.6    1.8
       ./.                                  Util                               363       59406    0.0    0.0     0.0    0.0
       train.w                              Hopfield                           362       60000    1.4    1.7     1.4    1.7
      train.n                               Hopfield                           358           6    0.0    0.0     0.0    0.0
     buildHopfieldData.trainingFunction     Hopfield                           356           6    0.0    0.0     0.0    0.0
     buildHopfieldData.\                    Hopfield                           346         111    0.0    0.0     0.0    0.0
     buildHopfieldData.first_len            Hopfield                           345           6    0.0    0.0     0.0    0.0
  main.dist                                 Main                               317           1    0.0    0.0     0.0    0.0
   hammingDistribution                      Measurement                        320           0    0.0    0.0     0.0    0.0
  patternGen                                Utils                              310           0    0.0    0.0     0.0    0.0
   toGenVector                              Utils                              311           0    0.0    0.0     0.0    0.0
    signGen                                 Utils                              333           0    0.0    0.0     0.0    0.0
  doHamming                                 ExpUtil                            298           1    0.0    0.0     0.0    0.0
   doHamming.n                              ExpUtil                            337           1    0.0    0.0     0.0    0.0
   toPercents                               Util                               335           1    0.0    0.0     0.0    0.0
   doHamming.hammingPct                     ExpUtil                            334           1    0.0    0.0     0.0    0.0
    ./.                                     Util                               336           8    0.0    0.0     0.0    0.0
   doHamming.hammingDists                   ExpUtil                            303           1    0.0    0.0     0.0    0.0
    hammingDistance                         Util                               306           8    0.0    0.0     0.0    0.0
     hammingDistance.l2                     Util                               312           8    0.0    0.0     0.0    0.0
     hammingDistance.(...)                  Util                               307           0    0.0    0.0     0.0    0.0
    hammingDistance.l1                      Util                               305           8    0.0    0.0     0.0    0.0
    hammingDistance.(...)                   Util                               304           8    0.0    0.0     0.0    0.0
   prettyList                               Util                               302           1    0.0    0.0     0.0    0.0
   doHamming.msg                            ExpUtil                            299           1    0.0    0.0     0.0    0.0
  main.pats                                 Main                               295           1    0.0    0.0     0.0    0.0
  main.p                                    Main                               294           1    0.0    0.0     0.0    0.0
  doErrorProb                               ExpUtil                            279           1    0.0    0.0     0.0    0.0
   doErrorProb.expErrs                      ExpUtil                            293           1    0.0    0.0     0.0    0.0
    computeErrorSuperAttractorNumbers       Analysis                           296           6    0.0    0.0     0.0    0.0
     ./.                                    Util                               297           6    0.0    0.0     0.0    0.0
   packL                                    Common                             286           2    0.0    0.0     0.0    0.0
    pack                                    Common                             292           2    0.0    0.0     0.0    0.0
   attachLabels                             Util                               281           1    0.0    0.0     0.0    0.0
    attachLabels.list                       Util                               284           1    0.0    0.0     0.0    0.0
     attachLabel                            Util                               285           2    0.0    0.0     0.0    0.0
      showsPrec                             Common                             291          12    0.0    0.0     0.0    0.0
  sampleHammingRange                        Measurement                        273           1    0.0    0.0     0.0    0.0
   sampleHammingDistance                    Measurement                        275           8    0.0    0.0     0.0    0.0
    sampleHammingDistance.n                 Measurement                        330           8    0.0    0.0     0.0    0.0
    sampleHammingDistance.basePerm          Measurement                        316           8    0.0    0.0     0.0    0.0
     toArray                                Util                               328           8    0.0    0.0     0.0    0.0
      toArray.l                             Util                               329           8    0.0    0.0     0.0    0.0
    sampleHammingDistance.multByPat         Measurement                        313           8    0.0    0.0     0.0    0.0
    sampleHammingDistance.coeffSamples      Measurement                        276           8    0.0    0.0     0.0    0.0
     shuffle                                Util                               277           8    0.0    0.0     0.0    0.0
      shuffle.len                           Util                               315           8    0.0    0.0     0.0    0.0
      shuffle.shuffledArray                 Util                               314           8    0.0    0.0     0.0    0.0
       shuffle.shuffledArray.\              Util                               331         800    0.0    0.0     0.0    0.0
   runT                                     Util                               274           1    0.0    0.0     0.0    0.0
  genIO                                     Main                               271           0    0.0    0.0     0.0    0.0
 CAF                                        Main                               267           0    0.0    0.0     0.0    0.0
  main                                      Main                               268           1    0.0    0.0     0.0    0.0
   experimentUsingT1NoAvg                   Clusters                           375           1    0.0    0.0     0.0    0.0
    basinsGivenProbabilityT1                Clusters                           379           6    0.0    0.0     0.0    0.0
     randomSignVector                       Util                               381           6    0.0    0.0     0.0    0.0
      randomBinaryVector                    Util                               383           6    0.0    0.0     0.0    0.0
   main.patCombiner                         Main                               343           1    0.0    0.0     0.0    0.0
   main.maxHamming                          Main                               324           1    0.0    0.0     0.0    0.0
    .*                                      Util                               325           1    0.0    0.0     0.0    0.0
   main.minHamming                          Main                               322           1    0.0    0.0     0.0    0.0
    .*                                      Util                               323           1    0.0    0.0     0.0    0.0
   main.dist                                Main                               318           0    0.0    0.0     0.0    0.0
    hammingDistribution                     Measurement                        319           1    0.0    0.0     0.0    0.0
     hammingDistribution.probs              Measurement                        327           1    0.0    0.0     0.0    0.0
     hammingDistribution.dist               Measurement                        326           1    0.0    0.0     0.0    0.0
     hammingDistribution.rs                 Measurement                        321           1    0.0    0.0     0.0    0.0
   patternGen                               Utils                              308           1    0.0    0.0     0.0    0.0
    toGenVector                             Utils                              309           1    0.0    0.0     0.0    0.0
   main.maxDegree                           Main                               290           1    0.0    0.0     0.0    0.0
   main.degrees                             Main                               287           1    0.0    0.0     0.0    0.0
    powersOfTwo                             SuperAttractors                    288           1    0.0    0.0     0.0    0.0
   main.n                                   Main                               278           1    0.0    0.0     0.0    0.0
   main.numRandoms                          Main                               272           1    0.0    0.0     0.0    0.0
   genIO                                    Main                               270           1    0.0    0.0     0.0    0.0
 CAF                                        ExpUtil                            265           0    0.0    0.0     0.0    0.0
  doCheckFixed                              ExpUtil                            372           0    0.0    0.0     0.0    0.0
   doCheckFixed.msg                         ExpUtil                            373           0    0.0    0.0     0.0    0.0
  doHamming                                 ExpUtil                            300           0    0.0    0.0     0.0    0.0
   doHamming.msg                            ExpUtil                            301           0    0.0    0.0     0.0    0.0
  doErrorProb.errorHeader                   ExpUtil                            282           1    0.0    0.0     0.0    0.0
  doErrorProb                               ExpUtil                            280           0    0.0    0.0     0.0    0.0
   doErrorProb.errorHeader                  ExpUtil                            283           0    0.0    0.0     0.0    0.0
 CAF                                        Hopfield                           264           0    0.0    0.0     0.0    0.0
  getUpdatables_                            Hopfield                           440           0    0.0    0.0     0.0    0.0
 CAF                                        SuperAttractors                    262           0    0.0    0.0     0.0    0.0
  powersOfTwo                               SuperAttractors                    289           0    0.0    0.0     0.0    0.0
 CAF                                        Utils                              260           0    0.0    0.0     0.0    0.0
  signGen                                   Utils                              332           1    0.0    0.0     0.0    0.0
 CAF                                        Clusters                           259           0    0.0    0.0     0.0    0.0
  experimentUsingT1NoAvg                    Clusters                           377           0    0.0    0.0     0.0    0.0
   experimentUsingT1NoAvg.probabilities     Clusters                           378           1    0.0    0.0     0.0    0.0
 CAF                                        Math.Combinatorics.Exact.Primes    212           0    0.0    0.0     0.0    0.0
 CAF                                        System.Random                      205           0    0.0    0.0     0.0    0.0
 CAF                                        GHC.IO.Handle.FD                   184           0    0.0    0.0     0.0    0.0
 CAF                                        GHC.Conc.Signal                    181           0    0.0    0.0     0.0    0.0
 CAF                                        GHC.Float                          177           0    0.0    0.0     0.0    0.0
 CAF                                        GHC.Float.ConversionUtils          176           0    0.0    0.0     0.0    0.0
 CAF                                        GHC.IO.Encoding                    175           0    0.0    0.0     0.0    0.0
 CAF                                        GHC.IO.Encoding.Iconv              174           0    0.0    0.0     0.0    0.0
 CAF                                        GHC.Integer.Logarithms.Internals   142           0    0.0    0.0     0.0    0.0
