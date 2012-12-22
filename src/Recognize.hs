

Method = Hopfield | Boltzmann

buildHopfieldData :: [Pattern] -> HopfieldData


loadPicture :: String -> Int -> Int-> IO CBinaryPattern

loadPictureStd s = loadPicture 200 200

recPic :: Method -> [FilePath] -> FilePath -> IO (Maybe FilePath)
recPic method imgPaths queryImgPath = do
  l@(queryImg:imgs) <- mapM loadPictureStd (queryImgPath:imgPaths)
  gen <- getStdGen
  let queryPat:imgPats = map toPattern l
      runRand r =  evalRand r gen
  return $ case method of
    Holpfield -> case runRand $ matchPattern (buildHopfieldData imgPats) queryPath of
                   Left pattern -> Nothing -- TODO apply heuristic if we want
                   Right i -> Just $ imgPaths !! i
    Boltzmann -> error "Boltzmann not implemented"
