{-# LANGUAGE FlexibleInstances #-}

module Testing where
    import Data.Maybe
    import DigitRecognition
    import System.CPUTime
    import Framework
    import Control.Monad
    import System.Console.ANSI
    import Control.Exception
    import Data.List
    import Data.Dynamic
    
    class Testable a where
        makeLens :: a -> [(Digit, Int)]
        getInfo :: Digit -> a -> Dynamic

    
    instance Testable ([(Digit, Int)], [(Digit, [[(Int, Int)]])]) where
        makeLens (dc, ds) = dc
        getInfo digit (ds, dc) = toDyn $ lookupVal digit dc
    
    instance Testable [(Digit, [PixelImage])] where
        makeLens corpus = [(d, length i) | (d,i) <- corpus]
        getInfo digit corpus = toDyn $ lookupVal digit corpus

   
    -- Time an IO action
    time :: IO t -> IO Double
    time a = do
        start <- getCPUTime
        v <- a
        end   <- getCPUTime
        let diff = (fromIntegral (end - start)) / (10^12)
        return (diff :: Double)

    assertTrue :: Bool -> IO Bool
    assertTrue b =  do
        putStr "\t* "
        ret <- (try $ evaluate b :: IO (Either SomeException Bool))
        case ret of
            Left ex -> do
                putRed $ "Failed with exception: " ++ (head $ lines $ show ex)
                return False
            Right b -> do
                when b $ putGreen  "Passed!" 
                when (not b) $ putRed "Failed, but didn't crash!"
                return b


    assertEqual :: Eq a => a -> a -> (a -> String) -> IO Bool
    assertEqual a c s =  do
        putStr "\t* "
        ret <- (try $ evaluate (a == c) :: IO (Either SomeException Bool))
        case ret of
            Left ex -> do
                putRed $ "Failed with exception: " ++ (head $ lines $ show ex)
                return False
            Right True -> do
                putGreen $ "Passed!" 
                return True
            Right False -> do
                putRed "Failed, but didn't crash!"
                putRed $ "Should have been:" ++ (s c)
                putRed $ "But got:" ++ (s a)
                return False

    assertError :: a -> String -> IO Bool
    assertError exp errMsg = do
        putStr "\t* "
        ret <- (try $ evaluate exp )
        case ret of
            Left ex -> do
                putGreen $ "Possibly passed, error message: " ++ (head $ lines $ show (ex :: SomeException))
                return True
            Right b -> do
                putRed $ "Failed, reason: " ++ errMsg
                return False

    putRed :: String -> IO ()
    putRed str = do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn str
        setSGR [Reset]

    putGreen :: String -> IO ()
    putGreen str = do
        setSGR [SetColor Foreground Vivid Green]
        putStrLn str
        setSGR [Reset]

    testAllFeatures :: IO Bool
    testAllFeatures = do
        putStrLn "Testing allFeatures:"
        fmap and $ sequence
            [assertTrue (length allFeatures == 784),
            assertTrue (0 `elem` allFeatures),
            assertTrue (700 `elem` allFeatures)]

    testAllDigits :: IO Bool
    testAllDigits = do
        putStrLn "Testing allDigits:"
        assertTrue (sort allDigits == (map read $ map (\x -> [x]) "0123456789"))
    
    imageOutput = unlines ["                            ",
                           "                            ",
                           "                            ",
                           "                            ",
                           "                            ",
                           "                            ",
                           "                            ",
                           "      ######                ",
                           "      ################      ",
                           "      ################      ",
                           "            # ########      ",
                           "                  ###       ",
                           "                 ####       ",
                           "                 ####       ",
                           "                ####        ",
                           "                ###         ",
                           "                ###         ",
                           "               ###          ",
                           "              ####          ",
                           "              ###           ",
                           "             ####           ",
                           "            ####            ",
                           "           ####             ",
                           "           ####             ",
                           "          #####             ",
                           "          #####             ",
                           "          ###               ",
                           "                            "]

    testShowPixelImage :: PixelImage -> IO Bool
    testShowPixelImage img = do
        putStrLn "testing showPixelImage"
        assertEqual (showPixelImage img) imageOutput (++"\n")


    testLookupVal :: IO Bool
    testLookupVal = do
        putStrLn "Testing lookupVal:"
        b <- assertTrue (lookupVal 10 (zip [1..10] ['a'..'z']) == 'j')
        fmap and $ sequence
            [(putStrLn "Testing error cases. For full credit, should not be a Prelude exception.") >> (return True),
            assertError (lookupVal 1 [(1, "lookup"), (1, "conflicts"), (1, "are"), (1, "fun")]) 
                        "lookupVal should throw an error when there is more than 1 result for a lookup!",
            assertError (lookupVal 1 []) 
                        "lookupVal should throw an error when the key is not in the list!"]
        return b
    

    testBuildCorpus :: [(PixelImage, Digit)] -> IO Bool
    testBuildCorpus trainingImages = do
        putStrLn "Testing buildCorpus"
        let corpus = buildCorpus trainingImages
        let lens = makeLens corpus
        let lc v = lookupVal v lens
        let smallCorpus = buildCorpus (take 10 trainingImages)
        let slens = makeLens smallCorpus
        let ls v = lookupVal v slens
        b1 <- fmap and $ sequence [
            assertTrue (and [lc 5 == 434, lc 0 == 479, lc 4 == 535,
                             lc 1 == 563, lc 9 == 495, lc 2 == 488,
                             lc 3 == 493, lc 6 == 501, lc 7 == 550,
                             lc 8 == 462]),
            assertTrue $ length lens == 10]
        putStrLn "Testing buildCorpus ignores digits with no images."
        putStrLn "This is required for full credit, but your code may work without it."
        b2 <- fmap and $ sequence [
            assertTrue (and [ls 5 == 1, ls 0 == 1, ls 4 == 2, ls 1 == 3,
                             ls 9 == 1, ls 2 == 1, ls 3 == 1]),
            assertTrue $ length slens == 7
            ]
        return (b1)

    testProbOfDigit :: [(PixelImage, Digit)] -> IO Bool
    testProbOfDigit trainingImages = do
        putStrLn "Testing probOfDigit"
        let corpus = buildCorpus trainingImages
        fmap and $ sequence
            [assertTrue $ (probOfDigit corpus 0) == (479 `outOf` 5000) && (probOfDigit corpus 1) == (563 `outOf` 5000)
                        && (probOfDigit corpus 2) == (61  `outOf`  625) && (probOfDigit corpus 3) == (493 `outOf` 5000),
             assertTrue $ (probOfDigit corpus 4) == (107 `outOf` 1000) && (probOfDigit corpus 5) == (217 `outOf` 2500)
                        && (probOfDigit corpus 6) == (501 `outOf` 5000) && (probOfDigit corpus 7) == (11  `outOf`  100)
                        && (probOfDigit corpus 8) == (231 `outOf` 2500) && (probOfDigit corpus 9) == (99  `outOf` 1000)]

    mbc xs d n = 
        let y = nub $ map snd $ xs
            i = take n $ map fst $ filter (\k -> snd k == d) xs
        in buildCorpus $ map (\x -> (x,0)) i



    fd = (fromJust . fromDynamic)
    testSmoothing :: [(PixelImage, Digit)] -> IO Bool
    testSmoothing trainingImages = do
        putStrLn "Testing smoothing. This is required for full credit, but your code may work without it."
        putStrLn "Testing smoothing for probOfFeature." 
        let zeroInfo = fd $ getInfo 0 $ mbc trainingImages 0 10 
        someSmooth <- assertTrue  $ (probOfFeature zeroInfo 0) > 0
        when someSmooth $ judgeSmoothing 10 $ probOfFeature zeroInfo 0
        putStrLn "Testing smoothing for probOfNoFeature."
        let smallerInfo = fd $ getInfo 0 $ mbc trainingImages 0 6
        someSmoothN <- assertTrue  $ (probOfNoFeature smallerInfo 300) > 0
        when someSmoothN $ judgeSmoothing 6 (probOfNoFeature smallerInfo 300)
        return True 
     where judgeSmoothing k prob =
                if (prob <= (1 `outOf` (k+2))) then putGreen "\tYou are using great smoothing!"
                else if (prob <= (1 `outOf` (k+1))) then putGreen "\tYou are using decent smoothing!"
                else putRed "\tYour smoothing is bad!"

    testProbOfFeature :: [(PixelImage, Digit)] -> IO Bool
    testProbOfFeature trainingImages = do
        putStrLn "Testing probOfFeature"
        let zeroImages = fd $ getInfo 0 $ mbc trainingImages 0 10 
            approx x y = abs (x-y) < 0.1
        fmap and $ sequence
            [assertTrue $ probOfFeature zeroImages 10 `approx` 0,
             assertTrue $ probOfFeature zeroImages 300 `approx` (5 `outOf` 6)
            ]

    testProbOfNoFeature :: [(PixelImage, Digit)] -> IO Bool
    testProbOfNoFeature trainingImages = do
        putStrLn "Testing probOfNoFeature"
        let zeroImages = fd $ getInfo 0 $ mbc trainingImages 0 10 
            approx x y = abs (x-y) < 0.1
        fmap and $ sequence
            [assertTrue $ probOfNoFeature zeroImages 10 `approx` 1,
             assertTrue $ probOfNoFeature zeroImages 300 `approx` (1 `outOf` 10)
            ]

    testRankOfDigit :: [(PixelImage, Digit)] -> [PixelImage] -> [Digit] -> IO Bool 
    testRankOfDigit trainingImages validImages validLabels = do
        putStrLn "Testing rankOfDigit"
        let corpus = buildCorpus $ take 1000 trainingImages
            img1 = validImages!!10
            img2 = validImages!!20
        fmap and $ sequence [
            assertTrue $ ((rankOfDigit corpus 0 img1) > (rankOfDigit corpus 4 img1)),
            assertTrue $ ((rankOfDigit corpus 9 img2) > (rankOfDigit corpus 1 img2))]

    testClassifyImage :: [(PixelImage, Digit)] -> [PixelImage] -> [Digit] -> IO Bool 
    testClassifyImage trainingImages validImages validLabels = do
        putStrLn "Testing classifyImage"
        let corpus = buildCorpus $ take 500 trainingImages
        fmap and $ sequence [
            assertTrue $ ((classifyImage corpus (validImages!!2)) == 1),
            assertTrue $ ((classifyImage corpus (validImages!!8)) /= 5)]

    testRankImage :: [(PixelImage, Digit)] -> [PixelImage] -> [Digit] -> IO Bool 
    testRankImage trainingImages validImages validLabels =  do
        putStrLn "Testing rankImage (optional)"
        let corpus = buildCorpus $ take 500 trainingImages
        assertTrue $ (length $ rankImage corpus (validImages!!2)) == 10
        putStrLn "Testing if rankImage is sorted (even more optional)"
        assertTrue $ let ranks = map snd $ rankImage corpus (validImages!!2) 
                     in reverse ranks == sort ranks
        return True
    
    runTests ::[(PixelImage, Digit)] -> [PixelImage] -> [Digit] -> IO ()
    runTests trainingImages validImages validLabels =  do
        milestone <- fmap and $ sequence $ 
            [testAllFeatures,
             testAllDigits,
             testLookupVal,
             testBuildCorpus trainingImages,
             testShowPixelImage (validImages!!0)]
        putStrLn $ if milestone then "You passed critical tests for the milestone." else "Milestone not completed."
        when (milestone) $ do  
            project <- fmap and $ sequence $ 
                [testProbOfDigit trainingImages, 
                 testProbOfFeature trainingImages,
                 testProbOfNoFeature trainingImages,
                 testSmoothing trainingImages,
                 testRankOfDigit trainingImages validImages validLabels,
                 testClassifyImage trainingImages validImages validLabels,
                 testRankImage trainingImages validImages validLabels]
            putStrLn $ if project then "You pass all critical tests. Try running the program for real!" else "Project not completed."
        
