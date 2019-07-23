module DigitRecognition where
    import Framework
    import Data.List (nub, sort)
    import Data.Tuple (swap)
    import Data.Ratio (numerator, denominator)
    import Debug.Trace

    -- All undefined values and functions should be completed. Your code will compile and test 
    -- (with the -- test flag) even if some functions are left undefined.
    
    --                                       Milestone 1    

    -- Create a list of all possible features, starting at 0.
    allFeatures :: [Feature]
    allFeatures = [0..783]

    -- Create a list of all possible digit labels. 
    allDigits :: [Digit]
    allDigits = [0..9]
    
    -- showPixelImage should take a PixelImage and turn it into a single string.
    -- Since we have lost gray colors (see readPixelImage in Framework.hs), our
    -- string will have '#' for black pixels, and ' ' for white pixels.
    --
    -- I suggest a helper function that takes an individual row of a pixel image and turns it into a
    -- string. You can then use the built-in (unlines) function, which takes a list of strings and
    -- turns them into a single string, separated by newline.
    -- 
    -- Example: showPixelImage [[True, True], [True, False]]
    --          "##\n# \n"
    showPixelImage :: PixelImage -> String
    showPixelImage img = unlines [buildRow row | row <- img]

    buildRow :: PixelImageRow -> String
    buildRow row = [if(str) then '#' else ' ' | str <- row]

    -- lookupVal takes a key of type a, an association list from a to b, and returns the hopefully
    -- unique value associated with the key. If lst contains the tuple (k, v), then 
    -- lookupVal k lst should return v.
    --
    -- For full credit, ensure that the key matches exactly one tuple in the list.
    -- 
    -- Example: lookupVal 7 [(8, 'a'), (7, 'b'), (9,'c')]
    --          'b'
    lookupVal :: Eq a => a -> [(a, b)] -> b
    lookupVal key lst = if length vals /= 1
                        then error "The key does not match exactly one tuple in the list"
                        else head vals    
       where vals = [b | (k, b) <- lst, key == k]

    -- A corpus is an association list between digits and the images that are labeled with that
    -- digit. By storing the information this way, we avoid the frequent computation of which images
    -- are labeled with which digit. 
    type Corpus = [(Digit, [PixelImage])]
    
    --types to remove extra work
    type Summary = [[(Int, Int)]]
    type DigitSummary = [(Digit, Summary)]
    type DigitCount = [(Digit, Int)]
    --type Corpus = (DigitCount, DigitSummary)

    -- When we read in the files, we get a list of image-label tuples. It is far more efficient to
    -- group the images by their label as a Corpus. buildCorpus takes the list of tuples and
    -- separates it into sub-lists for each label. Order does not matter, either of the digits or of
    -- the images associated with each digit.
    --
    -- I suggest a helper function that takes a digit and returns the list of all images labeled with
    -- that digit.
    --
    -- For full credit, only create entries in the Corpus for digits that actually have associated
    -- images. You will need to use the (nub) function, which returns the set version of a list (i.e.
    -- all duplicate elements have been removed).
    -- 
    -- Example:  let 
    imgA = [[True, False], [False, False]]
    imgB = [[False, False], [False, False]]
    imgC = [[False, True], [False, False]]
    imglbls= [(imgA, 9), (imgB, 2), (imgC, 9)]
    --           corpus
    --           [(9, [ [[True, False]], [[False, True]] ]), (2, [[[False, False]]])]
    buildCorpus :: [(PixelImage, Digit)] -> Corpus
    --buildCorpus imglbls = undefined
    --buildCorpus imglbls = [(dgt, helpBuildDigit dgt imglbls) | (img, dgt) <- imglbls]
    buildCorpus imglbls = nub [(dgt, helpBuildDigit dgt imglbls) | (img, dgt) <- imglbls]
    --NOTE *buildCorpus is very slow becuase it does nub after creating a list, to fix this change
    --the order of when nub is applied.*
    --helpBuild takes Digit  and return list of PixelImage associated with that digit
    helpBuildDigit :: Digit -> [(PixelImage, Digit)] -> [PixelImage]
    helpBuildDigit digit imglbl = [img | (img, dgt) <- imglbl, dgt == digit]

    --                                  End of Milestone 1!
    --
    --                                  Final Project 
    
    -- Given a corpus and a specific digit Y, probOfDigit estimates P(Y). This is the fraction
    -- of the images in the corpus that are labeled with Y.  You will need to use `outOf` to create
    -- the fraction.
    -- You may find the (sum) function helpful: it takes a list of numbers and adds them together.
    -- Example: probOfDigit corpus 9
    --         2 % 3
    probOfDigit :: Corpus -> Digit -> Rational
    --probOfDigit corpus digit = undefined
    --'traceShowId $' 
    probOfDigit corpus digit =  numDig `outOf` numTot
        where numDig = length (lookupVal digit corpus)
              numTot = sum [length imgs | (dgt, imgs) <- corpus]
    
    -- Given the list of images (imgs) labeled for a given digit Y, and a feature F (ftr),
    -- probOfFeature imgs ftr estimates the probability P(ftr=Black | Y). See the assignment page for
    -- details.
    
    --
    probOfFeature :: [PixelImage] -> Feature -> Rational
    probOfFeature imgs ftr = (length [img | img <- imgs, img `hasFeature` ftr] + 1) `outOf` ((length imgs) + 2)

    -- Given the list of images (imgs) labeled for a given digit Y, and a feature F (ftr),
    -- probOfNOFeature imgs ftr estimates the probability P(ftr=White | Y). See the assignment page
    -- for details.
    probOfNoFeature :: [PixelImage] -> Feature -> Rational
    probOfNoFeature imgs ftr =  (nofeatures + 1) `outOf` ((length imgs) + 2)
        where nofeatures = length [img | img <- imgs, not ( img `hasFeature` ftr)]

    -- rankOfDigit should estimate the rank of a given digit for a given instance, as specified on
    -- the assignment page.
    -- You will need to use both probOfDigit, probOfFeature, and probOfNoFeature. 
    -- You may find the (product) function helpful.
    -- I recommend you calculate the values for positive features (those that occur in newImg)
    -- and negative features (those that do not occur in newImg) separately.
    rankOfDigit :: Corpus -> Digit -> PixelImage -> Rational
    -- probOfDigit * (probOfFeature and probOfNoFeature) 
    rankOfDigit corpus digit newImg = pDgt * pYes * pNo
        where pDgt = (probOfDigit corpus digit)
              pYes = (product [probOfNoFeature (lookupVal digit corpus) x | x <- allFeatures, not (hasFeature newImg x)])
              pNo = (product [probOfFeature (lookupVal digit corpus) x | x <- allFeatures, hasFeature newImg x])
    --((probOfDigit corpus digit) `product` (probOfFeature [newImg] digit)) `outOf` ((probOfDigit corpus digit) `product` (probOfNoFeature [newImg] digit))

    -- classifyImage should return the most likely digit, based on the rank computed by rankOfDigit.
    -- You will need to use the maximum function.
    -- An important fact: if you have a tuple of two values, maximum returns based on the first
    -- value.
    -- For full credit, make sure you check that the maximum rank is greater than 0. If it is not,
    -- print an error message.
    classifyImage :: Corpus -> PixelImage -> Digit
    classifyImage corpus newImg = digitClassification -- if (digitClassification > 0) then digitClassification else error "The maximum rank is not greater than 0"
        where digitClassification = snd (maximum [(rankOfDigit corpus dgt newImg, dgt) | dgt <- allDigits])

    --                                  Optional Helpful Functions
    -- These functions are optional, but may be helpful with debugging.

    -- valueOfRank takes a rank and turns it into a somewhat reasonable integer, suitable for
    -- printing. The ranks may be negative, that's perfectly fine.
    valueOfRank :: Rational -> Int
    valueOfRank r = 350 + ratLog r 
        where numDigits x = length $ show x
              ratLog r = (numDigits $ numerator r) - (numDigits $ denominator r)


    -- rankImage is similar to classify image, but instead of returning the best digit, it should
    -- return the list of digits and their ranks. Used by the --ranking flag.
    -- It is helpful, but not necessary, to sort the list.
    rankImage :: Corpus -> PixelImage -> [(Digit, Int)]
    rankImage corpus newImg = 
        undefined







