module Framework where
    import Data.List.Split (chunksOf)
    import Data.Ratio ((%))
    import Debug.Trace
    import Data.List (sort)

    --                                          Type Aliases
    -- These type aliases help to abstract our code. You will use them extensively in DigitRecognition.hs
    --
    type PixelImage = [[Bool]]
    -- A pixel image is a two-dimensional list of booleans.
    -- False represents an empty pixel, and True a grey or black pixel. Each pixel image will be 28x28.
    
    --my own custom type: PixelImageRow
    type PixelImageRow = [Bool]
    
    type Feature = Int
    -- Each pixel location is considered a separate feature for classification. Because the image is
    -- 28x28, there are 784 total features.
    type Digit = Integer
    -- Each image is of a specific digit, 0 through 9. To distinguish the labels or guesses from
    -- other numbers, we use a type alias.


    --                                      Primitive Functions
    -- These functions will be used in your implementation of the classifier in DigitRecognition. Be
    -- sure you understand how they are used.
    --
    hasFeature :: PixelImage -> Feature -> Bool
    --hasFeature checks if an image has a specific feature: i.e. if that pixel is white or blank.
    --
    --This encapsulates the ugliness of storing images as nested lists. Notice the normally
    --forbidden use of !!. This suggests that there should be a better way to handle and process
    --images. For the purposes of this project we will accept this.  We can take reassurance that
    --lists of length 28 are so short that better storage methods are probably unnecessary.
    hasFeature img ftr = 
        let dim = length img
            row = img !! (ftr `div` dim)
            pixel = row !! (ftr `mod` dim)
        in pixel
    -- Example:    img `hasFeature` ftr

    outOf :: Int -> Int -> Rational
    --outOf wraps around Haskell's built-in Rational data type. Rationals store fractional values
    --precisely, with no possibility of underflow. Internally, the numerator and denominator are
    --kept as Integers, which have no maximum outside the space limitations of computer memory. You
    --will use this function to return an estimated probability. 
    outOf a b =  (fromIntegral a) % (fromIntegral b)
    --Example:      2 `outOf` 10
    --              (length [1..3]) `outOf` (length [1..10])
    --

    --                                  Image Reading Functions
    -- These functions are used to turn strings into PixelImages and Digits. You will not need to
    -- call these functions, but understanding how they work may help with showPixelImage in
    -- DigitRecognition.hs.
    --
    isGoodImage :: [String] -> Bool
    -- isGoodImage ensures that the input list is 28 lines long, and that
    -- each line is 28 characters that are '#', '+', or ' '. 
    -- The (and) function takes a list of Booleans, returning True when all of them are True.
    isGoodImage strs = length(strs) == 28 
                       && (and [isGoodLine str | str <- strs])
        where isGoodLine str = length str == 28 && and [c `elem` "#+ " | c <- str]

    readPixelImage :: [String] -> PixelImage
    --readPixelImage turns a 28x28 list of characters into a 28x28 list of booleans.
    --Both '+' and '#' are taken to be True, while ' ' becomes False. Thus, we lose some
    --information.
    readPixelImage strs = if isGoodImage strs 
                          then [readLine str | str <- strs] 
                          else error "Improper pixel image!"
        where readPixel x = not (x == ' ')
              readLine str = [readPixel x | x <- str]

    readPixelImages :: String -> [PixelImage]
    --readPixelImages takes the contents of a file storing images and turns it into a list of
    --PixelImages. The (lines) function breaks the string into a list of string, separating at '\n'
    --newlines. The (chunksOf) function, imported from Data.List.Split, breaks the list of lines
    --into 28-line chunks. Thus, we end up with a list of list of strings.
    readPixelImages imgStr =  
        let imageChunks = chunksOf 28 (lines imgStr)
        in [readPixelImage strs | strs <- imageChunks]

    readLabels :: String -> [Digit]
    --readLabels takes the contents of a file storing labels and turns it into a list of digits.
    --Since each label is on one line, reading a label is simply a matter of ensuring it is
    --a string "0" through "9", and calling the built-in (read) function.
    readLabels lblStr = [readLabel str | str <- lines lblStr]
        where readLabel str = if str `elem` validDigits
                              then read str
                              else error $ "Invalid label!" ++ str
              validDigits = [show x | x <- [0..9]]

    readLabeledImages :: String -> String -> [(PixelImage, Digit)]
    --readLabeledImages takes the contents of two files, one holding images and the other holding
    --the corresponding labels, and pairs them up. Recall that (zip) takes two lists and returns the
    --list of tuples.
    readLabeledImages imgStr lblStr =
        let images = readPixelImages imgStr
            labels = readLabels lblStr
        in if length images == length labels
           then zip images labels
           else error "Number of labels and images are different!"


    --Black magic functions
    -- sortCorpus is used to keeps everything nice and sorted, so equality can be checked. 
    -- You should never need to use this function.
    sortCorpus corpus = sort [(label, sort images) | (label, images) <- corpus]
