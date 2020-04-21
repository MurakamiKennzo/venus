-- Convert a non-negative integer to its english words representation. Given input is guaranteed to be less than 231 - 1.

module NumberToWords
  (
    numberToWords
  ) where

numberToWords :: Int -> String
numberToWords = numberToWords' 0

numberToWords' :: Int -> Int -> String
numberToWords' i n
  | a == 0 = c
  | otherwise = numberToWords' (succ i) a <> " " <> c
  where (a, b) = n `divMod` 1000 
        c = readThree b <> if i == 0 then "" else " " <> (show $ toEnum'' i)

readThree :: Int -> String
readThree n
  | n <= 20 = show $ toEnum' n
  | n == 30 = show $ toEnum' 21
  | n == 40 = show $ toEnum' 22
  | n == 50 = show $ toEnum' 23
  | n == 60 = show $ toEnum' 24
  | n == 70 = show $ toEnum' 25
  | n == 80 = show $ toEnum' 26
  | n == 90 = show $ toEnum' 27
  | n >= 100 = let (a, b) = n `divMod` 100 in readThree a <> " " <> (show $ toEnum'' 0) <> if b == 0 then "" else " " <> readThree b
  | otherwise = let (a, b) = n `divMod` 10 in readThree (a * 10) <> " " <> readThree b

toEnum' :: Int -> EnglishNumber
toEnum' = toEnum

toEnum'' :: Int -> EnglishUnit
toEnum'' = toEnum

data EnglishUnit = Hundred
                 | Thousand
                 | Million
                 | Billion deriving (Show, Enum)

data EnglishNumber = Zero
                   | One
                   | Two
                   | Three
                   | Four
                   | Five
                   | Six
                   | Seven
                   | Eight
                   | Nine
                   | Ten
                   | Eleven
                   | Twelve
                   | Thirteen
                   | Fourteen
                   | Fifteen
                   | Sixteen
                   | Seventeen
                   | Eighteen
                   | Nineteen
                   | Twenty
                   | Thirty
                   | Forty
                   | Fifty
                   | Sixty
                   | Seventy
                   | Eighty
                   | Ninety deriving (Show, Enum)
