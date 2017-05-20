--import Prelude hiding (Word)
import Data.Maybe
import Data.List
import Test.QuickCheck

type Text  = String
type Word  = String
type First = Int
type Last  = Int

occS :: Text -> Word -> [(First,Last)]
occS text word = getOcc skipS (length(word)-1) 0 text word

occI :: Text -> Word -> [(First,Last)]
occI text word = getOcc skipI (length(word)-1) 0 text word

{- added a parameter for skip width
    (occS: 1,
     occI: calculated via index of last occurrence in word (length(word) - 1 - index)
             (can this be done without recalculating the values every time? e.g. Memoization?)
-}
getOcc :: (Word -> Char -> Int) -> Int -> Int -> Text -> Word -> [(First,Last)]
getOcc _ _ _ "" _ = []
getOcc _ _ _ _ "" = []
getOcc skip tpos wcnt text word
 | tpos >= length(text)   = []                                                    -- no more occurrences possible
 | wcnt == length(word)-1 = if checkSame then [(tpos-wcnt,tpos)] ++ getOcc skip (tpos+length(word)) 0 text word
                              else getOcc skip (tpos+calcSkip) 0 text word        -- found a word, add start/end-index and carry on
 | otherwise              = if checkSame then getOcc skip tpos (wcnt+1) text word -- keep matching single chars until mismatch or word end
                              else getOcc skip (tpos+calcSkip) 0 text word
 where checkSame = (word!!(length(word)-1-wcnt) == text!!(tpos-wcnt))
       calcSkip  = (skip word (text!!(tpos-wcnt)))

-- simple skip width
skipS :: Word -> Char -> Int
skipS _ _ = 1

{- improved skip width
   - This function takes the last occurence of a char in the search pattern "word"
   and returns the index as a safe skip width.
   - Maybe can be "Nothing" or "Just "value"", fromMaybe defines a default case for "Nothing"
   - last letter of a word is a special case, to prevent (length(w) - 1 - i) == 0, a max is used
-}
skipI :: Word -> Char -> Int
skipI w c = max 1 (fromMaybe (length(w)) (elemIndex c (reverse w)))

-- ----- TEST property for QuickCheck -----
prop_coincide :: Text -> Word -> Bool
prop_coincide text word = occS text word == occI text word
