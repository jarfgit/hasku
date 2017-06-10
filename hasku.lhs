> import Data.Char
> import Data.List
> import System.Random


> vowels     = ['a','e','i','o','u','é']
> semiVowels = ['y']
> vocab  	 = ["Beyoncé","abracadabra","alligators","blue","bright","burning","call","candle",
>               "carpe diem","cat","chrysalis","city","code","communicate","compile","computer", "clone",
>               "danger","data","diamond","doom","electric","eleven","energy","epic","eternity",
>               "eye","flower","forest","forever","garden","ghost","goth","grow","hymnal","illuminati",
>               "infinite","inspire","jungle","kernel","laser","love","memory","mirth","mystery",
>               "mythic","new","no","nonsense","order","personality","photosynthesis","poem","poetry",
>               "program","pyramid","rage","raven","red","rhyme","robot","rose","safe","screen",
>               "secret","serendipity","shh","shoe","silly","skulls","sky","spider","stars","street",
>               "sun","sunset","sushi","synchronicity","tablet","tall","the","toe","transcend",
>               "triangle","tyger","type","unicorn","urchin","us","vanilla","violets","volcano","wary",
>               "waste","we","whale","why","wild","wind","wiry","with","yesterday"]


-- > vocab  	 = ["a","aa","aaa","aaaa","aaaaa"] 



** WORDS WITH NO VOWELS **
These are almost non-existent but easy to check for (so why not)


> hasVowels          :: String -> Bool
> hasVowels           = any (\x -> elem x (vowels ++ semiVowels))



** DEALING WITH 'Y' **
A vowel + 'y' creates a vowel sound, so these cases can be dealt with as a diphthong.
When a word begins with the letter 'y' whether it is a vowel depends on the following letter.

When is 'y' a vowel?

TRUE:
'y' is the first letter && 'y' : consonant (ex. "ygritte")
'y' is the last letter && consonant : y (ex. "sky")
'y' is in the middle && consonant : 'y' : consonant (ex. "hymn", "myrth")

FALSE:
'y' is the first letter && 'y' : vowel (ex. "yellow" )
'y' is the last letter && vowel : y (ex. "spacey")
'y' is in the middle &&	vowel : 'y' : vowel (ex. "beyond")
'y' is in the middle && vowel : 'y' : consonant (ex. "layman", "haystack")

SPECIAL CASES: 
'y' is in the middle && consonant : 'y' : vowel 
T - (ex. "myopic", "cryogenesis") exception might be if vowel is an 'o'?
T - (ex. "crying") might be solved by dropping prefixes and suffixes prior to parsing vowels? 


Takes a string and returns a list of boolean values: True if char is a vowel False if not

> mapVs		    	 :: String -> [Bool]
> mapVs []	  	 	  = [] 
> mapVs (x:xs) 		  = elem x vowels : mapVs xs 


Modifies a list of boolean values for all vowels in a string accounting for special cases of the letter 'y'

> mapSemiVs          :: [Int] -> [Bool] -> [Bool]
> mapSemiVs [] bs     = bs
> mapSemiVs ns []     = []
> mapSemiVs (n:ns) bs  		 
>					  | n == 0 && not next = mapSemiVs ns (replaceAt bs True n) 
>				  	  | n == endIndex bs && not prev = mapSemiVs ns (replaceAt bs True n) 
>					  | 0 < n && n < endIndex bs && not prev && not next = mapSemiVs ns (replaceAt bs True n)  
>					  | otherwise = mapSemiVs ns bs
>					  where prev = (bs !! (n-1))
>			 		    	next = (bs !! (n+1)) 


** WHAT TO DO WITH 'Q' AND 'U' **

> mapQandU			:: [Int] -> [Bool] -> [Bool]
> mapQandU [] bs     = bs 
> mapQandU ns []     = []
> mapQandU (n:ns) bs = replaceAt bs False (n + 1)


Combines functions above to return a tuple that countains the original string and a list of booleans that correspond to each character's vowel status

> mapAllVowels		 :: String -> (String, [Bool])
> mapAllVowels xs 	  = (xs, intersect (mapQandU (findIndices(=='q') xs) (mapVs xs)) (mapSemiVs (findIndices (=='y') xs) (mapVs xs)))


** CONSONANT + "LE" AS FINAL SYLLABLE **

> endsWithE          :: String -> Bool
> endsWithE []        = False
> endsWithE xs        = last xs == 'e' 


> isESilent          :: String -> Bool
> isESilent xs 	   
> 				      | not (endsWithE xs) = False
>				      | vowelCount == 1 = False -- If there is only one vowel (e.g. "the" = False, "toe" = True)  
>                     | tail lastThree == "le" && notElem lastThree (map (:"le") vowels) = False 
>                     | otherwise = True
>				      where lastThree = drop (length xs - 3) xs
>				            vowelCount = length $ filter (== True) (snd $ mapAllVowels xs)


> mapSilentE	     :: (String, [Bool]) -> (String, [Bool])
> mapSilentE (x, y)
>					  | endsWithE x && isESilent x = (x, replaceAt y False (endIndex y))
>					  | otherwise = (x, y)



** COMBINE ALL PARSING FUNCTIONS **

Calls all syllable parsing functions on a string and returns an Int representing the number of True in list of Bools
	
	* Confirm word has vowels, if not return 1
	* Map all vowels and semivowels
	* Account for silent 'e'

	This order works for eye, tye 

> count	             :: String -> Int
> count []  		  = 0
> count xs
>			          | not (hasVowels xs) = 1
>			          | otherwise = length $ filter (== True) (snd $ mapSilentE $ mapAllVowels xs)



** COMPOSE HAIKU **
Takes a syllable count, and empty list and returns a random list of words where the total syllable count matches the argument
                     

> randomW            :: IO String 
> randomW             = do i <- randomRIO (0, length vocab - 1)
>                          return (vocab !! i) 


> compose            :: Int -> [String] -> IO [String]
> compose 0 xs        = return xs
> compose n xs        = do word <- randomW
>                          let syllables = count word 
>                          if n >= syllables then compose (n - syllables) (word:xs)
>                             else compose n xs


> hasku 		     :: IO ()
> hasku 		      = do 
>                         x <- compose 5 []
>                         y <- compose 7 []
>                         z <- compose 5 []
>                         putStrLn $ unlines $ [unwords x] ++ [unwords y] ++ [unwords z]



** UTILITY FUNCTIONS **

Replaces a list item with a different value at a given index 

> replaceAt	         :: [a] -> a -> Int -> [a]
> replaceAt [] _ _    = []
> replaceAt x y n
>					  | n == 0 = [y] ++ tail x 
>					  | n == endIndex x =  init x ++ [y]
>					  | otherwise = take (n) x ++ [y] ++ drop (n + 1) x


Returns the last index in a list 

> endIndex	 	     :: [a] -> Int
> endIndex [] 		  = 0
> endIndex x  		  = length x - 1 















// OLD STUFF 

-- Step 1: is 'e' the final char?  

-- > endsWithE        :: String -> Bool
-- > endsWithE []      = False
-- > endsWithE xs      = if last xs == 'e' then True else False

-- Step 2: does it end with "le" preceeded by a consonant?

-- > isConBeforeLE     :: [Char] -> Bool
-- > isConBeforeLE []   = False
-- > isConBeforeLE xs   = if elem (head (drop (length xs - 3) xs)) vowels then False else True  

-- -- Step 3: is "e" silent?

-- > isESilent         :: [Char] -> Bool
-- > isESilent []       = False
-- > isESilent xs       = if endsWithE xs && isConBeforeLE xs then False else True 

//TRUE + TRUE = FALSE

STREAMLINED:


-- > isESilent       :: [Char] -> Bool
-- > isESilent xs 	   
-- > 				   | length (filter (==True) (mapVowels xs)) == 1 = False -- If there is only one 'e' (e.g. "the" = False, "toe" = True) 
-- >				   | length xs <= 3 && length (filter (==True) (mapVowels xs)) > 1 = True 
-- >				   | otherwise = elem (reverse (take 3 (reverse xs))) (map (:"le") vowels)

-- > isESilent       :: [Char] -> Bool
-- > isESilent xs 	   
-- > 				   | length (filter (==True) (mapVowels xs)) == 1 = False -- If there is only one 'e' (e.g. "the" = False, "toe" = True) 
-- > 				   | length (filter (==True) (mapVowels xs)) > 1 = True
-- >				   | otherwise = elem lastThree (map (:"le") vowels)
-- >				   where lastThree = reverse (take 3 (reverse xs))
-- >						  




Test:

*Main> testString = "The little table has a tale to tell"
*Main> map isESilent $ words testString
[True,False,False,True,False,True,True,True]

Would only modify the elements that evaluate to False. Ignore any that evaluate to True.

Step 5: if 'e' is not silent, drop the last three character to modify the string, increment syllable count 

-- > dropConBeforeLE           :: ([Char], [Char], Int) -> ([Char], [Char], Int)
-- > dropConBeforeLE ([], y, n) = ([], y, n) -- !!! MAY NEED TO REVISIT THIS AS AN ERROR?
-- > dropConBeforeLE (x, [], n) = (x, [], n)
-- > dropConBeforeLE (x, y, n)  = if not (isESilent x) 
-- >									then (x, reverse (tail (reverse y)), n + 1) -- (x, reverse (drop 3 (reverse y)), n + 1) will drop + consonant +le
-- > 									else (x, reverse (tail (reverse y)), n)






-- > dropSemiVowels	     		  	:: [Int] -> ([Char], [Char], Int) -> ([Char], [Char], Int)
-- > dropSemiVowels [] (x, y, n)     = (x, y, n)
-- > dropSemiVowels (z:zs) (x, y, n)
-- >								   | z == 0 && notElem (x !! 1) vowels = dropSemiVowels zs (x, tail y, n + 1 ) -- if first letter && next letter is not a vowel drop first letter
-- >								   | z == (length x - 1) && notElem (head (tail (reverse x))) vowels = dropSemiVowels zs (x, reverse(tail (reverse y)), n + 1 )
-- >								   | otherwise = dropSemiVowels zs (x, y, n)



-- > dropSemiVowels			           :: [Bool] -> ([Char], [Char], Int) -> ([Char], [Char], Int)
-- > dropSemiVowels (z:zs) (x, (y:ys), n)	
-- >							 		|  	 
-- >									then dropVowels vs (x, filter (/=v) y, n + length (filter (==v) y))
-- >									else dropVowels vs (x, y, n)

-- > dropSemiVowels	     			:: [Int] -> ([Char], [Char], Int) -> ([Char], [Char], Int)
-- > dropSemiVowels [] (x, y, n)      = (x, y, n)
-- > dropSemiVowels [z:zs] (x, y, n)  = if   
-- >									then dropVowels vs (x, filter (/=v) y, n + length (filter (==v) y))
-- >									else dropVowels vs (x, y, n)


-- > isYaVowel	     			:: [Char] -> Bool
-- > isYaVowel []   			 = False
-- > isYaVowel [x]  			 = if x == 'y' then True else False
-- > isYaVowel xs   			 
-- >							| head xs == 'y' && notElem (head (tail xs)) vowels = True
-- >							| last xs == 'y' && notElem (head (tail (reverse xs))) vowels = True
-- >							| otherwise = False



-- > dropFirstY            	:: ([Char], [Char], Int) -> ([Char], [Char], Int)
-- > dropFirstY (x, y, n) 		 = if isYaVowel y then (x, tail y, n + 1) else (x, y, n)



** REMAINING VOWELS AFTER ALL SPECIAL CASES **

For each vowel in the modified string, remove it, and increment syllable count

-- > dropVowels             	:: [Char] -> ([Char], [Char], Int) -> ([Char], [Char], Int)
-- > dropVowels [] (x, y, n)    = (x, y, n)
-- > dropVowels (v:vs) (x, y, n)  
-- >							| elem v y = dropVowels vs (x, filter (/=v) y, n + length (filter (==v) y))
-- >							| isYaVowel
-- >							| otherwise dropVowels vs (x, y, n)



First pass at removing vowels - does not account for 'y'

-- > dropVowels             	  :: [Char] -> ([Char], [Char], Int) -> ([Char], [Char], Int)
-- > dropVowels [] (x, y, n)      = (x, y, n)
-- > dropVowels (v:vs) (x, y, n)  = if elem v y 
-- >									then dropVowels vs (x, filter (/=v) y, n + length (filter (==v) y))
-- >									else dropVowels vs (x, y, n)

-- > countVowels	:: ([Char], [Bool]) -> ([Char], [Bool]) 
-- > countVowels ([], y) = 
-- > countVowels (x, y) =  



Call all syllable parsing functions on a string and return a triple with ("originalString", "modifiedString", syllableCount) in the following order:
	
	* Confirm word has vowels - if not return 1 for syllableCount
	* Parse all last syllables with consonant + 'le'
	* Parse all remaining vowels 


-- > countSyllables    :: [Char] -> ([Char], [Char], Int)
-- > countSyllables []  = ([], [], 0)
-- > countSyllables x   = if hasVowels x 
-- >									then dropVowels vowels (dropConBeforeLE (x, x, 0))
-- >									else (x, x, 1) -- words that have no vowels do have 1 syllable


Test: 

Test with empty list
*Main> countSyllables ""
("","",0)

Test with word with no vowels
*Main> countSyllables "crwth"
("crwth","crwth",1)

*Main> countSyllables "c"
("c","c",1)

Tests with list of all vowels:
*Main> dropVowels "adorable"
("adorable","drbl",4)


