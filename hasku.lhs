> import Data.Char
> import Data.List
> import System.Random


> vowels     = ['a','e','i','o','u','é']
> semiVowels = ['y']
> vocab  	 = ["Beyoncé","abracadabra","alligators","blue","bright","burning","call","candle",
>               "carpe diem","cat","chrysalis","city","code","communicate","compile","computer", "clone",
>               "danger","data","diamond","doom","electric","eleven","energy","epic","eternity",
>               "eye","flower","forest","forever","garden","ghost","goth","grow","haystack","hymnal","illuminati",
>               "infinite","inspire","jungle","kernel","laser","love","memory","mirth","mystery",
>               "mythic","new","not","nonsense","order","personality","photosynthesis","poem","poetry",
>               "program","pyramid","rage","raven","red","rhyme","robot","rose","safe","screen",
>               "secret","serendipity","shh","shoe","silly","skulls","sky","spider","stars","street",
>               "sun","sunset","sushi","synchronicity","tablet","tall","the","toe","transcend",
>               "triangle","tyger","type","unicorn","urchin","us","vanilla","violets","volcano","wary",
>               "waste","we","whale","why","wild","wind","wiry","with","yesterday","quiet","quick", "burqa","sequel","sequin","harlequin","fork","git","system","quince"]


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

> parseSemiVs          :: [Int] -> [Bool] -> [Bool]
> parseSemiVs [] bs     = bs
> parseSemiVs ns []     = []
> parseSemiVs (n:ns) bs  		 
>					  | n == 0 && not next = parseSemiVs ns (replaceAt bs True n) 
>				  	  | n == endIndex bs && not prev = parseSemiVs ns (replaceAt bs True n) 
>					  | 0 < n && n < endIndex bs && not prev && not next = parseSemiVs ns (replaceAt bs True n)  
>					  | otherwise = parseSemiVs ns bs
>					  where prev = (bs !! (n - 1))
>			 		    	next = (bs !! (n + 1)) 


Combines functions above to return a tuple that countains the original string and a list of booleans that correspond to each character's vowel status

> mapVowels		 :: String -> (String, [Bool])
> mapVowels xs 	  = (xs, (parseSemiVs (findIndices (=='y') xs) (mapVs xs)))



** WHAT TO DO WITH 'Q' AND 'U' **

> parseQU			     :: [Int] -> (String, [Bool]) -> (String, [Bool])
> parseQU [] (xs, bs)     = (xs, bs)
> parseQU (n:ns) (xs, bs) 
>                         | next == 'u' = parseQU ns (xs, replaceAt bs False (n + 1))
>                         | otherwise = parseQU ns (xs, bs)
>						  where next = (xs !! (n + 1))

> mapQU                  :: (String, [Bool]) -> (String, [Bool])
> mapQU (xs, bs)          = parseQU (findIndices (=='q') xs) (xs, bs)



** CONSONANT + "LE" AS FINAL SYLLABLE **

> endsWithE          :: String -> Bool
> endsWithE []        = False
> endsWithE xs        = last xs == 'e' 


> isESilent          :: String -> Bool
> isESilent []        = False
> isESilent xs 
>				      | vowelCount == 1 = False -- If there is only one vowel (e.g. "the" = False, "toe" = True)  
>                     | tail lastThree == "le" && notElem lastThree (map (:"le") vowels) = False 
>                     | otherwise = True
>				      where lastThree = drop (length xs - 3) xs
>				            vowelCount = length $ filter (== True) (snd $ mapVowels xs)


> mapSilentE	     :: (String, [Bool]) -> (String, [Bool])
> mapSilentE (x, y)
>					  | endsWithE x && isESilent x = (x, replaceAt y False (endIndex y))
>					  | otherwise = (x, y)



** COMBINE ALL PARSING FUNCTIONS **

Calls all syllable parsing functions on a string and returns an Int representing the number of True in list of Bools
	
	* Confirm word has vowels, if not return 1
	* Map all vowels and semivowels
	* Account for 'q' + 'u'
	* Account for silent 'e'


> count	             :: String -> Int
> count []  		  = 0
> count xs
>			          | not (hasVowels xs) = 1
>			          | otherwise = length $ filter (== True) (snd $ mapQU$ mapSilentE $ mapVowels xs)



** COMPOSE HAIKU **
Takes a syllable count, and empty list and returns a random list of words where the total syllable count matches the argument
                     

> randWord            :: IO String 
> randWord             = do i <- randomRIO (0, length vocab - 1)
>                           return (vocab !! i) 


> compose            :: Int -> [String] -> IO [String]
> compose 0 xs        = return xs
> compose n xs        = do word <- randWord
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

