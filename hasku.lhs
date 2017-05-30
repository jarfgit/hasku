> import Data.Char
> import Data.List

> vowels     = ['a','e','i','o','u','é']
> semiVowels = ['y']
> vocab  	 = [] 


** WORDS WITH NO VOWELS **
These are almost non-existent but easy to check for (so why not)

> hasVowels       :: [Char] -> Bool
> hasVowels []     = False 
> hasVowels (x:xs) = if elem x (vowels ++ semiVowels)

-- > hasVowels (x:xs) = if elem x vowels || x == 'y'

>						then True
>						else hasVowels xs


** CONSONANT + "LE" AS FINAL SYLLABLE **

Step 1: is 'e' the final char?  
Note: This should be checked separately to rule out words that don't end with e.

> endsWithE        :: [Char] -> Bool
> endsWithE []      = False
> endsWithE [x]     = False
> endsWithE xs      = if last xs == 'e' then True else False

Step 2: is last 'e' preceeded by 'l'? 

> isLBeforeE       :: [Char] -> Bool
> isLBeforeE []     = False
> isLBeforeE [x]    = False
> isLBeforeE xs     = if ((length xs) >= 2) && head (tail (reverse xs)) == 'l' then True else False 

Step 3: is "le" preceeded by a consonant?

> isConBeforeLE     :: [Char] -> Bool
> isConBeforeLE []   = False
> isConBeforeLE [x]  = False
> isConBeforeLE xs   = if ((length xs) >= 3) && elem (last (take 3 (reverse xs))) vowels then False else True 

Step 4: is "e" silent?

> isESilent         :: [Char] -> Bool
> isESilent []       = False
> isESilent [x]      = False
> isESilent xs       = if endsWithE xs && isLBeforeE xs && isConBeforeLE xs then False else True 

//TRUE + TRUE = FALSE

Test:

*Main> testString = "The little table has a tale to tell"
*Main> map isESilent $ words testString
[True,False,False,True,False,True,True,True]

Would only modify the elements that evaluate to False. Ignore any that evaluate to True.

Step 5: if 'e' is not silent, drop the last three character to modify the string, increment syllable count 

> dropConBeforeLE           :: ([Char], [Char], Int) -> ([Char], [Char], Int)
> dropConBeforeLE ([], y, n) = ([], y, n) -- !!! MAY NEED TO REVISIT THIS AS AN ERROR?
> dropConBeforeLE (x, [], n) = (x, [], n)
> dropConBeforeLE (x, y, n)  = if not (isESilent x) 
>									then (x, reverse (tail (reverse y)), n + 1) -- (x, reverse (drop 3 (reverse y)), n + 1) will drop + consonant +le
> 									else (x, y, n)


** DEALING WITH 'Y'
A vowel + 'y' creates a vowel sound, so these cases can be dealt with as diphthongs.
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
F - (ex. "lawyer", "canyon") 
T - (ex. "myopic", "cryogenesis") exception might be if vowel is an 'o'?
T - (ex. "crying") might be solved by dropping prefixes and suffixes prior to parsing vowels? 


Takes a string and returns a list of boolean values: True if char is a vowel False if not

> mapVowels						:: [Char] -> [Bool]
> mapVowels []				 	 = [] 
> mapVowels (x:xs) 			 	 = elem x vowels : mapVowels xs 

Takes a string and returns a list of indices for all occurances of the letter 'y'

> indexSemi					:: [Char] -> [Int]
> indexSemi	[]               = []
> indexSemi x 	         	 = findIndices (=='y') x

Modifies a list of boolean values for all vowels in a string accounting for special cases of the letter 'y'

> mapSemiVowels                 :: [Int] -> [Bool] -> [Bool]
> mapSemiVowels [] bs            = bs
> mapSemiVowels ns []            = []
> mapSemiVowels (n:ns) bs		 
>								 | n == 0 && not next = mapSemiVowels ns (replaceAt bs True n) 
>								 | n == (length bs - 1) && not prev = mapSemiVowels ns (replaceAt bs True n) 
>								 | 0 < n && n > (length bs -1) && not prev && not next = mapSemiVowels ns (replaceAt bs True n)  
>								 | otherwise = mapSemiVowels ns bs
>								  where prev = (bs !! (n-1))
>								    	next = (bs !! (n+1)) 




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

> dropVowels             	:: [Char] -> ([Char], [Char], Int) -> ([Char], [Char], Int)
> dropVowels [] (x, y, n)    = (x, y, n)
> dropVowels (v:vs) (x, y, n)  = if elem v y 
>									then dropVowels vs (x, filter (/=v) y, n + length (filter (==v) y))
>									else dropVowels vs (x, y, n)




** COMBINE ALL PARSING FUNCTIONS 

Call all syllable parsing functions on a string and return a triple with ("originalString", "modifiedString", syllableCount) in the following order:
	
	* Confirm word has vowels - if not return 1 for syllableCount
	* Parse all last syllables with consonant + 'le'
	* Parse all remaining vowels 


> countSyllables    :: [Char] -> ([Char], [Char], Int)
> countSyllables []  = ([], [], 0)
> countSyllables x   = if hasVowels x 
>									then dropVowels vowels (dropConBeforeLE (x, x, 0))
>									else (x, x, 1) -- words that have no vowels do have 1 syllable

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



** COMPOSE HAIKU **

** UTILITY FUNCTIONS **

Replaces a list item with a different value at a given index 

> replaceAt	 :: [a] -> a -> Int -> [a]
> replaceAt x y n
>					| n == 0 = [y] ++ tail x 
>					| n == (length x - 1) = reverse ([y] ++ tail (reverse x))
>					| 0 < n && n < length x - 1 = take (n) x ++ [y] ++ drop (n+1) x
>					| otherwise = undefined



