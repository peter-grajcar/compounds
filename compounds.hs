data Token = EmptyToken | LParenToken | RParenToken | AtomToken String | NumberToken Integer

instance Show Token where
    show EmptyToken = ""
    show LParenToken = "LParen"
    show RParenToken = "RParen"
    show (AtomToken a) = "Atom(" ++ a ++ ")"
    show (NumberToken n) = "Number(" ++ (show n) ++ ")"

isNumberToken :: Char -> Bool
isNumberToken c = elem c ['0'..'9']

isUpperLetter :: Char -> Bool
isUpperLetter c = elem c ['A'..'Z']

isLowerLetter :: Char -> Bool
isLowerLetter c = elem c ['a'..'z']

-- >>> map charToInteger "0123456789"
-- [0,1,2,3,4,5,6,7,8,9]
--
charToInteger :: Char -> Integer
charToInteger c = elemIndex 0 c ['0'..'9']
    where
        elemIndex :: Eq a => Integer -> a -> [a] -> Integer
        elemIndex _ _ [] = error "not a NumberToken"
        elemIndex i y (x:xs)
            | x == y    = i
            | otherwise = elemIndex (i+1) y xs


-- >>> tokenize "Na(OH)2"
-- [Atom(Na),LParen,Atom(O),Atom(H),RParen,Number(2)]
--
-- >>> tokenize "H2SO4"
-- [Atom(H),Number(2),Atom(S),Atom(O),Number(4)]
--
tokenize :: [Char] -> [Token]
tokenize input = tail (process EmptyToken input)
    where
        process :: Token -> [Char] -> [Token]
        process currentToken [] = [currentToken]
        process (NumberToken n) (x:xs)
            | isNumberToken x                       = process (NumberToken (n*10 + (charToInteger x))) xs
        process (AtomToken [a]) (x:xs)
            | isUpperLetter a && isLowerLetter x    = process (AtomToken ([a, x])) xs 
            | isUpperLetter x                       = (AtomToken [a]):process (AtomToken [x]) xs
        process currentToken (x:xs)
            | isNumberToken x                       = currentToken:process (NumberToken (charToInteger x)) xs
            | isUpperLetter x                       = currentToken:process (AtomToken [x]) xs
            | x == '('                              = currentToken:process (LParenToken) xs
            | x == ')'                              = currentToken:process (RParenToken) xs
            | otherwise                             = error ("unexpected char " ++ [x])

data Compound = Atom String Integer | Group [Compound] Integer

instance Show Compound where
    show (Atom a n) = "(" ++ a ++ (show n) ++ ")"
    show (Group cs n)
        | n == 1    = "(" ++ concat [show c | c <- cs] ++ ")" 
        | otherwise = "(" ++ concat [show c | c <- cs] ++ (show n) ++ ")" 

-- >>> parse [AtomToken "H", NumberToken 2, AtomToken "O", NumberToken 1]
-- ((H2)(O1))
--
-- >>> parse [AtomToken "Na", LParenToken, AtomToken "O", AtomToken "H", RParenToken, NumberToken 2]
-- ((Na1)((O1)(H1)2))
--
-- >>> parse [AtomToken "H", NumberToken 2, AtomToken "S", AtomToken "O", NumberToken 4]
-- ((H2)(S1)(O4))
--
parse :: [Token] -> Compound
parse tokens = parseCompound [Group [] 1] tokens
    where
        parseCompound :: [Compound] -> [Token] -> Compound
        parseCompound (c:cs) []
            | null cs           = c
            | otherwise         = error "too many elements on the stack"
        parseCompound ((Group gs n):cs) ((AtomToken a):(NumberToken an):ts) = parseCompound ((Group (gs ++ [Atom a an]) n):cs) ts
        parseCompound ((Group gs n):cs) ((AtomToken a):ts)                  = parseCompound ((Group (gs ++ [Atom a 1]) n):cs) ts
        parseCompound cs (LParenToken:ts)                                   = parseCompound ((Group [] 1):cs) ts
        parseCompound ((Group g1s _):(Group g2s n2):cs) (RParenToken:(NumberToken gn):ts)    = parseCompound ((Group (g2s ++ [Group g1s gn]) n2):cs) ts
        parseCompound ((Group g1s n1):(Group g2s n2):cs) (RParenToken:ts)                    = parseCompound ((Group (g2s ++ [Group g1s n1]) n2):cs) ts

        parseCompound _ (t:_) = error ("unexpected token " ++ (show t))
        parseCompound _ _ = error "error"


type ElementInfo = (String, String, String)

elements :: [ElementInfo]
elements = [("H", "hydrogen", "hydrogenium"), ("He", "helium", "helium"), ("S", "sulfur", "sulphur"), ("Na", "sodium", "natrium"), ("Fe", "iron", "ferrum")]

elementName :: ElementInfo -> String
elementName (_, name, _) = name

elementSymbol :: ElementInfo -> String
elementSymbol (symbol, _, _) = symbol

elementLatinName :: ElementInfo -> String
elementLatinName (_, _, latin) = latin

-- >>> getElementBySymbol "He"
-- ("He","helium")
--
getElementBySymbol :: String -> ElementInfo
getElementBySymbol symbol = search symbol elements
    where
        search :: String -> [ElementInfo] -> ElementInfo
        search sym (e:es)
            | sym == (elementSymbol e)  = e
            | otherwise                 = search sym es
        search sym [] = error ("element" ++ sym ++ "not found")

-- list of element suffixes from section IR-5.3.3.2
elementSuffixes :: [String]
elementSuffixes = ["ogen", "ygen", "orus", "ese", "ine", "ium", "ic", "en", "on", "um", "ur", "y"]

-- >>> indexOf 'l' "hello"
-- 2
--
indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = index 0 x xs
    where
        index :: Eq a => Int -> a -> [a] -> Int
        index _ _ [] = -1
        index n y (x:xs)
            | y == x    = n
            | otherwise = index (n+1) y xs

-- >>> getStem "oxygen"
-- "ox"
--
-- >>> getStem . elementName . getElementBySymbol $ "Na"
-- "sod"
--
getStem :: String -> String
getStem name = take (findSuffixIndex name elementSuffixes) name
    where
        findSuffixIndex :: String -> [String] -> Int
        findSuffixIndex name (e:es) = if i /= -1 then i else findSuffixIndex name es
            where i = indexOf e (suffixes name)
        findSuffixIndex name [] = length name

        suffixes :: [a] -> [[a]]
        suffixes []         = [] : []
        suffixes f@(_:t) = f: suffixes t 


-- "Exceptions include Zn and Group 18 elements ending in 'on', where the 'ide' ending is added to the element names"
-- "For some elements (e.g. Fe, Ag, Au) a Latin stem is used before the 'ide' ending"
-- >>> homoatomicAnionName "S"
-- "sulfide"
--
-- >>> homoatomicAnionName "Fe"
-- "ferride"
--
homoatomicAnionName :: String -> String
homoatomicAnionName symbol
    | symbol == "Zn"                    = "zincide"
    | symbol == "Ne"                    = "neonide"
    | symbol == "Ar"                    = "argonide"
    | symbol == "Kr"                    = "kryptonide"
    | symbol == "Xn"                    = "xenonide"
    | symbol == "Rn"                    = "radonide"
    | symbol == "Og"                    = "oganessonide"
    | elem symbol ["Fe", "Ag", "Au"]    = (getStem . elementLatinName . getElementBySymbol $ symbol) ++ "ide"
    | otherwise                         = (getStem . elementName . getElementBySymbol $ symbol) ++ "ide"
