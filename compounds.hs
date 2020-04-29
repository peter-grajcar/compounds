data Token = EmptyToken | LParenToken | RParenToken | AtomToken String | NumberToken Int

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

-- >>> map charToInt "0123456789"
-- [0,1,2,3,4,5,6,7,8,9]
--
charToInt :: Char -> Int
charToInt c = elemIndex 0 c ['0'..'9']
    where
        elemIndex :: Eq a => Int -> a -> [a] -> Int
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
            | isNumberToken x                       = process (NumberToken (n*10 + (charToInt x))) xs
        process (AtomToken [a]) (x:xs)
            | isUpperLetter a && isLowerLetter x    = process (AtomToken ([a, x])) xs 
            | isUpperLetter x                       = (AtomToken [a]):process (AtomToken [x]) xs
        process currentToken (x:xs)
            | isNumberToken x                       = currentToken:process (NumberToken (charToInt x)) xs
            | isUpperLetter x                       = currentToken:process (AtomToken [x]) xs
            | x == '('                              = currentToken:process (LParenToken) xs
            | x == ')'                              = currentToken:process (RParenToken) xs
            | otherwise                             = error ("unexpected char " ++ [x])

data Compound = Atom Element Int | Group [Compound] Int

instance Show Compound where
    show (Atom a n) = "(" ++ (elementSymbol a) ++ (show n) ++ ")"
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
        parseCompound ((Group gs n):cs) ((AtomToken sym):(NumberToken an):ts) = parseCompound ((Group (gs ++ [Atom (getElementBySymbol sym) an]) n):cs) ts
        parseCompound ((Group gs n):cs) ((AtomToken sym):ts)                  = parseCompound ((Group (gs ++ [Atom (getElementBySymbol sym) 1]) n):cs) ts
        parseCompound cs (LParenToken:ts)                                   = parseCompound ((Group [] 1):cs) ts
        parseCompound ((Group g1s _):(Group g2s n2):cs) (RParenToken:(NumberToken gn):ts)    = parseCompound ((Group (g2s ++ [Group g1s gn]) n2):cs) ts
        parseCompound ((Group g1s n1):(Group g2s n2):cs) (RParenToken:ts)                    = parseCompound ((Group (g2s ++ [Group g1s n1]) n2):cs) ts

        parseCompound _ (t:_) = error ("unexpected token " ++ (show t))
        parseCompound _ _ = error "error"


type Element = (String, String, String)

elements :: [Element]
elements = [("H", "hydrogen", "hydrogenium"), ("He", "helium", "helium"), ("S", "sulfur", "sulphur"), ("Na", "sodium", "natrium"), ("Fe", "iron", "ferrum"), ("O", "oxygen", "oxygenium"), ("Ca", "calcium", "calcium"), ("P", "phosphorus", "phosphorus"), ("C", "carbon", "carbo")]

-- list of element suffixes from section IR-5.3.3.2
elementSuffixes :: [String]
elementSuffixes = ["ogen", "ygen", "orus", "ese", "ine", "ium", "ic", "en", "on", "um", "ur", "y"]

-- list of simple multiplicative prefixes
simpleMultPrefixes :: [String]
simpleMultPrefixes = ["", "di", "tri", "tetra", "penta", "hexa", "hepta", "octa", "nona", "deca", "undeca", "dodeca"]

-- list of complicated multiplicative prefixes
complicatedMultPrefixes :: [String]
complicatedMultPrefixes = ["", "bis", "tris", "tetrakis", "pentakis", "hexakis", "heptakis", "octakis", "nonakis", "decakis", "undecakis", "dodecakis"]

elementName :: Element -> String
elementName (_, name, _) = name

elementSymbol :: Element -> String
elementSymbol (symbol, _, _) = symbol

elementLatinName :: Element -> String
elementLatinName (_, _, latin) = latin

-- >>> getElementBySymbol "He"
-- ("He","helium","helium")
--
getElementBySymbol :: String -> Element
getElementBySymbol symbol = search symbol elements
    where
        search :: String -> [Element] -> Element
        search sym (e:es)
            | sym == (elementSymbol e)  = e
            | otherwise                 = search sym es
        search sym [] = error ("element " ++ sym ++ " not found")

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
-- >>> anionName $ Atom (getElementBySymbol "S") 2
-- "disulfide"
--
-- >>> anionName $ Atom (getElementBySymbol "Fe") 1
-- "ferride"
--
anionName :: Compound -> String
anionName (Atom e n) = (simpleMultPrefixes !! (n - 1)) ++ (ide e)
    where 
        ide :: Element -> String
        ide e
            | elementSymbol e == "Zn"   = "zincide"
            | elementSymbol e == "Ne"   = "neonide"
            | elementSymbol e == "Ar"   = "argonide"
            | elementSymbol e == "Kr"   = "kryptonide"
            | elementSymbol e == "Xn"   = "xenonide"
            | elementSymbol e == "Rn"   = "radonide"
            | elementSymbol e == "Og"   = "oganessonide"
            | elem (elementSymbol e) ["Fe", "Ag", "Au", "Pb", "Sn", "Cu"]    
                                        = (getStem . elementLatinName $ e) ++ "ide"
            | otherwise                 = (getStem . elementName $ e) ++ "ide"
anionName c = error ((show c) ++ " is not a homoatomic anion")


-- Element Sequence, Table VI, page 260
elementSequence :: [String]
elementSequence = ["F", "Cl", "Br", "I", "At", "O", "S", "Se", "Te", "Po", "Lv", "H", "N", "P", 
                    "As", "Sb", "Bi", "C", "Si", "Ge", "Sn", "Pb", "Fl", "B", "Al", "Ga", "In", 
                    "Tl", "Zn", "Cd", "Hg", "Cn", "Cu", "Ag", "Au", "Rg", "Ni", "Pd", "Pt", "Ds",
                    "Co", "Rh", "Ir", "Mt", "Fe", "Ru", "Os", "Hs", "Mn", "Tc", "Re", "Bh", "Cr",
                    "Mo", "W", "Sg", "V", "Nb", "Ta", "Db", "Ti", "Zr", "Hf", "Rf", "Sc", "Y", 
                    "La", "Lu", "Ac", "Lr", "Be", "Mg", "Ca", "Sr", "Ba", "Ra", "Li", "Na", "K",
                    "Rb", "Cs", "Fr", "He", "Ne", "Ar", "Kr", "Xe", "Rn", "Og"]


-- >>> sequencePosition $ parse (tokenize "O2")
-- 5
--
-- >>> sequencePosition $ parse (tokenize "(PO4)2")
-- 5
--
sequencePosition :: Compound -> Int
sequencePosition (Atom  e  _) = indexOf (elementSymbol e) elementSequence
sequencePosition (Group cs _) = minimum (map (sequencePosition) cs)

-- splits the compoud to electronegative constituent and electropositive constituents
-- >>> splitConstituents $ parse (tokenize "Ca3(PO4)2")
-- (((P1)(O4)2),[(Ca3)])
--
-- >>> splitConstituents $ parse (tokenize "CO2")
-- ((O2),[(C1)])
--
splitConstituents :: Compound -> (Compound, [Compound])
splitConstituents (Group cs _) = foldr  (\c (neg, pos) -> if (sequencePosition c) < (sequencePosition neg) then (c, neg:pos)  else (neg, c:pos) ) 
                                        (head cs, []) 
                                        (tail cs)
splitConstituents _ = undefined

compoundName :: Compound -> String
compoundName = undefined