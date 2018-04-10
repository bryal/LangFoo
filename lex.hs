{-# LANGUAGE PostfixOperators, OverloadedStrings, FlexibleInstances #-}

import Prelude hiding (lex)
import Data.String
import Data.List
import Data.Maybe

data Lexer c = Ch c
             | Wildcard
             | Str [c]
             | Any [Lexer c]
             | Seq [Lexer c]
             | (:*) (Lexer c)
  deriving (Show, Eq)

(~*) = (:*)
(~+) l = Seq [l, (l~*)]

instance IsString (Lexer Char) where
  fromString = Str

data Result c = Done [c]
              | Remains [c] [c]
              | Err
  deriving (Show, Eq)

maybeDone :: Result c -> Maybe [c]
maybeDone (Done s) = Just s
maybeDone _        = Nothing

remToResult :: [c] -> [c] -> Result c
remToResult s []  = Done s
remToResult s rem = Remains s rem

mapResult :: ([c] -> [d]) -> Result c -> Result d
mapResult f (Done s)       = Done (f s)
mapResult f (Remains s s') = remToResult (f s) (f s')
mapResult f Err            = Err

mapMatched :: ([c] -> [c]) -> Result c -> Result c
mapMatched f (Done s)       = Done (f s)
mapMatched f (Remains s s') = Remains (f s) s'
mapMatched f Err            = Err

bindRem :: ([c] -> Result c) -> Result c -> Result c
bindRem mf (Done s)       = bindRem mf (Remains s [])
bindRem mf (Remains s s') = mapMatched (s++) (mf s')
bindRem mf Err            = Err

toChar :: Int -> Char
toChar = toEnum

to :: Char -> Char -> Lexer Char
to c1 c2 = Any (fmap (Ch . toChar) [fromEnum c1 .. fromEnum c2])

(~|) :: Lexer c -> Lexer c -> Lexer c
(~|) l1 l2 = Any [l1, l2]

(~&) :: Lexer c -> Lexer c -> Lexer c
(~&) l1 l2 = Seq [l1, l2]

lexChar :: (Eq c) => c -> [c] -> Result c
lexChar c [] = Err
lexChar c (c':[]) | c == c'   = Done (c:[])
                  | otherwise = Err
lexChar c s | c == head s = Remains (take 1 s) (tail s)
            | otherwise   = Err

lexWild :: (Eq c) => [c] -> Result c
lexWild []     = Err
lexWild (c:[]) = Done (c:[])
lexWild (c:s)  = Remains (c:[]) s

lexSeq' :: (Eq c) => [Lexer c] -> Result c -> Result c
lexSeq' [] r          = r
lexSeq' ((:*) l:ls) r = bindRem (lexStarFollowedBy l (Seq ls)) r
lexSeq' (l:ls) r      = lexSeq' ls (bindRem (lex l) r)

lexSeq :: (Eq c) => [Lexer c] -> [c] -> Result c
lexSeq l s = lexSeq' l (Remains [] s)

lexStar :: (Eq c) => Lexer c -> [c] -> Result c
lexStar l s = case lex l s of
  Done s'        -> Done s'
  Remains s' s'' -> mapMatched (s'++) (lexStar l s'')
  Err            -> if null s then Done [] else Remains [] s

-- Note: Greedy
lexStarFollowedBy :: (Eq c) => Lexer c -> Lexer c -> [c] -> Result c
lexStarFollowedBy l l' s = lex (Seq [l, (l~*), l'] ~| l') s

lex :: (Eq c) => Lexer c -> [c] -> Result c
lex (Ch c) s     = lexChar c s
lex Wildcard s   = lexWild s
lex (Str s1) s2  = maybe Err (remToResult s1) (stripPrefix s1 s2)
lex (Any ls) s   = fromMaybe Err (find (/= Err) (fmap (flip lex s) ls))
lex (Seq ls) s   = lexSeq ls s
lex ((:*) l) s   = lexStar l s

-- | No remainder allowed
lexExact :: (Eq c) => Lexer c -> [c] -> Maybe [c]
lexExact l s = maybeDone (lex l s)

--------------------------------------------------------

digit = '0' `to` '9'
int = (digit~+)
float = Seq [int, ".", int]
letter = ('a' `to` 'z') ~| ('A' `to` 'Z')
str = Seq [Ch '"', (Wildcard~*), Ch '"']
p
