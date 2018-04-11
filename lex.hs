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

data Result c = Ok [c] [c]
              | Err
  deriving (Show, Eq)

maybeDone :: Result c -> Maybe [c]
maybeDone (Ok s []) = Just s
maybeDone _         = Nothing

mapResult :: ([c] -> [d]) -> Result c -> Result d
mapResult f (Ok s s') = Ok (f s) (f s')
mapResult f Err       = Err

mapMatched :: ([c] -> [c]) -> Result c -> Result c
mapMatched f (Ok s s') = Ok (f s) s'
mapMatched f Err       = Err

bindRem :: ([c] -> Result c) -> Result c -> Result c
bindRem mf (Ok s s') = mapMatched (s++) (mf s')
bindRem mf Err       = Err

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
lexChar c (c':[]) | c == c'   = Ok (c:[]) []
                  | otherwise = Err
lexChar c s | c == head s = Ok (take 1 s) (tail s)
            | otherwise   = Err

lexWild :: (Eq c) => [c] -> Result c
lexWild []     = Err
lexWild (c:s)  = Ok (c:[]) s

lexSeq' :: (Eq c) => [Lexer c] -> Result c -> Result c
lexSeq' [] r          = r
lexSeq' ((:*) l:ls) r = bindRem (lexStarFollowedBy l (Seq ls)) r
lexSeq' (l:ls) r      = lexSeq' ls (bindRem (lex l) r)

lexSeq :: (Eq c) => [Lexer c] -> [c] -> Result c
lexSeq l s = lexSeq' l (Ok [] s)

lexStar :: (Eq c) => Lexer c -> [c] -> Result c
lexStar l s = case lex l s of
  Ok s' s'' -> mapMatched (s'++) (lexStar l s'')
  Err       -> Ok [] s

-- Note: Greedy
lexStarFollowedBy :: (Eq c) => Lexer c -> Lexer c -> [c] -> Result c
lexStarFollowedBy l l' s = lex (Seq [l, (l~*), l'] ~| l') s

lex :: (Eq c) => Lexer c -> [c] -> Result c
lex (Ch c) s    = lexChar c s
lex Wildcard s  = lexWild s
lex (Str s1) s2 = maybe Err (Ok s1) (stripPrefix s1 s2)
lex (Any ls) s  = fromMaybe Err (find (/= Err) (fmap (flip lex s) ls))
lex (Seq (l:(Seq ls):ls')) s = lex (Seq (l:(ls++ls'))) s
lex (Seq ls) s  = lexSeq ls s
lex ((:*) l) s  = lexStar l s

-- | No remainder allowed
lexExact :: (Eq c) => Lexer c -> [c] -> Maybe [c]
lexExact l s = maybeDone (lex l s)

--------------------------------------------------------

digit  = '0' `to` '9'
int    = (digit~+)
float  = int ~& "." ~& int
number = float ~| int

letter = ('a' `to` 'z') ~| ('A' `to` 'Z')
ident  = letter ~& ((letter ~| digit ~| "_")~*)

str    = Seq [Ch '"', (Wildcard~*), Ch '"']

-- Whitespace
wsChar = Any [" ", "\t", "\r", "\n"]
ws     = (wsChar~+)
wsOpt  = (wsChar~*)

binop arg op = ((arg ~& wsOpt ~& op)~+) ~& wsOpt ~& arg

factor     = number ~| Seq ["(", wsOpt, expr, wsOpt, ")"]
mulArg     = factor
mul        = binop mulArg "*"
divArg     = mul ~| mulArg
div'       = binop divArg "/"
addArg     = div' ~| divArg
add        = binop addArg "+"
subArg     = add ~| addArg
sub        = binop subArg "-"
expr       = sub ~| subArg

-- Operator precedence: *, /, +, -
