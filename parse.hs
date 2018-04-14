{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators  #-}
{-# LANGUAGE TupleSections     #-}

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.String
import           Prelude     hiding (lex)

data Parser out c where
  Ch        :: c -> Parser c c
  Range     :: Ord c => c -> c -> Parser c c
  Wildcard  :: Parser c c
  Str       :: [c] -> Parser [c] c
  Recognize :: Parser a c -> Parser [c] c
  Any       :: [Parser a c] -> Parser a c
  Seq       :: [Parser a c] -> Parser [a] c
  Or        :: Parser a c -> Parser b c -> Parser (Either a b) c
  And       :: Parser a c -> Parser b c -> Parser (a, b) c
  Map       :: (a -> b) -> Parser a c -> Parser b c
  (:*)      :: Parser a c -> Parser [a] c

instance IsString (Parser String Char) where
  fromString = Str

(~*) = (:*)
(~+) l = l ~: (l~*)
(~|) = Or
(~&) = And

(~:) :: Parser a c -> Parser [a] c -> Parser [a] c
(~:) l ll = Map (\(a, as) -> a:as) (l ~& ll)

(~++) :: Parser [a] c -> Parser [a] c -> Parser [a] c
(~++) la lb = Map (\(as, bs) -> as ++ bs) (la ~& lb)

to :: Char -> Char -> Parser Char Char
to = Range

unwrapEither :: Either a a -> a
unwrapEither (Left a)  = a
unwrapEither (Right a) = a

data Result out c = Ok out [c]
                | Err
  deriving (Show, Eq)

maybeDone :: Result out c -> Maybe out
maybeDone (Ok out []) = Just out
maybeDone _           = Nothing

mapOk :: (out -> [c] -> (out', [d])) -> Result out c -> Result out' d
mapOk f (Ok a s) = uncurry Ok (f a s)
mapOk _ _        = Err

mapOut :: (out -> out') -> Result out c -> Result out' c
mapOut f = mapOk (\a s -> (f a, s))

ror :: Result out c -> Result out c -> Result out c
ror a@(Ok _ _) _ = a
ror _ b          = b

bindRem :: Monoid out => ([c] -> Result out c) -> Result out c -> Result out c
bindRem mf (Ok a s) = mapOut (mappend a) (mf s)
bindRem mf Err      = Err

bindRem' :: (a -> b -> out) -> ([c] -> Result b c) -> Result a c -> Result out c
bindRem' comb mf (Ok a s) = mapOut (comb a) (mf s)
bindRem' comb mf Err      = Err

parseChar :: (Eq c) => c -> [c] -> Result c c
parseChar c [] = Err
parseChar c (c':[]) | c == c'   = Ok c []
                  | otherwise = Err
parseChar c s | c == head s = Ok (head s) (tail s)
            | otherwise   = Err

parseRange :: Ord c => c -> c -> [c] -> Result c c
parseRange start end (c:s) | c >= start && c <= end = Ok c s
parseRange _ _ _           = Err

parseWild :: (Eq c) => [c] -> Result c c
parseWild []    = Err
parseWild (c:s) = Ok c s

parseSeq :: Eq c => [Parser a c] -> [c] -> Result [a] c
parseSeq [] s          = Ok [] s
parseSeq ((:*) l:ls) s = mapOut (uncurry (:)) (parseStarFollowedBy l (Seq ls) s)
parseSeq (l:ls) s      = bindRem' (:) (parseSeq ls) (parse l s)

parseStarFollowedBy :: Eq c => Parser a c -> Parser b c -> [c] -> Result ([a], b) c
parseStarFollowedBy la lb s = ror (bindRem' (\a (as, b) -> (a:as, b))
                                          (parse ((la~*) ~& lb))
                                          (parse la s))
                                (mapOut ([],) (parse lb s))

parseAnd :: Eq c => Parser a c -> Parser b c -> [c] -> Result (a, b) c
parseAnd (And la ((:*) lb)) lc s = mapOut (\(a, (b, c)) -> ((a, b), c))
                                      (parseAnd la (And (lb~*) lc) s)
parseAnd ((:*) l) lb s = parseStarFollowedBy l lb s
parseAnd la       lb s = bindRem' (,) (parse lb) (parse la s)

parseStar :: Eq c => Parser a c -> [c] -> Result [a] c
parseStar l s = case parse l s of
  Ok a s' -> mapOut (a:) (parseStar l s')
  Err     -> Ok [] s

isErr Err = True
isErr _   = False

parse :: Eq c => Parser out c -> [c] -> Result out c
parse (Ch c) s          = parseChar c s
parse (Range c1 c2) s   = parseRange c1 c2 s
parse Wildcard s        = parseWild s
parse (Str s1) s2       = maybe Err (Ok s1) (stripPrefix s1 s2)
parse (Recognize l) s   = mapOk (\_ s' -> (take (length s - length s') s, s')) (parse l s)
parse (Any ls) s        = fromMaybe Err (find (not . isErr) (fmap (flip parse s) ls))
parse (Seq ls) s        = parseSeq ls s
parse (Or la lb) s      = (mapOut Left (parse la s)) `ror` (mapOut Right (parse lb s))
parse (And la lb) s     = parseAnd la lb s
parse (Map f l) s       = mapOut f (parse l s)
parse ((:*) l) s        = parseStar l s

-- | No remains allowed
parseExact :: Eq c => Parser out c -> [c] -> Maybe out
parseExact l s = maybeDone (parse l s)

mapRec :: ([c] -> b) -> Parser a c -> Parser b c
mapRec f l = Map f (Recognize l)

between :: Parser a c -> Parser x c -> Parser b c -> Parser x c
between la l lb = Map (fst . snd) (And la (And l lb))

digit :: Parser Int Char
digit = Map digitToInt ('0' `to` '9')

int :: Parser Int Char
int = mapRec read (digit~+)

float :: Parser Double Char
float = mapRec read (int ~& Str "." ~& int)

number :: Parser Double Char
number = Any [float, Map fromIntegral int]

letter :: Parser Char Char
letter = Any ['a' `to` 'z', 'A' `to` 'Z']

ident :: Parser String Char
ident = Recognize (letter ~& ((letter ~| digit ~| Ch '_')~*))

-- Whitespace
ws :: Parser Char Char
ws = Any (map Ch [' ', '\t', '\r', '\n'])

wss :: Parser String Char
wss = (ws~+)

wssOpt :: Parser String Char
wssOpt = (ws~*)

data Func = Sin
          | Cos
          | Exp
          | Log

instance Show Func where
  show f = case f of
    Sin -> "sin"
    Cos -> "cos"
    Exp -> "exp"
    Log -> "log"

data Expr = Const Double
          | Mul Expr Expr
          | Div Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | App Func Expr

showBinop op a b = "(" ++ show a ++ " " ++ op ++ " " ++ show b ++ ")"

instance Show Expr where
  show (Const x) = show x
  show (Mul a b) = showBinop "*" a b
  show (Div a b) = showBinop "/" a b
  show (Add a b) = showBinop "+" a b
  show (Sub a b) = showBinop "-" a b
  show (App f e) = "(" ++ show f ++ " " ++ show e ++ ")"

binop :: Parser Expr Char -> String -> (Expr -> Expr -> Expr) -> Parser Expr Char
binop arg opName constr =
  Map (\(as, b) -> let args = (map (fst . fst . fst) as ++ [b])
                   in foldl constr (head args) (tail args))
      (((arg ~& wssOpt ~& Str opName ~& wssOpt)~+) ~& arg)

const'     = Map Const number
funcArg    = Any [ const', parens ]

coreFunc constr name = Map (\_ -> constr) (Str name)

sin' = coreFunc Sin "sin"
cos' = coreFunc Cos "cos"
exp' = coreFunc Exp "exp"
log' = coreFunc Log "log"

func = Any [ sin', cos', exp', log' ]

app        = Map (\((f, _), arg) -> App f arg) (func ~& wss ~& funcArg)
parens     = between (Str "(" ~& wssOpt) expr (wssOpt ~& Str ")")
factor     = Any [ const', app, parens ]

mulArg     = factor
mul        = binop mulArg "*" Mul
divArg     = Any [mul, mulArg]
div'       = binop divArg "/" Div
addArg     = Any [div', divArg]
add        = binop addArg "+" Add
subArg     = Any [add, addArg]
sub        = binop subArg "-" Sub

expr       = Any [sub, subArg]

-- Operator precedence: *, /, +, -

