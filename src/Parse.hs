{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PostfixOperators  #-}
{-# LANGUAGE TupleSections     #-}

module Parse where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.String
import           Prelude     hiding (lex)


unwrapEither :: Either a a -> a
unwrapEither (Left a)  = a
unwrapEither (Right a) = a


data Result out c = Ok out [c]
                | Err
  deriving (Show, Eq)

isErr Err = True
isErr _   = False

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


type Parser out c = [c] -> Result out c

ch :: Eq c => c -> Parser c c
ch c []                  = Err
ch c (c':[]) | c == c'   = Ok c []
             | otherwise = Err
ch c s | c == head s     = Ok (head s) (tail s)
       | otherwise       = Err

range :: Ord c => c -> c -> Parser c c
range start end (c:s) | c >= start && c <= end = Ok c s
range _ _ _           = Err

to = range

wild :: Parser c c
wild []    = Err
wild (c:s) = Ok c s

str :: Eq c => [c] -> Parser [c] c
str s1 s2 = maybe Err (Ok s1) (stripPrefix s1 s2)

recognize :: Parser a c -> Parser [c] c
recognize p s = mapOk (\_ s' -> (take (length s - length s') s, s')) (p s)

pany :: [Parser a c] -> Parser a c
pany ps s = fromMaybe Err (find (not . isErr) (fmap ($ s) ps))

join :: (a -> b -> out) -> Parser a c -> Parser b c -> Parser out c
join f pa pb = pmap (uncurry f) (pa ~& pb)

rjoin :: (a -> b -> out) -> Result a c -> Parser b c -> Result out c
rjoin f (Ok a s) p = mapOut (f a) (p s)
rjoin _ Err _      = Err

appendFirst a (as, b) = (a:as, b)

multi0Then :: Parser a c -> Parser b c -> Parser ([a], b) c
multi0Then pa pb = pmap unwrapEither (por (join appendFirst
                                                pa (multi0Then pa pb))
                                          (pmap ([],)
                                                pb))

multi1Then :: Parser a c -> Parser b c -> Parser ([a], b) c
multi1Then pa pb = join appendFirst pa (multi0Then pa pb)

pseq :: [Parser a c] -> Parser [a] c
pseq []     = Ok []
pseq (p:ps) = p ~: pseq ps

por :: Parser a c -> Parser b c -> Parser (Either a b) c
por pa pb s = (pmap Left pa) s `ror` (pmap Right pb) s

(~|) = por

pand :: Parser a c -> Parser b c -> Parser (a, b) c
pand pa pb s = rjoin (,) (pa s) pb

(~&) = pand

pmap :: (a -> b) -> Parser a c -> Parser b c
pmap f p s = mapOut f (p s)

multi0 :: Parser a c -> Parser [a] c
multi0 p s = case p s of
  Ok a s' -> mapOut (a:) (multi0 p s')
  Err     -> Ok [] s

multi1 :: Parser a c -> Parser [a] c
multi1 p = p ~: multi0 p

(~:) :: Parser a c -> Parser [a] c -> Parser [a] c
(~:) = join (:)

(~++) :: Parser [a] c -> Parser [a] c -> Parser [a] c
(~++) = join (++)

exact ::Parser out c -> [c] -> Maybe out
exact p s = maybeDone (p s)

exact' :: Parser out c -> [c] -> out
exact' p s = fromJust (exact p s)

mapRec :: ([c] -> b) -> Parser a c -> Parser b c
mapRec f p = pmap f (recognize p)

between :: Parser a c -> Parser x c -> Parser b c -> Parser x c
between pa p pb = pmap (fst . snd) (pand pa (pand p pb))

-- Preceded by
pre pa pb = pmap snd (pa ~& pb)

-------------------------------------------------------------

digit :: Parser Int Char
digit = pmap digitToInt ('0' `to` '9')

int :: Parser Int Char
int = mapRec read (multi1 digit)

float :: Parser Double Char
float = mapRec read (int ~& str "." ~& int)

number :: Parser Double Char
number = pany [float, pmap fromIntegral int]

letter :: Parser Char Char
letter = pany ['a' `to` 'z', 'A' `to` 'Z']

ident :: Parser String Char
ident = recognize (letter ~& multi0 (letter ~| digit ~| ch '_'))

-- Whitespace
ws :: Parser Char Char
ws = pany (map ch [' ', '\t', '\r', '\n'])

wss :: Parser String Char
wss = multi1 ws

wssOpt :: Parser String Char
wssOpt = multi0 ws

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
          | Eq  Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | App Func Expr
          | If Expr Expr Expr

showBinop op a b = "(" ++ show a ++ " " ++ op ++ " " ++ show b ++ ")"

instance Show Expr where
  show (Const x) = show x
  show (Eq  a b) = showBinop "=" a b
  show (Mul a b) = showBinop "*" a b
  show (Div a b) = showBinop "/" a b
  show (Add a b) = showBinop "+" a b
  show (Sub a b) = showBinop "-" a b
  show (App f e) = "(" ++ show f ++ " " ++ show e ++ ")"
  show (If p c a) = "(if " ++ show p ++ " then " ++ show c ++ " else " ++ show a ++ ")"

binop :: Parser Expr Char -> String -> (Expr -> Expr -> Expr) -> Parser Expr Char
binop arg opName constr =
  pmap (\args -> foldr constr (head args) (tail args))
       (join (:) arg (multi0 (pre (wssOpt ~& str opName ~& wssOpt) arg)))

const'     = pmap Const number
if'        = pmap (\((p, c), a) -> If p c a)
                  (pre (str "if" ~& wss)          expr ~&
                   pre (wss ~& str "then" ~& wss) expr ~&
                   pre (wss ~& str "else" ~& wss) expr)
funcArg    = pany [ const', parens ]

coreFunc constr name = pmap (\_ -> constr) (str name)

sin' = coreFunc Sin "sin"
cos' = coreFunc Cos "cos"
exp' = coreFunc Exp "exp"
log' = coreFunc Log "log"

func = pany [ sin', cos', exp', log' ]

app        = pmap (\((f, _), arg) -> App f arg) (func ~& wss ~& funcArg)
parens     = between (str "(" ~& wssOpt) expr (wssOpt ~& str ")")
factor     = pany [ const', if', app, parens ]
eq         = binop factor "=" Eq
mul        = binop eq "*" Mul
div'       = binop mul "/" Div
add        = binop div' "+" Add
sub        = binop add "-" Sub
expr       = sub

-- Operator precedence: *, /, +, -, =

parseExpr = exact' expr
