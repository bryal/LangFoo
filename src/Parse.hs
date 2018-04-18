{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
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


data Result c out = Ok out [c]
                  | Err
  deriving (Show, Eq)

isErr Err = True
isErr _   = False

maybeDone :: Result c out -> Maybe out
maybeDone (Ok out []) = Just out
maybeDone _           = Nothing

mapOk :: (out -> [c] -> (out', [d])) -> Result c out -> Result d out'
mapOk f (Ok a s) = uncurry Ok (f a s)
mapOk _ _        = Err

bindOk :: (a -> Parser c b) -> Result c a -> Result c b
bindOk f (Ok a s) = let pb = f a
                    in pb s
bindOk _ _        = Err

mapOut :: (out -> out') -> Result c out -> Result c out'
mapOut f = mapOk (\a s -> (f a, s))

ror :: Result c out -> Result c out -> Result c out
ror a@(Ok _ _) _ = a
ror _ b          = b


type Parser c out = [c] -> Result c out

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

str :: Eq c => [c] -> Parser c [c]
str s1 s2 = maybe Err (Ok s1) (stripPrefix s1 s2)

recognize :: Parser c a -> Parser c [c]
recognize p s = mapOk (\_ s' -> (take (length s - length s') s, s')) (p s)

pany :: [Parser c a] -> Parser c a
pany ps s = fromMaybe Err (find (not . isErr) (fmap ($ s) ps))

pnot :: Parser c a -> Parser c ()
pnot p s = case p s of
             Ok _ _ -> Err
             Err    -> Ok () s

join :: (a -> b -> out) -> Parser c a -> Parser c b -> Parser c out
join f pa pb = pmap (uncurry f) (pa ~& pb)

rjoin :: (a -> b -> out) -> Result c a -> Parser c b -> Result c out
rjoin f (Ok a s) p = mapOut (f a) (p s)
rjoin _ Err _      = Err

appendFirst a (as, b) = (a:as, b)

multi0Then :: Parser c a -> Parser c b -> Parser c ([a], b)
multi0Then pa pb = pmap unwrapEither (por (join appendFirst
                                                pa (multi0Then pa pb))
                                          (pmap ([],)
                                                pb))

multi1Then :: Parser c a -> Parser c b -> Parser c ([a], b)
multi1Then pa pb = join appendFirst pa (multi0Then pa pb)

pseq :: [Parser c a] -> Parser c [a]
pseq []     = Ok []
pseq (p:ps) = p ~: pseq ps

por :: Parser c a -> Parser c b -> Parser c (Either a b)
por pa pb s = (pmap Left pa) s `ror` (pmap Right pb) s

(~|) = por

pand :: Parser c a -> Parser c b -> Parser c (a, b)
pand pa pb s = rjoin (,) (pa s) pb

(~&) = pand

pmap :: (a -> b) -> Parser c a -> Parser c b
pmap f p s = mapOut f (p s)

pconst :: out -> Parser c a -> Parser c out
pconst x = pmap (\a -> x)

multi0 :: Parser c a -> Parser c [a]
multi0 p s = case p s of
  Ok a s' -> mapOut (a:) (multi0 p s')
  Err     -> Ok [] s

multi1 :: Parser c a -> Parser c [a]
multi1 p = p ~: multi0 p

(~:) :: Parser c a -> Parser c [a] -> Parser c [a]
(~:) = join (:)

(~++) :: Parser c [a] -> Parser c [a] -> Parser c [a]
(~++) = join (++)

exact ::Parser c out -> [c] -> Maybe out
exact p s = maybeDone (p s)

exact' :: Parser c out -> [c] -> out
exact' p s = fromJust (exact p s)

mapRec :: ([c] -> b) -> Parser c a -> Parser c b
mapRec f p = pmap f (recognize p)

between :: Parser c a -> Parser c x -> Parser c b -> Parser c x
between pa p pb = pmap (fst . snd) (pand pa (pand p pb))

interspersed :: Parser c i -> Parser c a -> Parser c [a]
interspersed inter p = p ~: multi0 (pre inter p)

-- Preceded by
pre pa pb = pmap snd (pa ~& pb)

-------------------------------------------------------------

digit :: Parser Char Int
digit = pmap digitToInt ('0' `to` '9')

int :: Parser Char Integer
int = mapRec read (multi1 digit)

float :: Parser Char Double
float = mapRec read (int ~& str "." ~& int)

number :: Parser Char Double
number = pany [float, pmap fromIntegral int]

letter :: Parser Char Char
letter = pany ['a' `to` 'z', 'A' `to` 'Z']

ident :: Parser Char String
ident = recognize (letter ~& multi0 (letter ~| digit ~| ch '_'))

commaSep  = interspersed (wssOpt ~& str "," ~& wssOpt)
parenth p = between (ch '(' ~& wssOpt) p (wssOpt ~& ch ')')

-- Whitespace
ws :: Parser Char Char
ws = pany (map ch [' ', '\t', '\r', '\n'])

wss :: Parser Char String
wss = multi1 ws

wssOpt :: Parser Char String
wssOpt = multi0 ws

data Val = RealVal Double
         | BoolVal Bool
         | FuncVal (Val -> Val)
         | NilVal

instance Eq Val where
  (==) (RealVal a) (RealVal b) = abs (a - b) <= (a / 1000000000)
  (==) (BoolVal a) (BoolVal b) = a == b
  (==) NilVal      NilVal      = True
  (==) _ _                     = False

instance Show Val where
  show (RealVal x) = show x
  show (BoolVal b) = show b
  show (FuncVal _) = "<FUNCTION>"
  show NilVal      = "nil"

data Expr = Nil
          | Const Val
          | Var String
          | App Expr Expr
          | If Expr Expr Expr
          | Let [(String, Expr)] Expr
          | Lambda String Expr

showBinop op a b = "(" ++ show a ++ " " ++ op ++ " " ++ show b ++ ")"

instance Show Expr where
  show (Const x) = show x
  show (Var v)   = v
  show (App f e) = "(" ++ show f ++ " " ++ show e ++ ")"
  show (If p c a) = "(if " ++ show p ++ " then " ++ show c ++ " else " ++ show a ++ ")"
  show (Let bs e) = "(let " ++ intercalate ", " (fmap (\(b, v) -> b ++ " = " ++ show v) bs) ++ " in " ++ show e ++ ")"
  show (Lambda p e) = "(lam " ++ p ++ " -> " ++ show e ++ ")"

binop :: Parser Char Expr -> Parser Char Expr -> Parser Char Expr
binop parg pop =
  pmap (\(first, rest) -> foldl (\lhs (op, arg) -> App (App op lhs) arg) first rest)
       (parg ~& (multi0 ((between wssOpt pop wssOpt) ~& parg)))

infixified = between (str "`" ~& wssOpt) expr (wssOpt ~& str "`")
eqOp       = pmap Var (str "=")
times      = pmap Var (str "*")
divOp      = pmap Var (str "/")
plus       = pmap Var (str "+")
minus      = pmap Var (str "-")
operator   = pany [eqOp, times, divOp, plus, minus]
reserved   = pany (fmap str ["+", "-", "*", "/", "=", "true", "false", "nil", "if", "then", "else", "let", "in", "lam"])
bool       = pany [pconst True (str "true"), pconst False (str "false")]
nil        = pconst NilVal (str "nil")
val        = pany [pmap RealVal number, pmap BoolVal bool, nil]
const'     = pmap Const val
var        = pre (pnot reserved) (pmap Var ident)
if'        = pmap (\((p, c), a) -> If p c a)
                  (pre (str "if" ~& wss)          expr ~&
                   pre (wss ~& str "then" ~& wss) expr ~&
                   pre (wss ~& str "else" ~& wss) expr)
binding    = ident ~& pre (wss ~& str "=" ~& wss) expr
let'       = pmap (\(bnds, body) -> Let bnds body)
                  (pre (str "let" ~& wss) (commaSep binding) ~&
                   pre (wss ~& str "in" ~& wss) expr)
lambda     = pmap (\(params, body) -> foldr Lambda body params)
                  (pre (str "lam") (multi1 (pre wss ident)) ~&
                   pre (wssOpt ~& str "->" ~& wssOpt) expr)
arg        = pany [ const', if', let', var, parens ]
func       = arg
app        = pmap (\(f, args) -> foldl App f args) (func ~& multi1 (pre wssOpt arg))
preOpApp   = pmap (\(f, args) -> foldl App f args) (operator ~& multi1 (pre wssOpt arg))
parens     = pany (fmap parenth [ operator
                                , preOpApp
                                , expr])
factor     = pany [ const', if', let', lambda, app, var, parens ]
eq         = binop factor eqOp
mul        = binop eq times
div'       = binop mul divOp
add        = binop div' plus
sub        = binop add minus
userBinop  = binop sub infixified
expr       = userBinop

-- Operator precedence: *, /, +, -, =

parseExpr = exact' (between wssOpt expr wssOpt)
