-- https://www.hackerrank.com/challenges/expressions-v2
import Text.Parsec
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as P
import Text.Parsec.Expr
import Data.List (intercalate)

magic = 1000*1000*1000 + 7

data Exp
  = Const Int
  | UnExp Op Exp
  | BinExp Op Exp Exp
  deriving (Eq, Show)

data Op = Add | Mul | Sub | Div | Neg | Pos deriving (Eq, Show)

powm :: Int -> Int -> Int -> Int
powm b e m = go b e m 1 where
go b 0 m r = r
go b e m r | e `mod` 2 == 1 = go (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
go b e m r = go (b * b `mod` m) (e `div` 2) m r

eval :: Exp -> Int
eval e = case e of
  Const i -> i
  UnExp op e -> let i = eval e in case op of
    Neg -> -i `mod` magic
    Pos -> i `mod` magic
  BinExp op ex ey -> let
    x = eval ex
    y = eval ey
    in case op of
      Add -> (x + y) `mod` magic
      Mul -> (x * y) `mod` magic
      Sub -> (x - y) `mod` magic
      Div -> eval $ BinExp Mul (Const x) (Const (powm y (magic - 2) magic))

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
postfix name fun       = Postfix (do{ reservedOp name; return fun })

lang = P.emptyDef
lexer = P.makeTokenParser lang

reservedOp = P.reservedOp lexer
integer = P.integer lexer
natural = P.natural lexer
parens = P.parens lexer
symbol = P.symbol lexer
expr = term `chainr1` addop
term = factor `chainr1` mulop
factor = (option id prefix) <*> (parens expr <|> Const . fromInteger <$> natural)
prefix =
  do {symbol "+"; return $ UnExp Pos}
  <|>
  do {symbol "-"; return $ UnExp Neg}
mulop =
  do {symbol "*"; return $ BinExp Mul}
  <|>
  do {symbol "/"; return $ BinExp Div}
addop =
  do {symbol "+"; return $ BinExp Add}
  <|>
  do {symbol "-"; return $ BinExp Sub}

main = do
  input <- getContents
  case parse expr "" input of
    Left e -> error $ show e
    Right e -> print $ eval e

