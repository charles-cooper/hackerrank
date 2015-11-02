-- https://www.hackerrank.com/challenges/down-with-abstractions
import Control.Monad
import Text.Parsec
import Control.Exception.Base (assert)
import Text.Parsec.Char
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as P
type Var = String
data Exp
  = VarExp Var
  | App Exp Exp
  | Lam Var Exp
  deriving (Show, Eq)

lang = P.emptyDef
  { P.reservedOpNames = ["\\","."]
  , P.identStart = alphaNum <|> char '_'
  , P.identLetter = alphaNum <|> char '_'
  }
lexer = P.makeTokenParser lang
parens = P.parens lexer
identifier = P.identifier lexer
reservedOp = P.reservedOp lexer
lam = parens $ do
  reservedOp "\\"
  vs <- many1 identifier
  reservedOp "."
  e <- expr
  return $ mkLam vs e
mkLam [] e = e
mkLam (v:vs) e = Lam v (mkLam vs e)
app = parens $ do
  e1 <- expr
  e2 <- expr
  return $ App e1 e2
var = VarExp <$> identifier
whitespace = P.whiteSpace lexer
expr = do {whitespace; (try var <|> try app <|> lam)}

freeIn :: Var -> Comb -> Bool
freeIn v e = let
  go _ True =  True
  go e _ = case e of
    VarT v' -> v == v'
    AppT e1 e2 -> go e1 False || go e2 False
    LamT v' e1 -> if v==v' then False else go e1 False
    Prim _ -> False
  in go e False

simple :: Exp -> Comb
simple (VarExp v) = VarT v
simple (App e1 e2) = AppT (simple e1) (simple e2)
simple (Lam x e) = LamT (x) (simple e)

reduce :: Comb -> Comb
reduce c = case c of
  VarT _ -> c
  AppT a b -> AppT (reduce a) (reduce b)
  LamT x e ->
    if not (x `freeIn` e) then AppT (Prim K) (reduce e)
    else case e of
      -- Invariant: x is free in e so if e is a varT it must be equal
      VarT y -> if x == y then Prim I else undefined
      LamT y e -> reduce $ LamT x (reduce $ LamT y e)
      AppT e1 e2 ->
        if VarT x == e2 && not (x `freeIn` e1) then reduce e1 -- eta reduce
        -- x must be free in e1, e2 or both.
        else if not (x `freeIn` e1) -- && x `freeIn` e2
        then AppT (AppT (Prim B) (reduce e1)) (reduce $ LamT x e2)
        else if {-x `freeIn` e1 && -} not (x `freeIn` e2)
        then AppT (AppT (Prim C) (reduce $ LamT x e1)) (reduce e2)
        -- else if x `freeIn` e1 && x `freeIn` e2
        else AppT (AppT (Prim S) (reduce $ LamT x e1)) (reduce $ LamT x e2)
      Prim _ -> reduce e
  p@(Prim {}) -> p

      -- _ -> error $ "Error: " ++ show e

eval :: Exp -> Comb
eval = reduce . simple

data P = K | S| I | B | C
  deriving (Show, Eq)
data Comb = VarT Var | Prim P | AppT Comb Comb | LamT Var Comb
  deriving (Show, Eq)

fmt :: Comb -> String
fmt c = case c of
  Prim p -> show p
  AppT c (Prim p') -> fmt c ++ show p'
  AppT c c' -> fmt c ++ "(" ++ fmt c' ++ ")"

main = do
  n <- getLine
  input <- getContents
  forM_ (lines input) $ \line -> case parse expr line line of
    Left e -> print $ e
    Right p -> do { putStrLn . fmt $ eval p }

