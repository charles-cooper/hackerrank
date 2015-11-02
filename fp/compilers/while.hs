-- https://www.hackerrank.com/challenges/while-language-fp
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as P
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Functor

data Stmt
  = WhileStmt Expr [Stmt]
  | AssignStmt Var Expr
  | IfStmt Expr [Stmt] [Stmt]
  deriving Show
data Expr
  = VarExpr Var
  | IntLit Integer
  | BoolLit Bool
  | BoolExpr BoolOp Expr Expr
  | IntExpr IntOp Expr Expr
  deriving Show
data ReducedExpr
  = RInt Integer
  | RBool Bool
  deriving Show

type Var = String
data BoolOp = And | Or
  deriving Show
data IntOp = Add | Sub | Mul | Div | Gt | Lt
  deriving Show

type Env = Map.Map String ReducedExpr
reduce :: Expr -> Env -> ReducedExpr
reduce e env = case e of
  VarExpr v       -> fromJust $ Map.lookup v env
  IntLit i        -> RInt i
  BoolLit b       -> RBool b
  BoolExpr op x y -> let
    RBool rx = reduce x env
    RBool ry = reduce y env
    in case op of
      And -> RBool $ rx && ry
      Or  -> RBool $ rx || ry
  IntExpr op x y  -> let
    RInt rx = reduce x env
    RInt ry = reduce y env
    in case op of
      Add -> RInt  $ rx + ry
      Sub -> RInt  $ rx - ry
      Mul -> RInt  $ rx * ry
      Div -> RInt  $ rx `div` ry
      Gt  -> RBool $ rx > ry
      Lt  -> RBool $ rx < ry

type Program = [Stmt]
step :: (Program, Env) -> (Program, Env)
step ([], env) = ([], env)
step ((s:ss), env) = case s of
  AssignStmt v e -> (ss, Map.insert v (reduce e env) env)
  IfStmt e caseT caseF -> case (reduce e env) of
    RBool True  -> let env' = runProgram caseT env in (ss, env')
    RBool False -> let env' = runProgram caseF env in (ss, env')
  WhileStmt e block -> case (reduce e env) of
    RBool True  -> let env' = runProgram block env in (s:ss, env')
    RBool False -> (ss, env)

runProgram :: Program -> Env -> Env
runProgram p env = snd $ fromJust
  $ find (\(p',_) -> null p')
  $ iterate (step) (p, env)

whitespace = P.whiteSpace lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
braces = P.braces lexer
assign = do
  v <- identifier
  reserved ":="
  e <- expr
  return $ AssignStmt v e
semi = P.semi lexer
whileStmt = do
  reserved "while"
  e <- parens expr
  reserved "do"
  body <- braces program
  return $ WhileStmt e body
stmt = try assign <|> whileStmt <|> ifStmt
ifStmt = do
  reserved "if"
  e <- parens expr
  reserved "then"
  ifT <- braces program
  reserved "else"
  ifF <- braces program
  return $ IfStmt e ifT ifF

expr = buildExpressionParser table term
bool = do {reserved "true"; return True} <|> do {reserved "false"; return False }
term = parens expr <|> IntLit <$> integer <|> try (BoolLit <$> bool) <|> VarExpr <$> identifier
binary sym f = Infix (do { reservedOp sym ; return f }) AssocLeft
lang = P.emptyDef
  { P.identStart = letter
  , P.identLetter = letter
  , P.reservedNames = ["true", "false"]
  , P.reservedOpNames = ["+","-","*","/",">","<", "and", "or"]
  }
lexer  = P.makeTokenParser lang
parens = P.parens lexer
reservedOp = P.reservedOp lexer
integer = P.integer lexer
table =
  [ [binary "*" (IntExpr Mul), binary "/" (IntExpr Div)]
  , [binary "+" (IntExpr Add), binary "-" (IntExpr Sub)]
  , [binary ">" (IntExpr Gt),  binary "<" (IntExpr Lt)]
  , [binary "and" (BoolExpr And)]
  , [binary "or" (BoolExpr Or)]
  ]

showVal :: ReducedExpr -> String
showVal e = case e of
  RInt i -> show i
  RBool True -> "true"
  RBool False -> "false"
fmt :: Env -> String
fmt env = unlines
  $ map (\(k,v) -> k ++ " " ++ showVal v)
  $ Map.toList env

program = stmt `sepBy` semi
main = do
  input <- getContents
  case parse program "" input of
    Left e -> print "Error e"
    Right p -> putStr $ fmt $ runProgram p Map.empty

