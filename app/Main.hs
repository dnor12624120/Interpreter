{-# LANGUAGE DeriveFunctor, TypeFamilies, LambdaCase, UndecidableInstances #-}

import Data.Map
import qualified Data.Map as M
import Control.Monad.Reader

newtype Rec f = In { out :: f (Rec f) }

type Algebra f a = f a -> a

type Id = String 
type Error = String

type ExecutionStack = [Statement]
type SymbolTable = Map Id (Maybe Value)
type OutputStream = [Value]

type EvalReader = Reader EvalState (Either Error Value)

data Type = 
    IntT 
    | BoolT
    deriving (Show, Eq)

data Value = 
    IntV Int
    | BoolV Bool
    deriving (Show, Eq)

data ExprF a = 
    Constant Value
    | Variable Id
    | Plus a a
    | Minus a a
    | Multiply a a
    | Divide a a
    | And a a
    | Or a a
    deriving (Show, Eq, Functor)

data StatementF a =
    NoOp
    | VarDeclaration Type Id
    | VarAssignment Id Expr
    | Print Expr
    | If Expr a a
    | Compound [a]
    deriving (Show, Eq, Functor)

newtype Expr = Expr { runExpr :: Rec ExprF }
newtype Statement = Statement { runStatement :: Rec StatementF }
            
instance Eq (f (Rec f)) => Eq (Rec f) where
    (==) (In f) (In g) = f == g  

instance Show (f (Rec f)) => Show (Rec f) where
    show (In f) = show f

instance Eq Expr where
    (==) (Expr a) (Expr b) = a == b

instance Show Expr where
    show (Expr e) = show e

instance Eq Statement where
    (==) (Statement a) (Statement b) = a == b

instance Show Statement where
    show (Statement s) = show s

mkExpr :: ExprF (Rec ExprF) -> Expr
mkExpr = Expr . In

mkStatement :: StatementF (Rec StatementF) -> Statement
mkStatement = Statement . In

data EvalState =    
    EvalState { executionStack :: ExecutionStack,
                symbolTable :: SymbolTable, 
                outputStream :: OutputStream 
              }
    deriving (Show, Eq)

emptyState :: EvalState
emptyState = EvalState { executionStack = [], symbolTable = M.empty, outputStream = [] }

cata :: (Functor f) => Algebra f a -> Rec f -> a
cata f = f . fmap (cata f) . out

initState :: ExecutionStack -> EvalState
initState program = EvalState { executionStack = program, symbolTable = M.empty, outputStream = [] }

mkBool :: Bool -> Expr
mkBool = mkExpr . Constant . BoolV 

mkInt :: Int -> Expr
mkInt = mkExpr . Constant . IntV 

mkVar :: Id -> Expr
mkVar = mkExpr . Variable

eval :: Expr -> EvalReader 
eval (Expr (In (Constant x))) = pure $ Right x 
eval (Expr (In (Variable var))) = do
    state <- ask
    let val = lookupSymbol state var 
    case val of
        Just (Just x) -> pure $ Right $ x
        Just Nothing -> pure $ Left $ "Error: Variable " ++ var ++ " has not been initialized."
        Nothing -> pure $ Left $ "Error: Trying to use undeclared variable " ++ var ++ "."
    where
        lookupSymbol state var = M.lookup var $ symbolTable state
eval (Expr (In (Plus x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (IntV n1)) (Right (IntV n2)) = (Right (IntV (n1 + n2)))
eval (Expr (In (Minus x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (IntV n1)) (Right (IntV n2)) = (Right (IntV (n1 - n2)))
eval (Expr (In (Multiply x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (IntV n1)) (Right (IntV n2)) = (Right (IntV (n1 * n2)))
eval (Expr (In (Divide x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (IntV n1)) (Right (IntV n2)) = (Right (IntV (n1 `div` n2)))
eval (Expr (In (And x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (BoolV n1)) (Right (BoolV n2)) = (Right (BoolV (n1 && n2)))
eval (Expr (In (Or x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (BoolV n1)) (Right (BoolV n2)) = (Right (BoolV (n1 || n2)))

main :: IO ()
main = putStrLn "Hello World!"