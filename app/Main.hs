{-# LANGUAGE DeriveFunctor, TypeFamilies, LambdaCase, UndecidableInstances #-}

import Data.Map
import qualified Data.Map as M

newtype Rec f = In { out :: f (Rec f) }

type Algebra f a = f a -> a

type Id = String 

type ExecutionStack = [Statement]
type SymbolTable = Map Id (Maybe Value)
type OutputStream = [Value]

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
mkExpr expr = Expr $ In expr

mkStatement :: StatementF (Rec StatementF) -> Statement
mkStatement st = Statement $ In st

data EvalState =    
    EvalState { executionStack :: ExecutionStack,
                symbolTable :: SymbolTable, 
                outputStream :: OutputStream 
              }
    deriving (Show, Eq)

cata :: (Functor f) => Algebra f a -> Rec f -> a
cata f = f . fmap (cata f) . out

initState :: ExecutionStack -> EvalState
initState program = EvalState { executionStack = program, symbolTable = M.empty, outputStream = [] }

mkBool :: Bool -> Expr
mkBool val = mkExpr $ Constant $ BoolV val

mkInt :: Int -> Expr
mkInt val = mkExpr $ Constant $ IntV val

mkVar :: Id -> Expr
mkVar id = mkExpr $ Variable id

{-evalExpr :: ExprF (Maybe Value) -> Maybe Value
evalExpr expr = case expr of  
    (Constant x) -> Just x
    (Variable id) -> Nothing
    (Plus (Just x) (Just y)) -> Just $ x + y
    (Minus (Just x) (Just y)) -> Just $ x - y
    (Multiply (Just x) (Just y)) -> Just $ x * y
    (Divide (Just x) (Just y)) -> Just $ x / y 
    _ -> Nothing -}

test1 :: ExprF (Maybe Value)
test1 = Constant (IntV 5)

test2 :: ExprF (Maybe Value)
test2 = Constant Nothing

ex1 :: [Statement]
ex1 = mkStatement <$>
    [
        VarDeclaration BoolT "a",
        VarDeclaration IntT "v",
        VarAssignment "a" (mkBool True),
        If (mkVar "a") (In $ VarAssignment "v" (mkInt 2)) (In $ VarAssignment "v" (mkInt 3)),
        Print (mkVar "v")
    ]

main :: IO ()
main = putStrLn "Hello World!"