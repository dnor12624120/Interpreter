{-# LANGUAGE DeriveFunctor, TypeFamilies, LambdaCase, UndecidableInstances #-}

import Data.Map
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as M

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

data EvalCode =
    SuccessC
    | ErrorC String
    deriving (Show, Eq)

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
    EvalState { 
                executionStack :: ExecutionStack,
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
        op (Right (IntV n1)) (Right (IntV n2)) = Right $ IntV $ n1 + n2
eval (Expr (In (Minus x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (IntV n1)) (Right (IntV n2)) = Right $ IntV $ n1 - n2
eval (Expr (In (Multiply x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (IntV n1)) (Right (IntV n2)) = Right $ IntV $ n1 * n2
eval (Expr (In (Divide x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (IntV n1)) (Right (IntV n2)) = Right $ IntV $ n1 `div` n2
eval (Expr (In (And x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (BoolV n1)) (Right (BoolV n2)) = Right $ BoolV $ n1 && n2
eval (Expr (In (Or x y))) = op <$> eval (Expr x) <*> eval (Expr y)
    where
        op (Right (BoolV n1)) (Right (BoolV n2)) = Right $ BoolV $ n1 || n2

exec :: Statement -> State EvalState EvalCode
exec (Statement (In (Compound list))) = do
    state <- get
    put (state { executionStack = (mkStatement NoOp) : (Statement <$> list) ++ (tail $ executionStack state) })
    return SuccessC
exec (Statement (In (VarDeclaration varType varId))) = do
    state <- get
    case M.lookup varId (symbolTable state) of 
        Nothing -> do
            put (state { symbolTable = M.insert varId Nothing symTable} )
            return $ SuccessC
            where 
                symTable = symbolTable state 
        _ -> return $ ErrorC $ "Error: Identifier " ++ varId ++ " already defined."
exec (Statement (In (VarAssignment varId expr))) = do
    state <- get
    case runReader (eval expr) state of 
        Left error -> return $ ErrorC $ error
        Right val -> do
            put (state { symbolTable = M.insert varId (Just val) symTable} )
            return $ SuccessC
            where 
                symTable = symbolTable state 
exec (Statement (In (Print expr))) = do
    state <- get
    case runReader (eval expr) state of
        Left error -> return $ ErrorC $ error
        Right val -> do
            put (state { outputStream = (outputStream state) ++ [val]} )
            return SuccessC
exec (Statement (In (If expr ifBranch elseBranch))) = do
    state <- get
    let cond = runReader (eval expr) state
    case cond of
        Left error -> return $ ErrorC $ error
        Right val -> do
            if val == BoolV True then
                exec $ Statement ifBranch
            else
                exec $ Statement elseBranch
exec (Statement (In (NoOp))) = return SuccessC

execAll :: EvalState -> String -> (String, EvalCode)
execAll state@(EvalState [] _ os) trace = (trace ++ show state, SuccessC)
execAll state@(EvalState (h:t) _ _) trace = 
    let (code, state'@(EvalState (h':t') _ _)) = runState (exec h) state
    in case code of
        ErrorC error -> ("", ErrorC error)
        SuccessC -> execAll (state' { executionStack = t'}) (trace ++ "\n" ++ show state)

test :: EvalState
test = EvalState { 
                    executionStack = [
                        mkStatement $ VarDeclaration BoolT "b",
                        mkStatement $ VarAssignment "b" (mkBool True),
                        mkStatement $ If (mkVar "b") 
                            (In $ Compound [
                                In $ Print (mkInt 1),
                                In $ Print (mkInt 2),
                                In $ Compound [
                                    In $ If (mkVar "b")
                                        (In $ Print (mkBool False))
                                        (In $ NoOp)
                                ]
                            ])
                            (In $ Print (mkInt 200)),
                        mkStatement $ Print (mkInt 3)
                        ],
                    symbolTable = M.empty,
                    outputStream = [] 
                  }

main :: IO ()
main = do
    let (trace, _) = execAll test ""
    writeFile "log.txt" trace
    return ()
