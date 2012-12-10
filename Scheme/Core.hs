module Scheme.Core where

import Control.Applicative ((<$>))
import Data.List (intercalate)

import Control.Monad.Trans.Class (lift)
import Control.Monad.State (State, runStateT, get, put)
import Control.Monad.Error.Class (Error, strMsg)
import Control.Monad.Error (ErrorT, runErrorT, throwError, catchError)
import Control.Monad.Identity (runIdentity)

import qualified Data.Map as M

import Text.Printf (printf)


data CompError = EvalError String | ThrownError String | StackOverflow Int

instance Show CompError where
    show (EvalError m) = m
    show (ThrownError m) = m
    show (StackOverflow s) = printf "StackOverflow: %d calls" s

instance Error CompError where
    strMsg = EvalError


type Env = (M.Map String Expr, Int, Int)
type ErrorState s a = ErrorT CompError (State s) a
type Computation a = ErrorState Env a
type Evaluation = Computation Expr
type Builtin = [Expr] -> Evaluation


class Show f => Function f where
    apply :: f -> [Expr] -> Evaluation
    apply' :: f -> [Expr] -> Evaluation
    apply' f args = (apply f args) `catchError` insertName
        where insertName (EvalError msg) = throwError $
                EvalError $ printf "Error while applying \
                                \function %s:\n%s" (show f) msg
              insertName e = throwError e


run :: Computation a -> Env -> (Either CompError a, Env)
run comp env@(eMap, stackLimit, stackSize) =
    if stackLimit == stackSize
        then (Left $ StackOverflow stackSize, env)
        else runIdentity $ runStateT (runErrorT comp)
                            (eMap, stackLimit, stackSize + 1)

putResult :: Either CompError a -> Computation a
putResult = either throwError (lift . return)

newEnv :: M.Map String Expr -> Int -> Env
newEnv builtins stackLimit = (builtins, stackLimit, 0)

getEnv :: Computation Env
getEnv = lift get

putEnv :: Env -> Computation ()
putEnv = lift . put

keepEnv :: Computation a -> Computation a
keepEnv comp = do
    env <- getEnv
    let (res, _) = run comp env
    putResult res

envLookup :: String -> Evaluation
envLookup = \k -> do
    (env, _, _) <- getEnv
    let envStr = "(" ++ (intercalate " " $ M.keys env) ++ ")"
        failWithMsg = throwError $
            EvalError $ printf "No function bound to name \
                            \%s, current env:\n%s" k envStr
    maybe failWithMsg return $ M.lookup k env

insertEnv :: String -> Expr -> Env -> Env
insertEnv k v (env, stackLimit, stackSize) =
    (M.insert k v env, stackLimit, stackSize)

printEnv :: Builtin
printEnv = \args -> do
    checkArgs 0 args
    (env, _, _) <- getEnv
    return . ListExpr $ IdentifierExpr <$> M.keys env

check :: Bool -> String -> Computation ()
check p errorMsg = if p then return () else throwError $ EvalError errorMsg

checkArgs c args =
    let al = length args
    in if al == c then return args else throwError $
        EvalError $ printf "function accepts %d args, \
                \%d given: %s" c al $ intercalate ", " $ show <$> args


data Expr =
    QuotedExpr {getExpr :: Expr} |
    ListExpr [Expr] |
    IdentifierExpr {getIdentifier :: String} |
    FuncExpr {getFunc :: Func} |
    IntegerExpr {getInteger :: Integer} |
    DoubleExpr {getDouble :: Double} |
    StringExpr String |
    BoolExpr {getBool :: Bool} |
    Void

getList (ListExpr es) = es
getList (QuotedExpr (ListExpr es)) = QuotedExpr <$> es

getNumericValue :: Expr -> Double
getNumericValue (IntegerExpr i) = realToFrac i
getNumericValue (DoubleExpr d) = realToFrac d

exprOrd (QuotedExpr e) = exprOrd e
exprOrd (ListExpr _) = 1
exprOrd (IdentifierExpr _) = 2
exprOrd (FuncExpr _) = 3
exprOrd (IntegerExpr _) = 4
exprOrd (DoubleExpr _) = 5
exprOrd (StringExpr _) = 6
exprOrd (BoolExpr _) = 7
exprOrd Void = 8

isList = (== 1) . exprOrd
isIdentifier = (== 2) . exprOrd
isFunc = (== 3) . exprOrd
isInteger = (== 4) . exprOrd
isDouble = (== 5) . exprOrd
isBool = (== 7) . exprOrd

isQuotable = (<= 2) . exprOrd
isNumeric = (`elem` [4,5]) . exprOrd
isOrderable = (`elem` [4..7]) . exprOrd

allOfOneType es = all (== exprOrd (head es)) $ exprOrd <$> es

instance Eq Expr where
    (QuotedExpr e1) == (QuotedExpr e2) = e1 == e2
    (ListExpr es1) == (ListExpr es2) = es1 == es2
    (IdentifierExpr i1) == (IdentifierExpr i2) = i1 == i2
    (FuncExpr f1) == (FuncExpr f2) = getName f1 == getName f2
    (StringExpr s1) == (StringExpr s2) = s1 == s2
    (BoolExpr b1) == (BoolExpr b2) = b1 == b2
    Void == Void = True
    e1 == e2 | all isNumeric [e1, e2] = getNumericValue e1 == getNumericValue e2
    _ == _ = False

instance Ord Expr where
    (BoolExpr b1) <= (BoolExpr b2) = b1 <= b2
    (StringExpr s1) <= (StringExpr s2) = s1 <= s2
    e1 <= e2 | all isNumeric [e1, e2] = getNumericValue e1 <= getNumericValue e2

instance Show Expr where
    show (QuotedExpr e)
        | isQuotable e      = "'" ++ show e
        | otherwise         = show e
    show (ListExpr es)      = "(" ++ (intercalate " " $ map show es) ++ ")"
    show (IdentifierExpr i) = i
    show (FuncExpr f)       = "#<function:" ++ (getName f) ++ ">"
    show (IntegerExpr i)    = show i
    show (DoubleExpr d)     = show d
    show (StringExpr s)     = show s
    show (BoolExpr v)       = if v then "#t" else "#f"
    show (Void)             = "#<void>"

instance Function Expr where
    apply (FuncExpr f) args = apply f args
    apply e args = checkArgs 0 args >> return e


data Func =
    Func {getName :: String, getArgs :: [String], getBody :: Expr} |
    VarargFunc {getName :: String, getArg :: String, getBody :: Expr} |
    BuiltinFunc {getName :: String, getBuiltin :: Builtin}

instance Show Func where
    show = getName

instance Function Func where 
    apply (Func _ argNames bodyExpr) args = keepEnv $ do
        checkArgs (length argNames) args
        env <- getEnv
        eArgs <- evalArgs args
        putEnv $ foldl (\e (k, v) -> insertEnv k v e) env $ zip argNames eArgs
        eval bodyExpr

    apply (VarargFunc name argName bodyExpr) args =
        apply (Func name [argName] bodyExpr) [ListExpr args]
    apply (BuiltinFunc _ bf) args = bf args


eval :: Expr -> Evaluation
eval (QuotedExpr e)       = return e
eval (ListExpr (fn:args)) = do {f <- eval fn; apply' f args;}
eval (ListExpr empty)     = return Void
eval (IdentifierExpr i)   = envLookup i
eval simpleExpr           = return simpleExpr

evalArgs :: [Expr] -> Computation [Expr]
evalArgs = sequence . map eval