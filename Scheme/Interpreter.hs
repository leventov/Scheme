module Scheme.Interpreter (greeting, builtins, runExprs) where

import Scheme.Parser
import Scheme.Core
import Scheme.Library

import Control.Applicative ((<$>))
import Data.List (intercalate)

import Control.Monad.Error (catchError)

import qualified Data.Map as M

builtins =
    let bi n b = (n, FuncExpr $ BuiltinFunc n b)
    in M.fromList [
        bi "env" printEnv,

        bi "error" errorBuiltin,
        bi "define" define,
        bi "if" ifBuiltin,

        bi "lambda" lambda,
        bi "begin" begin,
        bi "apply" (funcListBuiltin apply'),
        bi "map" mapBuiltin,
        bi "list" list,
        bi "quote" quote,

        bi "range" range,
        bi "cons" cons,
        bi "head" (listBuiltin head),
        bi "tail" (listBuiltin ListExpr . tail),
        bi "init" (listBuiltin ListExpr . init),
        bi "last" (listBuiltin last),

        bi "+" (simpleArith (+)),
        bi "*" (simpleArith (*)),
        bi "-" (simpleArith (-)),
        bi "/" division,

        bi "==" eq,
        bi "<" (orderBuiltin (<)),
        bi "<=" (orderBuiltin (<=)),
        bi ">" (orderBuiltin (>)),
        bi ">=" (orderBuiltin (>=)),

        bi "and" (boolBuiltin and),
        bi "or" (boolBuiltin or)]

runExpr :: Expr -> Evaluation
runExpr e = (eval e) `catchError` (return . IdentifierExpr . show)

runExprs :: Env -> [Expr] -> (String, Env)
runExprs env es =
    let comp = sequence $ runExpr <$> es
        showRight rs = intercalate "\n" $ show <$> rs
        (result, resultEnv) = run comp env
    in (either show showRight result, resultEnv)

mainLoop :: Env -> [Expr] -> String
mainLoop e es = fst $ runExprs e es

greeting = "Scheme interpreter. Type `(env)` to print available functions."

defaultStackLimit = 2^20

main = do
    putStrLn greeting
    interact $ \s -> (either show (mainLoop $
        newEnv builtins defaultStackLimit) $ parseExprs s) ++ "\n"
