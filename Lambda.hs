module Lambda where

import Expr
import Data.List

-- TODO 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars e = case e of
    Variable x -> [x]
    Function x expr -> (filter (/=x) (free_vars expr))
    Application expr1 expr2 -> nub (free_vars expr1 ++ free_vars expr2) 

-- TODO 1.2. reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce e_1 x e_2 = case e_1 of 
    Variable v -> if (v == x)
        then e_2
        else e_1
    Function v expr -> if (v == x) then e_1
        else 
            if (v `elem` free_vars e_2)
                then Function "a" $ rename "a" v $ reduce expr x e_2
                else Function v $ reduce expr x $ e_2
    Application expr1 expr2 -> Application (reduce expr1 x $ e_2) (reduce expr2 x $ e_2)
    where
        rename v' v expr = case expr of 
            Variable x -> if (x == v) then Variable v' else expr
            Function x expr' -> if (x == v) then Function v' $ rename v' v expr else expr
            Application expr1 expr2 -> Application (rename v' v expr1) (rename v' v expr2)

-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN expr = case expr of
    Application expr1 expr2 -> case expr1 of 
        Variable _ -> Application expr1 $ stepN $ expr2
        Function x expr' -> reduce expr' x expr2
        _ -> Application (stepN expr1) expr2
    Function x expr1 -> Function x $ stepN $ expr1
    _ -> expr

-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN expr = if (stepN expr == expr)
    then expr
    else reduceN $ stepN expr
    

reduceAllN :: Expr -> [Expr]
reduceAllN expr = if (stepN expr == expr)
    then [expr]
    else expr : (reduceAllN $ stepN $ expr)

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA expr = case expr of 
    Application expr1 expr2 -> case expr1 of
        Variable _ -> Application expr1 $ stepA expr2
        Function x expr' -> case expr2 of
            Application expr1' expr2' -> Application expr1 $ stepA expr2
            _ -> reduce expr' x expr2
        _ -> Application (stepA expr1) $ expr2
    Function x expr -> Function x $ stepA $ expr
    _ -> expr

-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA expr = if (stepA expr == expr)
    then expr
    else reduceA $ stepA expr

reduceAllA :: Expr -> [Expr]
reduceAllA expr = if (stepA expr == expr)
    then [expr]
    else expr : (reduceAllA $ stepA expr)

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros = undefined

-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode = undefined
