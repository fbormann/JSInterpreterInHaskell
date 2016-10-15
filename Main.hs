import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty, delete, fromList)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
-- NumLit
evalExpr env (NumLit d) = return $ Double d
-- VarRef
evalExpr env (VarRef (Id id)) = stateLookup env id
-- IntLit
evalExpr env (IntLit int) = return $ Int int
-- BoolLit
evalExpr env (BoolLit bool) = return $ Bool bool
-- StringLit
evalExpr env (StringLit str) = return $ String str
-- InfixExpr
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    case v1 of
        (Return r1) -> case v2 of
			(Return r2) -> infixOp env op r1 r2
			_ -> infixOp env op r1 v2
	_ -> case v2 of
		(Return r2) -> infixOp env op v1 r2
		_ -> infixOp env op v1 v2 
		-- we need this specification 
		-- in order to do recursive functions, for example
-- AssignExpr
evalExpr env (AssignExpr OpAssign var expr) = do
    case var of
        (LVar v) -> do -- x = something (x is already declared)
            tent <- stateLookup env v
            e <- evalExpr env expr
            case tent of
                GlobalVar -> createGlobalVar v e
                _ -> setVar v e
        (LBracket expr1 expr2) -> do -- x[n] = something (x is already declared)
            case expr1 of
                VarRef (Id id) -> do
                    evaluedI <- stateLookup env id
                    pos <- evalExpr env expr2
                    e <- evalExpr env expr
                    case evaluedI of
                        Array l -> do
                            newArray <- setVarArray (Array []) (Array l) pos e
                            setVar id newArray
                        _ -> error $ "Sorry, this variable is not an array"
-- ArrayLit
evalExpr env (ArrayLit []) = return (Array [])
evalExpr env (ArrayLit l) = evalArray env l (Array [])
-- ListExpr
evalExpr env (ListExpr []) = return Nil
evalExpr env (ListExpr (l:ls)) = do
    evalExpr env l >> evalExpr env (ListExpr ls)
-- DotRef
evalExpr env (DotRef expr (Id id)) = do
    v1 <- evalExpr env expr
    case v1 of
        Array l -> do
            case id of
                "len" -> return (myLength 0 l)
                "head" -> return (head l)
                "tail" -> return (Array (tail l))
                _ -> error $ "Sorry, this function is not defined"
-- BracketRef
evalExpr env (BracketRef expr1 expr2) = do -- x[n]
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    evalNthElement env v1 v2
-- PrefixExpr
evalExpr env (PrefixExpr op expr) = do
    v <- evalExpr env expr
    prefixOp env op v
-- CondExpr
evalExpr env (CondExpr expr1 expr2 expr3) = do
    v1 <- evalExpr env expr1
    if (v1 == (Bool True)) then
	evalExpr env expr2
    else if (v1 == (Bool False)) then
	evalExpr env expr3
    else
	return Nil
-- UnaryAssignExpr
evalExpr env (UnaryAssignExpr i (LVar v)) = do
    case i of
	PrefixInc -> evalExpr env (AssignExpr OpAssign (LVar v) (InfixExpr OpAdd (VarRef (Id v)) (IntLit 1)))
	PrefixDec -> evalExpr env (AssignExpr OpAssign (LVar v) (InfixExpr OpSub (VarRef (Id v)) (IntLit 1)))
	PostfixInc -> evalExpr env (AssignExpr OpAssign (LVar v) (InfixExpr OpAdd (VarRef (Id v)) (IntLit 1)))
	PostfixDec -> evalExpr env (AssignExpr OpAssign (LVar v) (InfixExpr OpSub (VarRef (Id v)) (IntLit 1)))
-- check why this is not running ( ++ x[n] )
--evalExpr env (UnaryAssignExpr inc (LBracket expr1 expr2) ) = do
--    case expr1 of
--        VarRef (Id id) -> do
--            case inc of
--		PrefixInc -> evalExpr env (AssignExpr OpAssign (LBracket VarRef (Id id) expr2) (InfixExpr OpAdd (VarRef (Id id)) (IntLit 1)))
--		PrefixDec -> evalExpr env (AssignExpr OpAssign (LBracket VarRef (Id id) expr2) (InfixExpr OpSub (VarRef (Id id)) (IntLit 1)))
--		PostfixInc -> evalExpr env (AssignExpr OpAssign (LBracket VarRef (Id id) expr2) (InfixExpr OpAdd (VarRef (Id id)) (IntLit 1)))
--		PostfixDec -> evalExpr env (AssignExpr OpAssign (LBracket VarRef (Id id) expr2) (InfixExpr OpSub (VarRef (Id id)) (IntLit 1)))
	    
-- CallExpr	
evalExpr env (CallExpr func params) = do
    case func of
        DotRef expr (Id id) -> do
            v1 <- evalExpr env expr
            case v1 of
                Array l -> do
		    if (id == "concat") then
			myConcat env l params
		    else if (id == "len") then
			return (myLength 0 l)
		    else if (id == "head") then
			return (head l)
		    else if (id == "tail") then
			return (Array(tail l))
		    else if (id == "equals") then
			myEquals env l params
		    else 
			error $ "Sorry, this function is not defined"
        _ -> do
            evaluedName <- evalExpr env func
            case evaluedName of
                Function id args stmt -> do
                    pushScope
                    evalArgs env args params
                    ret <- evalStmt env (BlockStmt stmt)
                    popScope
                    case ret of
                        Return r -> return r
                        _ -> return Nil

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (x:xs) ) = do
    v1 <- evalStmt env x
    if (v1 == (Break) || v1 == (Continue)) then
    	return v1
    else
	case v1 of
	Return r1 -> return (Return r1)
	_ -> evalStmt env (BlockStmt xs)
evalStmt env (IfStmt cond c1 c2) = do
    v1 <- evalExpr env cond
    if (v1 == (Bool True)) then
	evalStmt env c1
    else if (v1 == (Bool False)) then
	evalStmt env c2
    else return Nil
evalStmt env (IfSingleStmt cond c1) = do
    v1 <- evalExpr env cond
    if (v1 == (Bool True)) then
	evalStmt env c1
    else return Nil
evalStmt env (WhileStmt cond c1) = do
    v1 <- evalExpr env cond
    if (v1 == (Bool True)) then do
	v2 <- evalStmt env c1
	if (v2 == (Break)) then
	    return Nil
	else
	    case v2 of
	    Return r -> return (Return r)
            _ -> evalStmt env (WhileStmt cond c1)
    else 
	return Nil				
evalStmt env (DoWhileStmt c1 cond) = do
    v1 <- evalStmt env c1
    case v1 of
	Break -> return Nil
	Return r -> return (Return r)
	_ -> do
		v2 <- evalExpr env cond
		if (v2 == (Bool True)) then
	    	    evalStmt env (DoWhileStmt c1 cond)
        	else 
            	    return Nil
	
evalStmt env (ReturnStmt i) = 
    case i of
        Nothing -> return (Return Nil)
        Just v -> do
            v1 <- evalExpr env v
            return (Return v1)
evalStmt env (SwitchStmt expr []) = return Nil
evalStmt env (SwitchStmt expr (x:xs)) = 
    case x of
	(CaseClause expr1 c) -> do
	    v1 <- evalExpr env expr
	    v2 <- evalExpr env expr1
	    if (v1 == v2) then
	        evalStmt env (BlockStmt c)
	    else 
		evalStmt env (SwitchStmt expr xs)
        (CaseDefault c) -> evalStmt env (BlockStmt c)
evalStmt env (ForStmt initExpression cond afterblock block) = do
    evalForInit env initExpression
    case cond of
        Nothing -> return (Return Nil)
        (Just  expr) -> do
            checkCond <- evalExpr env expr
            if (checkCond == (Bool True)) then do 
                evalStmt env block
                case afterblock of
                    Nothing -> return (Return Nil)
                    (Just expr) -> do
                        evalExpr env expr
                        evalStmt env (ForStmt NoInit cond afterblock block)
            else 
                return Nil
evalStmt env (FunctionStmt (Id name) args stmts) = createGlobalVar name (Function (Id name) args stmts)		
evalStmt env (BreakStmt i) = return Break
evalStmt env (ContinueStmt i) = return Continue
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr


-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpNEq  (Int v1) (Int v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

prefixOp :: StateT-> PrefixOp -> Value -> StateTransformer Value
prefixOp env PrefixLNot (Bool v) = return $ Bool $ not v
prefixOp env PrefixMinus (Int v) = return $ Int $ (-v)
prefixOp env PrefixMinus (Double v) = return $ Double $ (-v)
prefixOp env PrefixPlus (Int v) = return $ Int v
prefixOp env PrefixPlus (Double v) = return $ Double v

unaryAssignOp :: StateT -> UnaryAssignOp -> Value -> StateTransformer Value
unaryAssignOp env PostfixInc (Int v1) = return $ Int $ v1 + 1
unaryAssignOp env PostfixInc (Double v1) = return $ Double $ v1 + 1
unaryAssignOp env PostfixDec (Int v1) = return $ Int $ v1 - 1
unaryAssignOp env PostfixDec (Double v1) = return $ Double $ v1 - 1
unaryAssignOp env PrefixInc (Int v1) = return $ Int $ v1 + 1
unaryAssignOp env PrefixInc (Double v1) = return $ Double $ v1 + 1
unaryAssignOp env PrefixDec (Int v1) = return $ Int $ v1 - 1
unaryAssignOp env PrefixDec (Double v1) = return $ Double $ v1 - 1
--
-- Environment and auxiliary functions
--

environment :: StateT
environment = [Map.empty]

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    case scopeLookup s var of
        Nothing -> (GlobalVar, s)
        Just v -> (v, s)

scopeLookup :: StateT -> String -> Maybe Value
scopeLookup [] _ = Nothing
scopeLookup (x:xs) var =
    case Map.lookup var x of
        Nothing -> scopeLookup xs var
        Just v -> Just v

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> createLocalVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            createLocalVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, (searchAndUpdateVar var val s))

searchAndUpdateVar :: String -> Value -> StateT -> StateT
searchAndUpdateVar _ _ [] = error $ "Unreachable error"
searchAndUpdateVar var val stt = case (Map.lookup var (head stt)) of
        Nothing -> (head stt):(searchAndUpdateVar var val (tail stt))
        Just v -> (insert var val (head stt)):(tail stt)

createGlobalVar :: String -> Value -> StateTransformer Value
createGlobalVar var val = ST $ \s -> (val, createGlobalVarAux var val s)
 
createGlobalVarAux :: String -> Value -> StateT -> StateT
createGlobalVarAux var val (s:scopes) = 
    if (scopes == []) 
        then (insert var val s):[] 
    else s:(createGlobalVarAux var val scopes)

pushScope :: StateTransformer Value
pushScope = ST $ \s -> (Nil, (Map.empty):s)

popScope :: StateTransformer Value
popScope = ST $ \s -> (Nil, (tail s))

createLocalVar :: String -> Value -> StateTransformer Value
createLocalVar var val = ST $ \s -> (val, (insert var val (head s)):(tail s))



evalArray :: StateT -> [Expression] -> Value -> StateTransformer Value
evalArray env [] (Array l) = return (Array l)
evalArray env (x:xs) (Array l) = do
    v1 <- evalExpr env x
    evalArray env xs (Array (l++[v1]))

myLength :: Int -> [Value] -> Value
myLength x [] = Int x
myLength x (b:bs) = myLength (x+1) bs

myHead :: [Value] -> Value
myHead [] = Nil
myHead (x:xs) = x

myTail :: [Value] -> [Value]
myTail [] = []
myTail (x:xs) = xs

myConcat :: StateT -> [Value] -> [Expression] -> StateTransformer Value
myConcat env l [] = return (Array l)
myConcat env l (param:params) = do
    v1 <- evalExpr env param
    case v1 of
        (Array l2) -> myConcat env (l ++ l2) params
        v -> myConcat env (l ++ [v]) params

evalNthElement :: StateT -> Value -> Value -> StateTransformer Value
evalNthElement env (Array []) (Int n) = return Nil
evalNthElement env (Array (x:xs)) (Int 0) = return x
evalNthElement env (Array (x:xs)) (Int n) = do
    evalNthElement env (Array xs) (Int (n-1))

evalForInit :: StateT -> ForInit -> StateTransformer Value
evalForInit env (NoInit) = return Nil
evalForInit env (VarInit []) = return Nil
evalForInit env (VarInit (x:xs)) = do
    varDecl env x >> evalForInit env (VarInit xs)

evalForInit env (ExprInit expr) = do
    evalExpr env expr

evalArgs :: StateT-> [Id]-> [Expression]-> StateTransformer Value
evalArgs env [] [] = return Nil
evalArgs env ((Id id):ids) (param:params) =  do
        v <- evalExpr env param
        createLocalVar id v
        evalArgs env ids params
evalArgs env _ _ = error $ "Inconsistency between input and arguments"

myEquals :: StateT -> [Value] -> [Expression] -> StateTransformer Value
myEquals env l [] = return (Bool True)
myEquals env l (expr:exprs) = do
    evaluedExpr <- evalExpr env expr
    case evaluedExpr of
        (Array l2) -> do
            if (myEqualsArray l l2)
                then myEquals env l exprs
            else return (Bool False)

myEqualsArray :: [Value] -> [Value] -> Bool
myEqualsArray [] [] = True
myEqualsArray x [] = False
myEqualsArray [] y = False
myEqualsArray (x:xs) (y:ys) = (x == y) && (myEqualsArray xs ys)

setVarArray :: Value -> Value -> Value -> Value -> StateTransformer Value
setVarArray (Array x) (Array []) (Int 0) e = return (Array (x ++ [e]))
setVarArray (Array x) (Array []) (Int n) e = setVarArray (Array (x ++ [])) (Array []) (Int (n-1)) e
setVarArray (Array x) (Array (l:ls)) (Int 0) e = return (Array (x ++ [e] ++ ls))
setVarArray (Array x) (Array (l:ls)) (Int n) e = setVarArray (Array (x ++ [l])) (Array ls) (Int (n-1)) e
--
-- Types and boilerplate
--

type StateT = [Map String Value]
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, []) = ""
showResult (val, (s:scopes)) =
    show val ++ "\n" ++ show (toList $ union s (Map.empty)) ++ "\n" ++ showResult (val, scopes)

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f [Map.empty]

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
