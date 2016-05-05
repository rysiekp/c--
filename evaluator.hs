module Evaluator where
import Text.PrettyPrint.HughesPJClass
import LanguageStructs
import TypeChecker
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map
import Data.List as List
import Data.Maybe
import Data.IORef

type Scope = Map Name Var
type Funs = Map Name ([Arg], Statement)
type Env = (Scope, Funs)
type IntOp = (Integer -> Integer -> Integer)
type BoolOp = (Bool -> Bool -> Bool)
type Var = IORef Val
data Val
    = Int' Integer
    | Bool' Bool
    deriving (Eq, Ord)
instance Show Val where
    show (Int' i) = show i
    show (Bool' b) = show b
type Eval a = ExceptT String (StateT Env IO) a

runEval :: Eval a -> Env -> IO (Either String a)
runEval ev env = evalStateT (runExceptT ev) env

evalProgram :: Program -> Eval ()
evalProgram (Program funs main') = do
    env <- get
    let newEnv = List.foldr mapFunction env funs
    put newEnv
    evalStmt main'

mapFunction :: Function -> Env -> Env
mapFunction (Function name args s) (vars, funs) = 
    (vars, Map.insert name (args, s) funs)

inNewScope :: Eval a -> Eval a
inNewScope e = do
    oldEnv <- get
    ret <- e
    put oldEnv
    return ret

evalStmt :: Statement -> Eval ()
evalStmt (SSeq []) = do return ()
evalStmt (SSeq (s:ss)) = do
    evalStmt s
    evalStmt (SSeq ss)
evalStmt (SDeclare t s e) = do
    val <- evalExpr e
    newVar s val
evalStmt (SAssign s e) = do
    val <- evalExpr e
    t <- readVar s
    writeVar s val
evalStmt (SOpAss s PlusPlus) = evalStmt (SAssign s (EABinOp Plus (EVar s) (EIntLit 1)))
evalStmt (SOpAss s MinusMinus) = evalStmt (SAssign s (EABinOp Minus (EVar s) (EIntLit 1)))
evalStmt (SOpAss s (OpAss op e)) = evalStmt (SAssign s (EABinOp op (EVar s) e))
evalStmt (SIf c ifS elseS) = do
    Bool' cond <- evalExpr c
    if cond then
        inNewScope $ evalStmt ifS
    else
        when (isJust elseS) (inNewScope $ evalStmt $ fromJust elseS)
evalStmt (SWhile c s) =
    inNewScope $ while where
        while = do
            Bool' cond <- evalExpr c
            when cond (evalStmt s >> while)
evalStmt (SCall "print" args) = do
    if length args /= 1 then throwError "incorrect argument count in call to function print"
    else do
        env <- get
        let arg = evalExpr (head args)
        Right x <- liftIO $ runEval arg env
        liftIO $ print x
        return ()
evalStmt (SCall fun args) = do
    env@(vars, funs) <- get
    case Map.lookup fun funs of
        Nothing -> throwError $ "function " ++ fun ++ " not defined"
        Just (fargs, stmt) ->
            if length fargs /= length args then
                throwError $ "incorrect argument count in call to function " ++ fun
            else do
                funVars <- mapM createEnvEntry (zip fargs args)
                oldEnv <- get
                put (fromList funVars, funs)
                evalStmt stmt
                put oldEnv

createEnvEntry :: (Arg, Expr) -> Eval (Name, Var)
createEnvEntry ((EArg _ False aname), expr) = do
    env <- get
    e <- (evalExpr expr)
    val <- liftIO $ newIORef e
    return (aname, val)
createEnvEntry ((EArg _ True aname), (EVar name)) = do
    env <- get
    ref <- readRef name
    return (aname, ref)

evalExpr :: Expr -> Eval Val
evalExpr (EIntLit i) = return $ Int' i
evalExpr (EBoolLit b) = return $ Bool' b
evalExpr (EVar var) = do 
    v <- readVar var
    return v
evalExpr (EABinOp op a b) = do
    let op' = evalIntOp op
    Int' i1 <- evalExpr a
    Int' i2 <- evalExpr b
    return $ Int' $ op' i1 i2
evalExpr (ELBinOp op a b) = do
    let op' = evalBoolOp op
    Bool' b1 <- evalExpr a
    Bool' b2 <- evalExpr b
    return $ Bool' $ op' b1 b2
evalExpr e@(ERBinOp op a b) = do
    let op' = evalRelation op
    x1 <- evalExpr a
    x2 <- evalExpr b
    return $ Bool' $ op' x1 x2

newVar :: Name -> Val -> Eval ()
newVar name val = do
    (vars, funs) <- get
    val' <- liftIO $ newIORef val
    put $ (Map.insert name val' vars, funs)
    return ()

writeVar :: Name -> Val -> Eval ()
writeVar name val = do
    (vars, _) <- get
    let val' = searchScope vars name
    liftIO $ writeIORef  val' val

readRef :: Name -> Eval Var
readRef var = do
    (vars, _) <- get
    return $ searchScope vars var

readVar :: Name -> Eval Val
readVar var = do
    (vars, _) <- get
    liftIO $ readIORef $ searchScope vars var

searchScope :: Scope -> Name -> Var
searchScope s var =
    case Map.lookup var s of
        Just val -> val
        Nothing -> error $ "unbound variable " ++ var

evalIntOp :: ABinOp -> IntOp
evalIntOp Plus = (+)
evalIntOp Minus = (-)
evalIntOp Times = (*)
evalIntOp Div = div

evalBoolOp :: LBinOp -> BoolOp
evalBoolOp And = (&&)
evalBoolOp Or = (||)

evalRelation :: Ord a => RBinOp -> (a -> a -> Bool)
evalRelation Eq = (==)
evalRelation NEq = (/=)
evalRelation L = (<)
evalRelation LE = (<=)
evalRelation G = (>)
evalRelation GE = (>=)
 