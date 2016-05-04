module Evaluator where
import LanguageStructs
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Data.IORef

type Name = String
type Scope = [(Name, Var)]
type Env = ([Scope], [(Name, ([Arg], Statement))])
type Eval a = (StateT Env IO) a
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


runEval :: Eval a -> Env -> IO a
runEval ev env = evalStateT ev env

execEval :: Eval a -> Env -> IO Env
execEval ev env = execStateT ev env

evalProgram :: Program -> Eval ()
evalProgram (Program funs main') = do
    env <- get
    let newEnv = foldr mapFunction env funs
    put newEnv
    evalStmt main'

newScope :: Eval ()
newScope = do
    (vars, funs) <- get
    put ([]:vars, funs)

removeScope :: Eval ()
removeScope = do
    (v:vs, funs) <- get
    put (vs, funs)

mapFunction :: Function -> Env -> Env
mapFunction (Function name args s) (vars, funs) =
    (vars, (name, (args, s)):funs)

evalStmt :: Statement -> Eval ()
evalStmt (SSeq []) = do return ()
evalStmt (SSeq (s:ss)) = do
    evalStmt s
    evalStmt (SSeq ss)
evalStmt (SDeclare _ s e) = do
    val <- evalExpr e
    newVar s val
evalStmt (SAssign s e) = do
    val <- evalExpr e
    writeVar s val
evalStmt (SIf c ifS elseS) = do
    Bool' cond <- evalExpr c
    if cond then do
        newScope
        evalStmt ifS
        removeScope
    else
        case elseS of
            Nothing -> return ()
            Just s -> do
                newScope
                evalStmt s
                removeScope
evalStmt while@(SWhile _ _) = do
    newScope
    whileStmt while
    removeScope
evalStmt (SCall "print" args) =
    if length args /= 1 then
        error "incorrect argument count" 
    else do
        env <- get
        let arg = evalExpr (head args)
        x <- lift $ runEval arg env
        lift $ print x
        return ()
evalStmt (SCall fun args) = do
    env@(vars, funs) <- get
    case lookup fun funs of
        Nothing -> error $ "function " ++ fun ++ " not defined"
        Just (fargs, stmt) -> 
            if length fargs /= length args then
                error "incorrect argument count"
            else do
                funVars <- mapM createEnvEntry (zip fargs args)
                lift $ runEval (evalStmt stmt) ([funVars], funs)

whileStmt:: Statement -> Eval ()
whileStmt while@(SWhile c s) = do
    Bool' cond <- evalExpr c
    if cond then do
        evalStmt s
        whileStmt while
    else do
        return ()

createEnvEntry :: (Arg, Expr) -> Eval (Name, Var)
createEnvEntry ((EArg _ False aname), expr) = do
    env@(vars, funs) <- get
    e <- lift $ runEval (evalExpr expr) env
    val <- lift $ newIORef e
    return $ (aname, val)
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
evalExpr (ERBinOp op a b) = do
    let op' = evalRelation op
    x1 <- evalExpr a
    x2 <- evalExpr b
    return $ Bool' $ op' x1 x2

newVar :: Name -> Val -> Eval ()
newVar name val = do
    ((v:vs), funs) <- get
    case lookup name v of
        Nothing -> do
            val' <- liftIO $ newIORef val
            put $ ((((name, val'):v):vs), funs)
            return ()
        Just _ -> error $ "variable " ++ name ++ " already bound"

writeVar :: Name -> Val -> Eval ()
writeVar name val = do
    (vars, _) <- get
    let val' = searchScopes vars name
    liftIO $ writeIORef  val' val

readRef :: Name -> Eval Var
readRef var = do
    (vars, _) <- get
    return $ searchScopes vars var

readVar :: Name -> Eval Val
readVar var = do
    (vars, _) <- get
    lift $ readIORef $ searchScopes vars var

searchScopes :: [Scope] -> Name -> Var
searchScopes (s:ss) var =
    case lookup var s of
        Just val -> val
        Nothing -> searchScopes ss var
searchScopes [] var = error $ "unbound variable " ++ var

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
 