module TypeChecker where
import LanguageStructs
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map as Map
import Data.List as List
import Data.IORef
import Text.PrettyPrint.HughesPJClass
import Data.Maybe

type Name = String
type CScope = Map Name Type
type CFuns = Map Name [Arg]
type CEnv = (CScope, CFuns)

type Check a = ExceptT String (StateT CEnv IO) a

runCheck :: Check a -> CEnv -> IO (Either String a)
runCheck check env = evalStateT (runExceptT check) env

checkProgram :: Program -> Check ()
checkProgram (Program funs main') = do
    env <- get
    put $ List.foldr mapFunction' env funs
    mapM checkFunction funs
    checkStmt main'

mapFunction' :: Function -> CEnv -> CEnv
mapFunction' (Function name args _) (vars, funs) = 
    (vars, Map.insert name args funs)

checkFunction :: Function -> Check ()
checkFunction (Function name args s) = do
    oldEnv <- get
    mapM addArgToEnv args
    checkStmt s
    put oldEnv
    return ()

addArgToEnv :: Arg -> Check ()
addArgToEnv (EArg t _ name) = do
    (vars, funs) <- get
    put $ (Map.insert name t vars, funs)
    return ()

checkStmt :: Statement -> Check ()
checkStmt (SSeq []) = return ()
checkStmt (SSeq (s:ss)) = do
    checkStmt s
    checkStmt (SSeq ss)
checkStmt s@(SDeclare t var e) = do
    rhs <- checkExpr e
    compareTypes rhs t (show $ pPrint s)
    insertVar var t
    return ()
checkStmt s@(SAssign var e) = do
    lhs <- checkExpr (EVar var)
    rhs <- checkExpr e
    compareTypes lhs rhs (show $ pPrint s)
    return ()
checkStmt (SWhile c s) = do
    ct <- checkExpr c
    compareTypes ct TBool ("while (" ++ (show $ pPrint c) ++ ")")
    checkStmt s
checkStmt (SIf c ifS elseS) = do
    ct <- checkExpr c
    compareTypes ct TBool ("if (" ++ (show $ pPrint c) ++ ")")
    checkStmt ifS
    when (isJust elseS) (checkStmt $ fromJust elseS)
checkStmt (SCall "print" args) = do
    (vars, _) <- get
    checkLengths args [1] "print"
checkStmt (SCall fun args) = do
    (vars, funs) <- get
    case Map.lookup fun funs of
        Nothing -> throwError $ "function " ++ fun ++ " not defined"
        Just fargs -> do
            checkLengths args fargs fun
            mapM (checkRef fun) (zip fargs args)
            return ()
checkStmt s@(SOpAss var (OpAss op e)) = do
    lhs <- checkExpr (EVar var)
    rhs <- checkExpr e
    compareTypes lhs rhs (show $ pPrint s)
    compareTypes lhs TInt (show $ pPrint s)
    return ()
checkStmt s@(SOpAss var _) = do
    lhs <- checkExpr (EVar var)
    compareTypes lhs TInt (show $ pPrint s)
    return ()

checkLengths :: [a] -> [b] -> String -> Check ()
checkLengths a b fun =
    if length a /= length b then
        throwError $ "incorrect argument count in function " ++ fun
    else return ()

checkRef :: String -> (Arg, Expr) -> Check ()
checkRef fun ((EArg at True _), ex@(EVar _)) = do
    et <- checkExpr ex
    compareTypes at et ("call to function " ++ fun)
    return ()
checkRef fun ((EArg _ True _), e) =
    throwError $ "cannot pass " ++ (show $ pPrint e) ++ " as reference"
checkRef _ ((EArg t _ _), e) = return ()

insertVar :: Name -> Type -> Check ()
insertVar n t = do
    (vars, funs) <- get
    put (Map.insert n t vars, funs)
    return ()

checkExpr :: Expr -> Check Type
checkExpr (EVar var) = do
    (vars, _) <- get
    case Map.lookup var vars of
        Just t -> return t
        Nothing -> throwError $ "variable " ++ var ++ " not bound"
checkExpr (EIntLit _) = return TInt
checkExpr (EBoolLit _) = return TBool
checkExpr e@(EABinOp op a b) = do
    ta <- checkExpr a
    tb <- checkExpr b
    compareTypes ta tb (show $ pPrint e)
    compareTypes ta TInt (show $ pPrint e)
checkExpr e@(ELBinOp op a b) = do
    ta <- checkExpr a
    tb <- checkExpr b
    compareTypes ta tb (show $ pPrint e)
    compareTypes ta TBool (show $ pPrint e)
checkExpr e@(ERBinOp op a b) = do
    ta <- checkExpr a
    tb <- checkExpr b
    compareTypes ta tb (show $ pPrint e)
    return TBool

compareTypes :: Type -> Type -> String -> Check Type
compareTypes TInt TInt _ = return TInt
compareTypes TBool TBool _ = return TBool
compareTypes t1 t2 err = 
    throwError $ "cannot match type " ++ 
                 (show $ pPrint t1) ++ 
                 " with " ++
                 (show $ pPrint t2) ++
                 " in " ++
                 err 
