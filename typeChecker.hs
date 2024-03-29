module TypeChecker where
import LanguageStructs
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Map as Map
import Data.List as List
import Text.PrettyPrint.HughesPJClass
import Data.Foldable

type Name = String
type CScope = Map Name (Type, Bool)
type CFuns = Map Name [Arg]
type CEnv = (CScope, CFuns)

type Check a = ExceptT String (StateT CEnv IO) a

runCheck :: Check a -> CEnv -> IO (Either String a)
runCheck check = evalStateT (runExceptT check)

checkProgram :: Program -> Check ()
checkProgram (Program funs main') = do
    env <- get
    put $ List.foldr mapFunction' env funs
    mapM_ checkFunction funs
    checkStmt main'

mapFunction' :: Function -> CEnv -> CEnv
mapFunction' (Function name args _) (vars, funs) = 
    (vars, Map.insert name args funs)

checkFunction :: Function -> Check ()
checkFunction (Function _ args s) = do
    oldEnv <- get
    mapM_ addArgToEnv args
    checkStmt s
    put oldEnv
    return ()

addArgToEnv :: Arg -> Check ()
addArgToEnv (EArg t _ name) = do
    (vars, funs) <- get
    put (Map.insert name (t, False) vars, funs)
    return ()

inNewScope' :: Check a -> Check a
inNewScope' c = do
    oldEnv@(vars, funs) <- get
    put (Map.map makeGlobal vars, funs)
    ret <- c
    put oldEnv
    return ret

makeGlobal :: (Type, Bool) -> (Type, Bool)
makeGlobal (t, _) = (t, True)

checkStmt :: Statement -> Check ()
checkStmt (SSeq []) = return ()
checkStmt (SSeq (s:ss)) = do
    checkStmt s
    checkStmt (SSeq ss)
checkStmt s@(SDeclare t var e) = do
    rhs <- checkExpr e
    _ <- compareTypes rhs t (show $ pPrint s)
    createVar var t
    return ()
checkStmt s@(SAssign var e) = do
    lhs <- checkExpr (EVar var)
    rhs <- checkExpr e
    _ <- compareTypes lhs rhs (show $ pPrint s)
    return ()
checkStmt (SWhile c s) = do
    ct <- checkExpr c
    _ <- compareTypes ct TBool ("while (" ++ show (pPrint c) ++ ")")
    inNewScope' $ checkStmt s
checkStmt (SIf c ifS elseS) = do
    ct <- checkExpr c
    _ <- compareTypes ct TBool ("if (" ++ show (pPrint c) ++ ")")
    inNewScope' $ checkStmt ifS
    Data.Foldable.forM_ elseS checkStmt
checkStmt (SCall "print" (a:as)) = do
    checkLengths (a:as) [a] "print"
    _ <- checkExpr a
    return ()
checkStmt (SCall fun args) = do
    (_, funs) <- get
    case Map.lookup fun funs of
        Nothing -> throwError $ "function " ++ fun ++ " not defined"
        Just fargs -> do
            checkLengths args fargs fun
            mapM_ (checkRef fun) (zip fargs args)
            return ()
checkStmt s@(SOpAss var (OpAss _ e)) = do
    lhs <- checkExpr (EVar var)
    checkStmt (SAssign var e)
    _ <- compareTypes lhs TInt (show $ pPrint s)
    return ()
checkStmt s@(SOpAss var _) = do
    lhs <- checkExpr (EVar var)
    _ <- compareTypes lhs TInt (show $ pPrint s)
    return ()

checkLengths :: [a] -> [b] -> String -> Check ()
checkLengths a b fun =
    when (length a /= length b) $
        throwError $ "incorrect argument count in call to function " ++ fun

checkRef :: String -> (Arg, Expr) -> Check ()
checkRef fun (EArg at True _, ex@(EVar _)) = do
    et <- checkExpr ex
    _ <- compareTypes at et ("call to function " ++ fun)
    return ()
checkRef _ (EArg _ True _, e) =
    throwError $ "cannot pass " ++ show (pPrint e) ++ " as reference"
checkRef _ _ = return ()

createVar :: Name -> Type -> Check ()
createVar n t = do
    (vars, funs) <- get
    case Map.lookup n vars of
        Just (_, l) -> 
            if not l then
                throwError $ "variable " ++ n ++ " already bound"
            else do
                put (Map.insert n (t, False) vars, funs)
                return ()
        _ -> do
            put (Map.insert n (t, False) vars, funs)
            return ()

checkExpr :: Expr -> Check Type
checkExpr (EVar var) = do
    (vars, _) <- get
    case Map.lookup var vars of
        Just (t, _) -> return t
        Nothing -> throwError $ "variable " ++ var ++ " not bound"
checkExpr (EIntLit _) = return TInt
checkExpr (EBoolLit _) = return TBool
checkExpr e@(EABinOp _ a b) = do
    te <- checkBinOp e a b
    compareTypes te TInt (show $ pPrint e)
checkExpr e@(ELBinOp _ a b) = do
    te <- checkBinOp e a b
    compareTypes te TBool (show $ pPrint e)
checkExpr e@(ERBinOp _ a b) = do
    _ <- checkBinOp e a b
    return TBool

checkBinOp :: Expr -> Expr -> Expr -> Check Type
checkBinOp e a b = do
    ta <- checkExpr a
    tb <- checkExpr b
    _ <- compareTypes ta tb (show $ pPrint e)
    return ta

compareTypes :: Type -> Type -> String -> Check Type
compareTypes TInt TInt _ = return TInt
compareTypes TBool TBool _ = return TBool
compareTypes t1 t2 err = 
    throwError $ "cannot match type " ++ 
                 show (pPrint t1) ++ 
                 " with " ++
                 show (pPrint t2) ++
                 " in " ++
                 err 
