import LispVal
import Parser

evalFile :: T.Text -> IO()
evalFile fileExpr = (runASTinEnv basicEnv $ fileToEvalForm fileExpr) 
                        >>= print

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show)
                                evalBody
                                $ readExprFile input

runParseTest :: T.Text -> T.Text -- for view AST
runParseTest input = either (T.pack . show)
                            (T.pack . show)
                            $ readExpr input

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runResourceT 
                          $ runReaderT (unEval action) code

eval :: LispVal -> Eval LispVal
eval (List [Atom "quote", val]) = return val
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b) = return $ Bool b
eval (List []) = return Nil
eval Nil = return Nil

eval (List [Atom "write", rest]) =
        return . String . T.pack $ show rest

eval (List ((:) (Atom "write") rest)) = 
        return . String . T.pack . show $ List rest

eval n@(Atom _) = getVar n

getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
        env <- ask
        case Map.lookup atom env of
                Just x -> return x
                Nothing -> throe $ UnboundVar atom

eval (List [Atom "if", pred, truExpr, flsExpr]) = do
        ifRes <- eval pred 
        case ifRes of
                (Bool True) -> eval truExpr
                (Bool False) -> eval flsExpr
                _ -> throw $ BadSpecialForm "if"

eval (List [Atom "let", List pairs, expr]) = do
        env <- ask
        atoms <- mapM ensureAtom $ getEven pairs
        vals <- mapM eval $ getOdd pairs
        let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals) <> env
        in local (const env') $ evalBody expr

getEven :: [a] -> [a]
getEven [] = []
getEven(x:xs) = x : getOdd xs

getOdd :: [a] -> [a]
getOdd [] = []
getOdd (x:xs) = getEven xs

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = throw $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom

eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest)) = evalBody $ List rest

eval (List [Atom "define", varExpr, expr]) = do
    varAtom <- ensureAtom varExpr
    evalVal <- eval expr
    env <- ask
    let envFn = const $ Map.insert (extractVar varAtom) evalVal env
    in local envFn $ return varExpr

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    env <- ask
    local (const $ Map.insert var evalVal env) $ eval rest
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) =do
    evalVal <- eval defExpr
    env <- ask
    let envFn = const $ Map.insert var evalVal env
    in local envFn $ evalBody $ List rest
evalBody x = eval x
