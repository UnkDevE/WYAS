{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal (
    LispVal(..),
    Eval(..),
    IFunc(..),
    EnvCtx
) where 

import Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader

data LispVal 
    = Atom T.Text
    | List [LispVal]
    | Number Integer
    | String T.Text
    | Fun IFunc
    | Lambada IFunc EnvCtx
    | Nil
    | Bool Bool deriving (Typeable)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
    deriving ( Monad 
             , Functor
             , Applicative
             , MonadReader EnvCtx
             , MonadIO)

instance Show LispVal where 
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val =
    case val of
        (Atom atom) -> atom
        (String str) -> T.concat ["\"", str,"\""]
        (Number num) -> T.pack $ show num
        (Bool True) -> "#t"
        (Bool False) -> "#f"
        Nil -> "Nil"
        (List contents) -> T.concat ["(", T.unwords $ showVal <$> contents, ")"]
        (Fun _) -> "(internal function)"
        (Lambda _ _) -> "(lambda function)"

