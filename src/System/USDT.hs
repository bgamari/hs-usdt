{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module System.USDT
  ( Tracepoint
  , tracepoint
  , mkTracepoint
  , mkTracepoint'
  , triggerTracepoint
  ) where

import Data.List (intercalate)
import Data.Proxy
import Data.Char
import Data.Tuple
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import System.Process

newtype CTypeName = CTypeName { getCType :: String }

class TPArgs a where
    argsTypes :: Proxy a -> Q [(CTypeName, Type)]
    applyArgs :: Proxy a -> Q Exp -> Q Exp

arg :: forall a. PrimArg a => Proxy a -> Q (CTypeName, Type)
arg proxy = (,) <$> pure (argCType proxy) <*> argType proxy

instance TPArgs () where
    argsTypes _ = return []
    applyArgs _ e = [e| \() -> $(e) |]

instance TPArgs Int where
    argsTypes _ = sequence [arg @Int Proxy]
    applyArgs _ e = [e| \a -> $(e) a |]

instance PrimArg a => TPArgs (Solo a) where
    argsTypes _ = sequence [arg @a Proxy]
    applyArgs _ e = [e| \(Solo a) -> $(e) a |]

instance (PrimArg a, PrimArg b) => TPArgs (a, b) where
    argsTypes _ = sequence [arg @a Proxy, arg @b Proxy]
    applyArgs _ e = [e| \(a,b) -> $(e) a b |]

instance {-# OVERLAPS #-} (PrimArg a) => TPArgs a where
    argsTypes _ = sequence [arg @a Proxy]
    applyArgs _ e = [e| \a -> $(e) a |]

class Lift a => PrimArg a where
    argCType :: Proxy a -> CTypeName
    argType :: Proxy a -> Q Type

instance PrimArg Int where
    argCType _ = CTypeName "intptr_t"
    argType _ = [t| Int |]

-- | Register a new tracepoint and trigger it.
tracepoint :: forall args. (Lift args, TPArgs args)
           => String -> args -> Q Exp
tracepoint tpName args = do
    tp <- mkTracepoint' (Proxy @args) tpName
    [e| triggerTracepoint $(pure tp) args |]

-- | Register a new tracepoint.
mkTracepoint' :: forall args. (TPArgs args)
              => Proxy args -> String -> Q Exp

-- | Register a new tracepoint.
mkTracepoint :: forall args. (TPArgs args)
             => String -> Code Q (Tracepoint args)
mkTracepoint lbl = unsafeCodeCoerce (mkTracepoint' (Proxy @args) lbl)

-- | Trigger a tracepoint previously created using 'mkTracepoint'.
triggerTracepoint :: Tracepoint args -> args -> IO ()

#if !defined(DTRACE)

data Tracepoint args = Tracepoint

triggerTracepoint _ _ = return ()

mkTracepoint' _ _ = [e| Tracepoint |]

#else

newtype Tracepoint args = Tracepoint (args -> IO ())

mkProviderName :: Q String
mkProviderName = do
    Module _ (ModName mn) <- thisModule
    let mn' = map f mn
        f '.' = '_'
        f c   = c
    return ("hs_usdt__" ++ mn)

mkTracepoint' _ tpName = do
    providerName <- mkProviderName

    let func = "hs_" ++ map toUpper providerName ++ "_" ++ map toUpper tpName
    args <- argsTypes (Proxy @args)
    let commaSep = intercalate ", "
    let cArgNames =
            [ "arg" ++ show i
            | (i, _) <- zip [0..] args
            ]
    let cArgTypes =
            [ getCType cty ++ " " ++ nm
            | (nm, cty) <- zip cArgNames (map fst args)
            ]

    let cStub = unlines
            [ "#include <stdint.h>"
            , "#include <sys/sdt.h>"
            , "void " ++ func ++ "(" ++ commaSep cArgTypes ++ ") {"
            , "  " ++ macro ++ "(" ++ (commaSep $ [providerName, tpName] ++ cArgNames) ++ ");"
            , "}"
            ]
        macro = case length args of
              0 -> "DTRACE_PROBE"
              n -> "DTRACE_PROBE" ++ show n
    addForeignSource LangC cStub

    nm <- newName ("usdt_" ++ tpName)
    let mkFun a b = [t| $(a) -> $(b) |]
    fimp_ty <- foldr mkFun [t| IO () |] (map (return . snd) args)
    let fimp = ImportF CCall Unsafe func nm fimp_ty
    addTopDecls [ForeignD fimp]

    [e| Tracepoint $(applyArgs (Proxy @args) (varE nm)) |]

triggerTracepoint (Tracepoint x) args = x args

#endif

