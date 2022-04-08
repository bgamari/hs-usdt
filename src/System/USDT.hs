{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module System.USDT
  ( Tracepoint
  , tracepoint
  , mkTracepoint
  , mkTracepoint'
  , triggerTracepoint
  ) where

import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import System.Process

-- | Register a new tracepoint and trigger it.
tracepoint :: String -> Q Exp
tracepoint tpName = do
    [e| triggerTracepoint $(mkTracepoint' tpName) |]

-- | Register a new tracepoint.
mkTracepoint' :: String -> Q Exp

-- | Register a new tracepoint.
mkTracepoint :: String -> Code Q (Tracepoint ())
mkTracepoint lbl = unsafeCodeCoerce (mkTracepoint' lbl)

-- | Trigger a tracepoint previously created using 'mkTracepoint'.
triggerTracepoint :: Tracepoint () -> IO ()

#if !defined(DTRACE)

data Tracepoint args = Tracepoint

triggerTracepoint _ = return ()

mkTracepoint' _ = return Tracepoint

#else

newtype Tracepoint args = Tracepoint (IO ())

mkProviderName :: Q String
mkProviderName = do
    Module _ (ModName mn) <- thisModule
    let mn' = map f mn
        f '.' = '_'
        f c   = c
    return ("hs_usdt__" ++ mn)

mkTracepoint' tpName = do
    providerName <- mkProviderName

    let func = "hs_" ++ map toUpper providerName ++ "_" ++ map toUpper tpName
    let cStub = unlines
            [ "#include <sys/sdt.h>"
            , "void " ++ func ++ "(void) {"
            , "  DTRACE_PROBE(" ++ providerName ++ ", " ++ tpName ++ ");"
            , "}"
            ]
    addForeignSource LangC cStub

    nm <- newName ("usdt_" ++ tpName)
    fimp_ty <- [t| IO () |]
    let fimp = ImportF CCall Unsafe func nm fimp_ty
    addTopDecls [ForeignD fimp]

    [e| Tracepoint $(varE nm) |]

triggerTracepoint (Tracepoint x) = x

#endif

