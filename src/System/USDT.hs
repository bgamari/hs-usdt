{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module System.USDT where

import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import System.Process

dtraceCmd :: FilePath
dtraceCmd = "dtrace"

newtype Tracepoint args = Tracepoint Name

dtraceDef :: String -> String -> String
dtraceDef providerName tpName = unlines
    [ "provider " ++ providerName ++ " {"
    , "  probe " ++ tpName ++ "();"
    , "};"
    ]

buildDTraceObject :: FilePath -> Q FilePath
buildDTraceObject dFile = do
    oFile <- addTempFile ".o"
    runIO $ callProcess dtraceCmd ["-C", "-G", "-s", dFile, "-o", oFile]
    return oFile

buildDTraceHeader :: FilePath -> Q FilePath
buildDTraceHeader dFile = do
    hFile <- addTempFile ".h"
    runIO $ callProcess dtraceCmd ["-C", "-h", "-s", dFile, "-o", hFile]
    return hFile

mkProviderName :: Q String
mkProviderName = do
    Module _ (ModName mn) <- thisModule
    let mn' = map f mn
        f '.' = '_'
        f c   = c
    return ("hs_usdt__" ++ mn)

mkTracepoint :: String -> Q (Tracepoint ())
mkTracepoint tpName = do
    providerName <- mkProviderName

    dFile <- addTempFile ".d"
    runIO $ writeFile dFile (dtraceDef providerName tpName)
    oFile <- buildDTraceObject dFile
    hFile <- buildDTraceHeader dFile
    addForeignFilePath RawObject oFile

    nm <- newName ("usdt_" ++ tpName)
    fimp_ty <- [t| IO () |]
    let fimp = ImportF CApi Unsafe cName nm fimp_ty
        cName = hFile ++ " " ++ macro
        macro = map toUpper providerName ++ "_" ++ map toUpper tpName
    addTopDecls [ForeignD fimp]

    return (Tracepoint nm)

triggerTracepoint :: Tracepoint () -> Q Exp
triggerTracepoint (Tracepoint nm) = varE nm

tracepoint :: String -> ExpQ
tracepoint tpName = do
    tp <- mkTracepoint tpName
    triggerTracepoint tp

