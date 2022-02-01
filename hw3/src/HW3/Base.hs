{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module HW3.Base
  where

import Codec.Serialise (Serialise)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Data (Data (toConstr), Typeable)
import Data.Map (Map)
import Data.Sequence (Seq, fromList)
import Data.Set (member)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Lazy.Encoding ()
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import HW3.Action (HIO (HIO), HiPermission (AllowRead, AllowTime, AllowWrite),
                   PermissionException (PermissionRequired))
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import System.Random.Stateful (Random (randomR), getStdRandom)

data HiFun =
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving stock (Show, Eq, Typeable, Data, Ord, Generic)
  deriving anyclass (Serialise)
data HiValue =
  HiValueNull
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving stock (Show, Eq, Typeable, Data, Ord, Generic)
  deriving anyclass (Serialise)

sameConstr :: HiValue -> HiValue -> Bool
sameConstr a b = toConstr a == toConstr b

data HiExpr =
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq)

data HiError =
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving stock (Show, Eq, Typeable, Data, Ord, Generic)
  deriving anyclass (Serialise)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving stock (Show, Eq, Typeable, Data, Ord, Generic)
  deriving anyclass (Serialise)

listOfAllSpecialActions :: [String]
listOfAllSpecialActions = ["cwd", "now"]

convertToAction :: String -> HiAction
convertToAction s = case s of
  "cwd" -> HiActionCwd
  "now" -> HiActionNow
  _     -> undefined

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance HiMonad HIO where
  runAction action = case action of
    HiActionRead fp -> HIO (
      \ps ->
      do
        if not $ member AllowRead ps
          then throwIO $ PermissionRequired AllowRead
          else do
            isFile <- doesFileExist fp
            isDir <- doesDirectoryExist fp
            if isFile
              then do
                bytes <- BS.readFile fp
                let
                  text' = decodeUtf8' bytes in
                    case text' of
                      Left _    -> return $ HiValueBytes bytes
                      Right txt -> return $ HiValueString txt
              else if isDir
                then do
                  dirs <- listDirectory fp
                  return $ HiValueList $ fromList (HiValueString . pack <$> dirs)
                else
                  return HiValueNull)

    HiActionWrite fp bs -> HIO (\ps ->
      do
        if not $ member AllowWrite ps
          then throwIO $ PermissionRequired AllowWrite
          else do
            BS.writeFile fp bs
            return HiValueNull)

    HiActionMkDir fp -> HIO (\ps ->
      do
        if not $ member AllowWrite ps
          then throwIO $ PermissionRequired AllowWrite
          else do
            createDirectory fp
            return HiValueNull)

    HiActionChDir fp -> HIO (\ps ->
      do
        if not $ member AllowRead ps
          then throwIO $ PermissionRequired AllowRead
          else do
            setCurrentDirectory fp
            return HiValueNull)

    HiActionCwd -> HIO (\ps ->
      if not $ member AllowRead ps
          then throwIO $ PermissionRequired AllowRead
          else HiValueString . pack <$> getCurrentDirectory)
    HiActionNow -> HIO (\ps ->
      do
        if not $ member AllowTime ps
          then throwIO $ PermissionRequired AllowTime
          else HiValueTime <$> getCurrentTime)
    HiActionRand i1 i2 -> HIO (\_ ->
      do
        ir <- getStdRandom (randomR (i1, i2)) :: IO Int
        return $ HiValueNumber $ toRational ir)

    HiActionEcho t -> HIO (\ps ->
      do
         if not $ member AllowWrite ps
          then throwIO $ PermissionRequired AllowWrite
          else do
            putStrLn (unpack t)
            return HiValueNull)


todo :: a
todo = undefined

listOfAllOperators :: [String]
listOfAllOperators = [
  "add",
  "sub",
  "mul",
  "div",
  "not-less-than",
  "not-greater-than",
  "not-equals",
  "not",
  "and",
  "or",
  "less-than",
  "greater-than",
  "equals",
  "if",
  "length",
  "to-upper",
  "to-lower",
  "reverse",
  "trim",
  "list",
  "range",
  "fold",
  "pack-bytes",
  "unpack-bytes",
  "zip",
  "unzip",
  "encode-utf8",
  "decode-utf8",
  "serialise",
  "deserialise",
  "read",
  "write",
  "mkdir",
  "cd",
  "parse-time",
  "rand",
  "echo",
  "count",
  "keys",
  "values",
  "invert"
  ]

convertToOperation :: String -> HiFun
convertToOperation f = case f of
  "add"              -> HiFunAdd
  "sub"              -> HiFunSub
  "mul"              -> HiFunMul
  "div"              -> HiFunDiv
  "not"              -> HiFunNot
  "and"              -> HiFunAnd
  "or"               -> HiFunOr
  "less-than"        -> HiFunLessThan
  "greater-than"     -> HiFunGreaterThan
  "equals"           -> HiFunEquals
  "not-less-than"    -> HiFunNotLessThan
  "not-greater-than" -> HiFunNotGreaterThan
  "not-equals"       -> HiFunNotEquals
  "if"               -> HiFunIf
  "length"           -> HiFunLength
  "to-upper"         -> HiFunToUpper
  "to-lower"         -> HiFunToLower
  "reverse"          -> HiFunReverse
  "trim"             -> HiFunTrim
  "list"             -> HiFunList
  "range"            -> HiFunRange
  "fold"             -> HiFunFold
  "pack-bytes"       -> HiFunPackBytes
  "unpack-bytes"     -> HiFunUnpackBytes
  "zip"              -> HiFunZip
  "unzip"            -> HiFunUnzip
  "encode-utf8"      -> HiFunEncodeUtf8
  "decode-utf8"      -> HiFunDecodeUtf8
  "serialise"        -> HiFunSerialise
  "deserialise"      -> HiFunDeserialise
  "read"             -> HiFunRead
  "write"            -> HiFunWrite
  "mkdir"            -> HiFunMkDir
  "cd"               -> HiFunChDir
  "parse-time"       -> HiFunParseTime
  "rand"             -> HiFunRand
  "echo"             -> HiFunEcho
  "count"            -> HiFunCount
  "keys"             -> HiFunKeys
  "values"           -> HiFunValues
  "invert"           -> HiFunInvert
  _                  -> undefined

data FunInfo = FunInfo {
  getName  :: String,
  getNArgs :: NumberOfArgs
}

data NumberOfArgs = Finite Int | Infinity

info :: HiFun -> FunInfo
info f = case f of
  HiFunDiv            -> FunInfo "div" (Finite 2)
  HiFunMul            -> FunInfo "mul" (Finite 2)
  HiFunAdd            -> FunInfo "add" (Finite 2)
  HiFunSub            -> FunInfo "sub" (Finite 2)
  HiFunNot            -> FunInfo "not" (Finite 1)
  HiFunAnd            -> FunInfo "and" (Finite 2)
  HiFunOr             -> FunInfo "or" (Finite 2)
  HiFunLessThan       -> FunInfo "less-than" (Finite 2)
  HiFunGreaterThan    -> FunInfo "greater-than" (Finite 2)
  HiFunEquals         -> FunInfo "equals" (Finite 2)
  HiFunNotLessThan    -> FunInfo "not-less-than" (Finite 2)
  HiFunNotGreaterThan -> FunInfo "not-greater-than" (Finite 2)
  HiFunNotEquals      -> FunInfo "not-equals" (Finite 2)
  HiFunIf             -> FunInfo "if" (Finite 3)
  HiFunLength         -> FunInfo "length" (Finite 1)
  HiFunToUpper        -> FunInfo "to-upper" (Finite 1)
  HiFunToLower        -> FunInfo "to-lower" (Finite 1)
  HiFunReverse        -> FunInfo "reverse" (Finite 1)
  HiFunTrim           -> FunInfo "trim" (Finite 1)
  HiFunList           -> FunInfo "list" Infinity
  HiFunRange          -> FunInfo "range" (Finite 2)
  HiFunFold           -> FunInfo "fold" (Finite 2)
  HiFunPackBytes      -> FunInfo "pack-bytes" (Finite 1)
  HiFunUnpackBytes    -> FunInfo "unpack-bytes" (Finite 1)
  HiFunEncodeUtf8     -> FunInfo "encode-utf8" (Finite 1)
  HiFunDecodeUtf8     -> FunInfo "decode-utf8" (Finite 1)
  HiFunZip            -> FunInfo "zip" (Finite 1)
  HiFunUnzip          -> FunInfo "unzip" (Finite 1)
  HiFunSerialise      -> FunInfo "serialise" (Finite 1)
  HiFunDeserialise    -> FunInfo "deserialise" (Finite 1)
  HiFunRead           -> FunInfo "read" (Finite 1)
  HiFunWrite          -> FunInfo "write" (Finite 2)
  HiFunMkDir          -> FunInfo "mkdir" (Finite 1)
  HiFunChDir          -> FunInfo "cd" (Finite 1)
  HiFunParseTime      -> FunInfo "parseTime" (Finite 1)
  HiFunRand           -> FunInfo "rand" (Finite 2)
  HiFunEcho           -> FunInfo "echo" (Finite 1)
  HiFunKeys           -> FunInfo "keys" (Finite 1)
  HiFunCount          -> FunInfo "count" (Finite 1)
  HiFunValues         -> FunInfo "values" (Finite 1)
  HiFunInvert         -> FunInfo "invert" (Finite 1)
