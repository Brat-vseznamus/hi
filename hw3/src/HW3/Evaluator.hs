module HW3.Evaluator
  where

import Codec.Compression.Zlib (bestCompression, compressWith, decompress, defaultCompressParams)
import Codec.Compression.Zlib.Raw (CompressParams (CompressParams))
import Codec.Serialise (deserialise, serialise)
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift), runExceptT)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (Foldable (toList), find)
import Data.Functor ((<&>))
import Data.Map (Map, fromListWith)
import qualified Data.Map as MAP
import Data.Semigroup (stimes)
import Data.Sequence (fromList, (><))
import qualified Data.Sequence as S
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import HW3.Base (FunInfo (getNArgs),
                 HiAction (HiActionChDir, HiActionEcho, HiActionMkDir, HiActionRand, HiActionRead, HiActionWrite),
                 HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction), HiValue (..),
                 NumberOfArgs (Finite, Infinity), info)
import HW3.Pretty (NumberContent (IntegerContent), toNumberContent)
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . eval'

eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' expr =
  case expr of
    (HiExprValue value) ->
      return value
    (HiExprApply operation elements) -> do
      operValue <- eval' operation
      case operValue of
        (HiValueBool _) -> throwError HiErrorInvalidFunction
        (HiValueNumber _) -> throwError HiErrorInvalidFunction
        (HiValueString str) -> computeSubstring2 str elements
        (HiValueList seq') -> computeSubList2 seq' elements
        (HiValueBytes bytes) -> computeSubByteArray2 bytes elements
        (HiValueAction _) -> throwError HiErrorInvalidFunction
        (HiValueTime _) -> throwError HiErrorInvalidFunction
        (HiValueDict map') -> computeSubMap2 map' elements
        HiValueNull -> throwError HiErrorInvalidFunction
        (HiValueFunction fun) ->
          if isLazyLogicFunction fun
            then computeLazyLogic fun elements
            else do
              elementsValues <- mapM eval' elements
              if not $ checkLength ((getNArgs . info) fun) (length elementsValues)
                then throwError HiErrorArityMismatch
                else computeFunctionApplication2 fun elementsValues
    (HiExprRun cmd) -> do
      value <- eval' cmd
      case value of
        (HiValueAction action) -> lift $ runAction action
        _                      -> throwError HiErrorInvalidArgument
    (HiExprDict pairs) -> do
      values <- mapM (\(x, y) -> do
        x' <- eval' x
        y' <- eval' y
        return (x', y')) (toList pairs)
      return $ HiValueDict (MAP.fromList values)

checkLength :: NumberOfArgs -> Int -> Bool
checkLength Infinity _   = True
checkLength (Finite n) m = n == m


computeSubStructure :: HiMonad m =>
  (a -> Int) ->
  (a -> Integer -> HiValue) ->
  (a -> Rational -> Rational -> ExceptT HiError m HiValue) ->
  a ->
  [HiExpr] ->
  ExceptT HiError m HiValue
computeSubStructure
  toLen
  toElement
  toSlice
  structure
  elements = do
  elementsValues <- mapM eval' elements
  case elementsValues of
    []                                       -> throwError HiErrorArityMismatch
    [HiValueNumber ind]                      -> do
      if ind < 0 || ind >= toRational (toLen structure)
      then return HiValueNull
      else case toNumberContent ind of
        IntegerContent n -> return $ toElement structure n
        _                -> throwError HiErrorInvalidArgument

    [HiValueNumber ind1, HiValueNumber ind2] -> toSlice structure ind1 ind2
    [HiValueNull , HiValueNumber ind2] -> toSlice structure 0 ind2
    [HiValueNumber ind1, HiValueNull] -> toSlice structure ind1 (toRational $ toLen structure)
    [HiValueNull, HiValueNull] -> toSlice structure 0 (toRational $ toLen structure)
    _                                        -> throwError HiErrorInvalidArgument

computeSubMap2 :: HiMonad m  =>
  Map HiValue HiValue ->
  [HiExpr] ->
  ExceptT HiError m HiValue
computeSubMap2 map' [str] = do
  k <- eval' str
  let v = MAP.lookup k map' in case v of
    Nothing -> return HiValueNull
    Just v' -> return v'
computeSubMap2 _ _ = throwError HiErrorInvalidArgument

computeSubstring2 :: HiMonad m =>
  Text ->
  [HiExpr] ->
  ExceptT HiError m HiValue
computeSubstring2 = computeSubStructure
  T.length
  (\s n -> HiValueString $ pack [T.index s (fromIntegral n)])
  getSubstring2

computeSubList2 :: HiMonad m =>
  S.Seq HiValue ->
  [HiExpr] ->
  ExceptT HiError m HiValue
computeSubList2 = computeSubStructure
  S.length
  (\s n -> S.index s (fromIntegral n))
  getSubList2

computeSubByteArray2 :: HiMonad m =>
  BS.ByteString ->
  [HiExpr] ->
  ExceptT HiError m HiValue
computeSubByteArray2 = computeSubStructure
  BS.length
  (\s n -> HiValueNumber $
    (toRational . fromEnum)
    ((!!) (BS.unpack s) (fromIntegral n)))
  getSubByteArray2

getSubStructure :: HiMonad m =>
  (a -> Int) ->
  HiValue ->
  (a -> Int -> Int -> Int -> HiValue) ->
  a ->
  Rational ->
  Rational ->
  ExceptT HiError m HiValue
getSubStructure
  toLen
  value0
  toValue2
  structure
  ind1 ind2 =
  case toNumberContent ind1 of
    IntegerContent n -> do
      case toNumberContent ind2 of
        IntegerContent m -> do
          let len = toLen structure
              m'  = toPythonIndex (fromIntegral m) len
              n'  = toPythonIndex (fromIntegral n) len in
            if n' < 0 || m' < 0 || m' > len || n' >= len || n' > m'
            then return value0
            else
              return $ toValue2 structure n' m' len
        _ -> throwError HiErrorInvalidArgument
    _ -> throwError HiErrorInvalidArgument

getSubstring2 :: HiMonad m => Text -> Rational -> Rational -> ExceptT HiError m HiValue
getSubstring2 = getSubStructure
  T.length
  (HiValueString (pack ""))
  (\s n' m' len -> HiValueString $ T.drop n' (T.dropEnd (len - m') s))

getSubList2 :: HiMonad m => S.Seq HiValue -> Rational -> Rational -> ExceptT HiError m HiValue
getSubList2 = getSubStructure
  S.length
  (HiValueList S.empty)
  (\s n' m' _ -> HiValueList $ fromList $ drop n' $ take m' (toList s))

getSubByteArray2 :: HiMonad m => BS.ByteString -> Rational -> Rational -> ExceptT HiError m HiValue
getSubByteArray2 = getSubStructure
  BS.length
  (HiValueList S.empty)
  (\s n' m' _ -> HiValueBytes $ BS.pack $ drop n' $ take m' (BS.unpack s))

toPythonIndex :: Int -> Int -> Int
toPythonIndex pos len
  | pos < -len = 0
  | -len <= pos && pos < 0 = pos + len
  | 0 <= pos && pos < len = pos
  | otherwise = len

isNumber :: HiValue -> Bool
isNumber v = case v of
  (HiValueNumber _) -> True
  _                 -> False

isBool :: HiValue -> Bool
isBool v = case v of
  (HiValueBool _) -> True
  _               -> False

isString :: HiValue -> Bool
isString v = case v of
  (HiValueString _) -> True
  _                 -> False

isList :: HiValue -> Bool
isList v = case v of
  (HiValueList _) -> True
  _               -> False


isBytes :: HiValue -> Bool
isBytes v = case v of
  (HiValueBytes _) -> True
  _                -> False

isNum :: NumberContent  -> Bool
isNum v = case v of
  (IntegerContent _) -> True
  _                  -> False

isTime :: HiValue -> Bool
isTime v = case v of
  (HiValueTime _) -> True
  _               -> False

toText :: HiValue -> Text
toText t = case t of
  (HiValueString t') -> t'
  _                  -> T.empty


computeFunctionApplication2 :: HiMonad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
computeFunctionApplication2 f l = case f of
  HiFunAdd -> overloading
    [
      (all isNumber, computeArithmetic (+)),
      (all isString, concatStrings),
      (all isList,   concatLists),
      (all isBytes,  concatBytes),
      (\[a, b] -> isTime a && isNumber b, addTime)
    ] l
  HiFunSub -> overloading
    [
      (all isNumber, computeArithmetic (-)),
      (all isTime,   subTime)
    ] l
  HiFunMul -> overloading
    [
      (all isNumber, computeArithmetic (*)),
      (\[a, b] -> isString a && isNumber b, repeatString),
      (\[a, b] -> isList a && isNumber b, repeatList),
      (\[a, b] -> isBytes a && isNumber b, repeatBytes)
    ] l
  HiFunDiv -> overloading
    [
      (all isNumber,
      \_ ->
        do
          if not (all isNumber l)
            then throwError HiErrorInvalidArgument
            else let [HiValueNumber a, HiValueNumber b] = l in
              if b == 0
                then throwError HiErrorDivideByZero
                else return (HiValueNumber (a / b))),
      (all isString, concatStrings2)
    ] l
  HiFunNot -> computeLogic (\[x] -> not x) l
  -- HiFunAnd -> computeLogic and l
  -- HiFunOr -> computeLogic or l
  HiFunLessThan -> computeComparing2 (<) l
  HiFunGreaterThan -> computeComparing2 (>) l
  HiFunEquals -> computeComparing2 (==) l
  HiFunNotLessThan -> computeComparing2 (>=) l
  HiFunNotGreaterThan -> computeComparing2 (<=) l
  HiFunNotEquals -> computeComparing2 (/=) l
  -- HiFunIf -> do
  --   let [conc', a, b] = l in
  --     case conc' of
  --       (HiValueBool cond) ->
  --         if not (sameConstr a b) then throwError HiErrorInvalidArgument
  --         else return $ if cond then a else b
  --       _ -> throwError HiErrorInvalidArgument
  HiFunLength -> overloading
    [
      (all isString, computeStringOperation (HiValueNumber . toRational . T.length)),
      (all isList, sequenceLength)
    ] l
  HiFunToUpper -> computeStringOperation (HiValueString . T.toUpper) l
  HiFunToLower -> computeStringOperation (HiValueString . T.toLower) l
  HiFunReverse -> overloading
    [
      (all isString, computeStringOperation (HiValueString . T.reverse)),
      (all isList, reverseSequence)
    ] l
  HiFunTrim -> computeStringOperation (HiValueString . T.strip) l
  HiFunList -> return (HiValueList (fromList l))
  HiFunFold -> do
    let [fun, elements] = l in case fun of
      (HiValueFunction hf) ->
        do
          case elements of
            (HiValueList els) -> do
              case getNArgs . info $ hf of
                Finite n ->
                  if n /= 2 then throwError HiErrorInvalidFunction
                  else
                    case S.length els of
                      0 -> return HiValueNull
                      1 -> return (S.index els 0)
                      _ -> eval' $ foldl1 (\a b -> HiExprApply (HiExprValue (HiValueFunction hf)) [a, b]) (HiExprValue <$> els)
                Infinity -> throwError HiErrorInvalidFunction
            _ -> throwError HiErrorInvalidArgument
      _ -> throwError HiErrorInvalidArgument
  HiFunRange -> do
    case l of
      [HiValueNumber start, HiValueNumber end] -> do
        if start > end
          then return $ HiValueList . fromList $ []
          else return $ HiValueList . fromList $ HiValueNumber <$> [start..end]
      _ -> throwError HiErrorArityMismatch
  HiFunPackBytes -> do
    case l of
      [HiValueList seq'] -> do
        let els = toList seq' in
          if all isNumber els
            then do
              let ns = (\(HiValueNumber r) -> toNumberContent r) <$> els in
                if all isNum ns
                  then
                    let is = (\(IntegerContent n) -> fromIntegral n) <$> ns in
                      if all (\i -> 0 <= i && i <= 255) is
                        then return $ HiValueBytes $ BS.pack (toEnum <$> is)
                        else throwError HiErrorInvalidArgument
                  else throwError HiErrorInvalidArgument
            else throwError HiErrorInvalidArgument
      _ -> throwError HiErrorInvalidArgument
  HiFunUnpackBytes -> do
    case l of
      [HiValueBytes bytes] -> do
        let els = BS.unpack bytes in
          return $ HiValueList $
            fromList (els <&>
              HiValueNumber . toRational . fromEnum)
      _ -> throwError HiErrorInvalidArgument
  HiFunEncodeUtf8 -> do
    case l of
      [HiValueString text] ->
        return $ HiValueBytes (encodeUtf8 text)
      _ -> throwError HiErrorInvalidArgument
  HiFunDecodeUtf8 -> do
    case l of
      [HiValueBytes bytes] -> do
        case decodeUtf8' bytes of
          Left _    -> return HiValueNull
          Right txt -> return $ HiValueString txt
      _ -> throwError HiErrorInvalidArgument
  HiFunZip ->
    case l of
      [HiValueBytes bytes] -> return $ HiValueBytes $
        toStrict (compressWith bestCompressParams (toLazy bytes))
      _ -> throwError HiErrorInvalidArgument
  HiFunUnzip ->
    case l of
      [HiValueBytes bytes] -> return $ HiValueBytes $
        toStrict (decompress (toLazy bytes))
      _ -> throwError HiErrorInvalidArgument
  HiFunSerialise ->
    case l of
      [obj] -> return $ HiValueBytes $ toStrict $ serialise obj
      _     -> throwError HiErrorInvalidArgument
  HiFunDeserialise ->
    case l of
      [HiValueBytes bytes] -> return $ deserialise (toLazy bytes)
      _                    -> throwError HiErrorInvalidArgument
  HiFunRead ->
    case l of
      [HiValueString text] ->
        return $ HiValueAction (HiActionRead (T.unpack text))
      _ -> throwError HiErrorInvalidArgument
  HiFunWrite ->
    case l of
      [HiValueString fp, HiValueString text] ->
        return $ HiValueAction (HiActionWrite (T.unpack fp) (encodeUtf8 text))
      [HiValueString fp, HiValueBytes bytes] ->
        return $ HiValueAction (HiActionWrite (T.unpack fp) bytes)
      _ -> throwError HiErrorInvalidArgument
  HiFunMkDir ->
    case l of
      [HiValueString fp] ->
       return $ HiValueAction (HiActionMkDir (T.unpack fp))
      _ -> throwError HiErrorInvalidArgument
  HiFunChDir ->
    case l of
      [HiValueString fp] ->
        return $ HiValueAction (HiActionChDir (T.unpack fp))
      _ -> throwError HiErrorInvalidArgument
  HiFunParseTime ->
    case l of
      [HiValueString text] -> do
        let time = (readMaybe (T.unpack text) :: Maybe UTCTime) in
          case time of
            Just t -> return $ HiValueTime t
            _      -> return HiValueNull
      _ -> throwError HiErrorInvalidArgument
  HiFunRand ->
    case l of
      [HiValueNumber n1, HiValueNumber n2] -> do
        case (toNumberContent n1, toNumberContent n2) of
          (IntegerContent i1, IntegerContent i2) -> do
            if i1 < toInteger (minBound :: Int)
              || i1 > toInteger (maxBound :: Int)
              || i2 < toInteger (minBound :: Int)
              || i2 > toInteger (maxBound :: Int)
              then throwError HiErrorInvalidArgument
              else
                return
                  $ HiValueAction
                  $ HiActionRand (fromInteger i1) (fromInteger i2)
          _                                      -> throwError HiErrorInvalidArgument
      _ -> throwError HiErrorInvalidArgument
  HiFunEcho ->
    case l of
      [HiValueString text] ->
          return $ HiValueAction (HiActionEcho text)
      _ -> throwError HiErrorInvalidArgument
  HiFunKeys ->
    case l of
      [HiValueDict d] ->
        return $ HiValueList (fromList (MAP.keys d))
      _   -> throwError HiErrorInvalidArgument
  HiFunValues ->
    case l of
      [HiValueDict d] ->
        return $ HiValueList (fromList (MAP.elems d))
      _   -> throwError HiErrorInvalidArgument
  HiFunCount ->
    case l of
      [HiValueString text] ->
        return $ HiValueDict $
        MAP.map (HiValueNumber . toRational)
        (fromListWith (+)
          ((\x -> (HiValueString (T.pack [x]), 1 :: Int)) <$> T.unpack text))
      [HiValueBytes bytes] ->
        return $ HiValueDict $
        MAP.map (HiValueNumber . toRational)
        (fromListWith (+)
          ((\x -> (HiValueNumber $ toRational $ fromEnum x, 1 :: Int)) <$> BS.unpack bytes))
      [HiValueList seq']   ->
        return $ HiValueDict $
        MAP.map (HiValueNumber . toRational)
        (fromListWith (+)
          ((\x -> (x, 1 :: Int)) <$> toList seq'))
      _                      -> throwError HiErrorInvalidArgument
  HiFunInvert -> case l of
      [HiValueDict d] ->
        return $ HiValueDict $
          MAP.map
          (HiValueList . fromList)
          (MAP.fromListWith
            (++)
            $ (\(x, y) -> (y, [x])) <$> MAP.toList d)
      _   -> throwError HiErrorInvalidArgument
  _ -> undefined



isLazyLogicFunction :: HiFun -> Bool
isLazyLogicFunction f = case f of
  HiFunIf  -> True
  HiFunAnd -> True
  HiFunOr  -> True
  _        -> False

computeLazyLogic :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
computeLazyLogic HiFunIf [pr, a, b] =
  computeLazy pr False
  (\x -> case x of
    (HiValueBool True) -> True
    _                  -> False)
  (\c -> if c then a else b)

computeLazyLogic HiFunAnd [a, b] =
  computeLazy a True
  (\x -> case x of
    HiValueNull         -> True
    (HiValueBool False) -> True
    _                   -> False)
  (\c -> if c then a else b)

computeLazyLogic HiFunOr [a, b] =
  computeLazy a True
  (\x -> case x of
    HiValueNull         -> False
    (HiValueBool False) -> False
    _                   -> True)
  (\c -> if c then a else b)

computeLazyLogic _ _ = throwError HiErrorInvalidArgument

computeLazy :: HiMonad m =>
  HiExpr ->
  Bool ->
  (HiValue -> Bool) ->
  (Bool -> HiExpr) ->
  ExceptT HiError m HiValue
computeLazy
  predictor
  affectSelection
  predicate
  choiceHandler = do
    prValue <- eval' predictor
    let selection = predicate prValue in
      if affectSelection && selection
        then return prValue
        else eval' (choiceHandler selection)

bestCompressParams :: CompressParams
bestCompressParams = case defaultCompressParams of {
  CompressParams _ me wb ml cs n m_bs ->
    CompressParams bestCompression me wb ml cs n m_bs}

toLazy :: BS.ByteString -> BL.ByteString
toLazy = fromStrict


concatStrings :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
concatStrings [HiValueString a, HiValueString b] =
  return $ HiValueString (a <> b)
concatStrings _ = throwError HiErrorInvalidArgument

concatStrings2 :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
concatStrings2 [HiValueString a, HiValueString b] =
  return $ HiValueString (a <> pack "/" <> b)
concatStrings2 _ = throwError HiErrorInvalidArgument

repeatString :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
repeatString [HiValueString s, HiValueNumber n] =
  case toNumberContent n of
    IntegerContent i ->
      if i <= 0
        then throwError HiErrorInvalidArgument
        else
        return $ HiValueString (stimes i s)
    _                -> throwError HiErrorInvalidArgument

repeatString _ = throwError HiErrorInvalidArgument

sequenceLength :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
sequenceLength [HiValueList seq'] =
  return $ HiValueNumber $ toRational $ length seq'
sequenceLength _ = throwError HiErrorInvalidArgument

reverseSequence :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
reverseSequence [HiValueList seq'] =
  return $ HiValueList $ S.reverse seq'
reverseSequence _ = throwError HiErrorInvalidArgument

concatLists :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
concatLists [HiValueList seq1, HiValueList seq2] =
  return $ HiValueList $ seq1 >< seq2
concatLists _ = throwError HiErrorInvalidArgument

repeatList :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
repeatList [HiValueList seq', HiValueNumber n] =
  case toNumberContent n of
    IntegerContent i ->
      if i <= 0
        then throwError HiErrorInvalidArgument
        else
        return $ HiValueList (stimes i seq')
    _                -> throwError HiErrorInvalidArgument
repeatList _ = throwError HiErrorInvalidArgument

concatBytes :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
concatBytes [HiValueBytes bytes1, HiValueBytes bytes2] =
  return $ HiValueBytes $ bytes1 <> bytes2
concatBytes _ = throwError HiErrorInvalidArgument

repeatBytes :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
repeatBytes [HiValueBytes bytes, HiValueNumber n] =
  case toNumberContent n of
    IntegerContent i ->
      if i <= 0
        then throwError HiErrorInvalidArgument
        else
        return $ HiValueBytes (stimes i bytes)
    _                -> throwError HiErrorInvalidArgument
repeatBytes _ = throwError HiErrorInvalidArgument

addTime :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
addTime [HiValueTime time, HiValueNumber n] =
  return $ HiValueTime (addUTCTime (fromRational n) time)
addTime _ = throwError HiErrorInvalidArgument

subTime :: HiMonad m => [HiValue] -> ExceptT HiError m HiValue
subTime [HiValueTime time1, HiValueTime time2] =
  return $ HiValueNumber $ toRational (diffUTCTime time1 time2)
subTime _ = throwError HiErrorInvalidArgument


computeStringOperation :: HiMonad m =>
  (Text -> HiValue) ->
  [HiValue] ->
  ExceptT HiError m HiValue
computeStringOperation f l =
  let [s'] = l in
      case s' of
        (HiValueString text) ->
          return $ f text
        _ -> throwError HiErrorInvalidArgument

computeArithmetic :: HiMonad m =>
  (Rational -> Rational -> Rational) ->
  [HiValue] ->
  ExceptT HiError m HiValue
computeArithmetic f l =
  if not (all isNumber l)
    then throwError HiErrorInvalidArgument
    else let [HiValueNumber a, HiValueNumber b] = l in
      return (HiValueNumber $ f a b)

computeLogic :: HiMonad m =>
  ([Bool] -> Bool) ->
  [HiValue] ->
  ExceptT HiError m HiValue
computeLogic f l =
  if not (all isBool l)
    then throwError HiErrorInvalidArgument
    else let boolList = (\(HiValueBool y) -> y) <$> l in
      return (HiValueBool $ f boolList)

computeComparing2 :: HiMonad m =>
  (HiValue -> HiValue -> Bool) ->
  [HiValue] ->
  ExceptT HiError m HiValue
computeComparing2 f l = let [a, b] = l in return $ HiValueBool $ f a b

overloading :: HiMonad m =>
  [([HiValue] -> Bool, [HiValue] -> ExceptT HiError m HiValue)] ->
  [HiValue] ->
  ExceptT HiError m HiValue
overloading fs l =
  let f' = snd <$> find (\(pr, _) -> pr l) fs in
    case f' of
      Nothing -> throwError HiErrorInvalidArgument
      Just f  -> f l
