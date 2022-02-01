module HW3.Pretty
  where
import Data.ByteString (ByteString, unpack)
import Data.Char (chr, ord)
import Data.Foldable (Foldable (toList))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as MAP
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific,
                        fromRationalRepetendUnlimited)
import Data.Sequence (Seq)
import HW3.Base (FunInfo (getName), HiAction (..), HiValue (..), info)
import Prettyprinter (Doc, (<+>), Pretty (pretty), viaShow)
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue value = case value of
  HiValueFunction fun     -> prettyFun fun
  HiValueBool bool        -> prettyBool bool
  HiValueNumber ratio     -> prettyNumber ratio
  HiValueNull             -> pretty "null"
  HiValueString text      -> pretty $ show text
  HiValueList seq'        -> prettySeq seq'
  HiValueBytes byteString -> prettyBytes byteString
  HiValueAction action    -> prettyAction action
  HiValueTime time        -> prettyTime time
  HiValueDict map'        -> prettyMap map'
  where
    prettyFun f = (pretty . getName . info) f
    prettyBool bool = if bool then pretty "true" else pretty "false"
    prettyNumber ratio =
      let c = toNumberContent ratio in case c of
        IntegerContent n -> pretty n
        FractionContent n m -> pretty n <> pretty '/' <> pretty m
        DecimalContent sci -> pretty (formatScientific Fixed Nothing sci)
        MixedContent n i j ->
          pretty n <+> pretty (if i > 0 then "+" else "-") <+> pretty (abs i) <> pretty "/" <> pretty j
    prettyTime time =
      pretty "parse-time(\""
      <>
      viaShow time
      <>
      pretty "\")"

    prettyAction action = case action of
      HiActionRead s -> pretty "read(" <> viaShow s <> pretty ")"
      HiActionWrite s bs ->
        pretty "write("
        <>
        viaShow s
        <>
        pretty ","
        <+>
        prettyBytes bs
        <>
        pretty ")"
      HiActionMkDir s ->
        pretty "mkdir("
        <>
        viaShow s
        <>
        pretty ")"
      HiActionChDir s ->
        pretty "cd("
        <>
        viaShow s
        <>
        pretty ")"
      HiActionCwd -> pretty "cwd"
      HiActionNow -> pretty "now"
      HiActionRand i1 i2  ->
        pretty "rand("
        <>
        pretty i1
        <>
        pretty ", "
        <>
        pretty i2
        <>
        pretty ")"
      HiActionEcho text ->
        pretty "echo("
        <>
        pretty (show text)
        <>
        pretty ")"

prettyMap :: Map HiValue HiValue -> Doc AnsiStyle
prettyMap map' =
      let pretties = (\(k, v) ->
            prettyValue k
            <>
            pretty ": "
            <>
            prettyValue v) <$> MAP.toList map' in
        if null pretties
          then pretty "{ }"
          else
            pretty "{"
            <+>
            (foldl1 (\ac n -> ac <> pretty ", " <> n)  pretties)
            <+>
            pretty "}"


prettySeq :: Seq HiValue -> Doc AnsiStyle
prettySeq seq' =
  let l = toList seq' in
    case l of
      [] -> pretty "[ ]"
      _  ->
        pretty "["
        <+>
        foldl1
          (\ac newValue -> ac <> pretty "," <+> newValue)
          (prettyValue <$> l)
        <+>
        pretty "]"

p :: Rational -> (Scientific, Maybe Int)
p = fromRationalRepetendUnlimited


prettyBytes :: ByteString ->Doc AnsiStyle
prettyBytes byteString =
  let bytes = unpack byteString in
  case bytes of
    [] -> pretty "[# #]"
    _  ->
      pretty "[#"
      <+>
      foldl1
        (\ac newValue -> ac <> pretty " " <> newValue)
        (bytes
        <&> (\w ->
          let (fb, sb) = quotRem (fromEnum w) 16 in
            pretty (toHexDigit fb)
            <>
            pretty (toHexDigit sb)))
      <+>
      pretty "#]"

data NumberContent =
  IntegerContent Integer
  | FractionContent Integer Integer
  | DecimalContent Scientific
  | MixedContent Integer Integer Integer

toHexDigit :: Int -> Char
toHexDigit n
  | 0 <= n && n <= 9 = chr $ ord '0' + n
  | 10 <= n && n <= 15 = chr $ ord 'a' + (n - 10)
  | otherwise = '-'

toNumberContent :: Rational -> NumberContent
toNumberContent q =
  let (mp, rep) = fromRationalRepetendUnlimited q in
  case rep of
    Nothing ->
      let rmp = toRational mp in
        let (n, m) = (numerator rmp, denominator rmp) in
          let (k, r) = quotRem n m in
            if r == 0 then IntegerContent k else DecimalContent mp
    Just _ ->
      let (n, m) = (numerator q, denominator q) in
        let (k, r) = quotRem n m in
          if r == 0 then IntegerContent k else
          if k == 0 then FractionContent n m
            else MixedContent k r m

