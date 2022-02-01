import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck as QC (testProperty, (==>))
import Test.Tasty.SmallCheck as SC (testProperty, (==>))

import Control.Exception (SomeException, try)
import Data.List (sort)
import Data.Ord ()
import Data.Text.Prettyprint.Doc (Pretty (pretty))
import Data.Void (Void)
import GHC.Exts (fromList)
import HW3.Action (HIO (runHIO), HiPermission (AllowRead, AllowTime, AllowWrite))
import HW3.Base (HiError, HiExpr (..), HiFun (..), HiValue (..), convertToOperation,
                 listOfAllOperators)
import HW3.Evaluator (eval)
import HW3.Parser (getErrorMessage, parse)
import HW3.Pretty (prettyValue)
import System.Directory (removeDirectoryRecursive)
import Text.Megaparsec (ParseErrorBundle (ParseErrorBundle))

main :: IO ()
main = do
  defaultMain tests
  removeDirectoryRecursive "tmp"

tests :: TestTree
tests = testGroup "Tests"
  [
      parseFunctionNamesTests
    , parseArithmeticFunctionsTests
    , numbersTests
    , booleanTests
    , infixTests
    , stringTests
    , bytesTests
    , fileIOTests
    , lazyTests
    , dictTests
  ]

type ParseResult = Either (ParseErrorBundle String Void) HiExpr

parseTest :: String -> String -> ParseResult -> TestTree
parseTest name input expectedResult =
  testCase name $ parse input @?= expectedResult

parseTestIO :: String -> String -> TestTree
parseTestIO input output =
  testCase
    (input ++ " = " ++ output)
    (case parse input of
      Left bundle -> assertFailure ("not parsed, reason:" ++ getErrorMessage bundle)
      Right expr -> do
        value <- evaluation expr
        case value of
          Left exp          -> assertFailure ("not evaluated, reason" ++ show exp)
          Right (Left err)  -> assertFailure ("not evaluated, reason" ++ show err)
          Right (Right val) -> show (prettyValue val) @?= show (pretty output)
        )

parseFunctionNamesTests :: TestTree
parseFunctionNamesTests = testGroup "Parsing of functions names test" $
  let toFunExpr = fun . convertToOperation in
    (\f -> parseTest f f $ Right . toFunExpr $ f)
    <$> listOfAllOperators


parseArithmeticFunctionsTests :: TestTree
parseArithmeticFunctionsTests = testGroup "Parsing of arithmetic functions test" $
  let toFunExpr = fun . convertToOperation
      expr = (++ "()") in
    (\f -> parseTest
      (expr f)
      (expr f) $
      Right (funApply (convertToOperation f) []))
    <$> [
        "add"
      , "sub"
      , "mul"
      , "div"
    ]

numbersTests :: TestTree
numbersTests = testGroup "Simple Calculations tests"
  [
    parseTestIO "4" "4",
    parseTestIO "5.3" "5.3",
    parseTestIO "div(3, 4)" "0.75",
    parseTestIO "div(3, 7)" "3/7",
    parseTestIO "add(3, 4)" "7",
    parseTestIO "sub(10, 3)" "7",
    parseTestIO "mul(2, 3)" "6",
    parseTestIO "div(10, 3)" "3 + 1/3",
    parseTestIO "add(3, div(14, 100))" "3.14",
    parseTestIO "sub(mul(201, 11), 0.33)" "2210.67"
  ]

booleanTests :: TestTree
booleanTests = testGroup "Simple tests over boolean calculations"
  [
    parseTestIO "false" "false",
    parseTestIO "equals(add(2, 2), 4)" "true",
    parseTestIO "less-than(mul(999, 99), 10000)" "false",
    parseTestIO "if(greater-than(div(2, 5), div(3, 7)), 1, -1)" "-1",
    parseTestIO "and(less-than(0, 1), less-than(1, 0))" "false",
    parseTestIO "if(true, add, mul)" "add",
    parseTestIO "if(true, add, mul)(10, 10)" "20",
    parseTestIO "if(false, add, mul)(10, 10)" "100",
    parseTestIO "equals(add, add)" "true",
    parseTestIO "equals(add, mul)" "false"
  ]

infixTests :: TestTree
infixTests = testGroup "Simple tests over infix operations"
  [
    parseTestIO "2 + 2" "4",
    parseTestIO "2 + 2 * 3" "8",
    parseTestIO "((2 + 2) * 3)" "12",
    parseTestIO "2 + 2 * 3 == (2 + 2) * 3" "false",
    parseTestIO "10 == 2*5 && 143 == 11*13" "true"
  ]

stringTests :: TestTree
stringTests = testGroup "Simple strings and slices tests"
  [
    parseTestIO "length(\"Hello World\")" "11",
    parseTestIO "to-upper(\"Hello World\")" "\"HELLO WORLD\"",
    parseTestIO "to-lower(\"Hello World\")" "\"hello world\"",
    parseTestIO "reverse(\"stressed\")" "\"desserts\"",
    parseTestIO "trim(\" Hello World \")" "\"Hello World\"",
    parseTestIO "\"Hello\" + \"World\"" "\"HelloWorld\"",
    parseTestIO "\"Cat\" * 5" "\"CatCatCatCatCat\"",
    parseTestIO "\"/home/user\" / \"hi\"" "\"/home/user/hi\"",
    parseTestIO "\"Hello World\"(0)" "\"H\"",
    parseTestIO "\"Hello World\"(7)" "\"o\"",
    parseTestIO "\"Hello World\"(-1)" "null",
    parseTestIO "\"Hello World\"(99)" "null",
    parseTestIO "\"Hello World\"(0, 5)" "\"Hello\"",
    parseTestIO "\"Hello World\"(2, 4)" "\"ll\"",
    parseTestIO "\"Hello World\"(0, -4)" "\"Hello W\"",
    parseTestIO "\"Hello World\"(-4, -1)" "\"orl\"",
    parseTestIO "\"Hello, World\"(2, null)" "\"llo, World\"",
    parseTestIO "\"Hello, World\"(null, 5)" "\"Hello\"",
    parseTestIO "to-upper(\"what a nice language\")(7, 11)" "\"NICE\"",
    parseTestIO "\"Hello\" == \"World\"" "false",
    parseTestIO "length(\"Hello\" + \"World\")" "10",
    parseTestIO "length(\"hehe\" * 5) / 3" "6 + 2/3"
  ]

listTests :: TestTree
listTests = testGroup "Simple lists tests"
  [
    parseTestIO "list(1, 2, 3)" "[ 1, 2, 3 ]",
    parseTestIO "range(5, 10.3)" "[ 5, 6, 7, 8, 9, 10 ]",
    parseTestIO "fold(add, [11, 22, 33])" "66",
    parseTestIO "fold(mul, [11, 22, 33])" "7986",
    parseTestIO "fold(div, [11, 22, 33])" "1/66",
    parseTestIO "length([1, true, \"Hello\"])" "3",
    parseTestIO "reverse([1, true, \"Hello\"])" "[ \"Hello\", true, 1 ]",
    parseTestIO "[1, 2] + [3, 4, 5]" "[1, 2, 3, 4, 5]",
    parseTestIO "[0, \"x\"] * 3" "[ 0, \"x\", 0, \"x\", 0, \"x\" ]",
    parseTestIO "[\"hello\", true, \"world\"](1)" "true",
    parseTestIO "[\"hello\", true, \"world\"](1, 3)" "[ true, \"world\" ]",
    parseTestIO "fold(add, [2, 5] * 3)" "21",
    parseTestIO "fold(mul, range(1, 10))" "3628800",
    parseTestIO "[0, true, false, \"hello\", \"world\"](2, 4)" "[ false, \"hello\" ]",
    parseTestIO "reverse(range(0.5, 70/8))" "[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]"
  ]

bytesTests :: TestTree
bytesTests = testGroup "Simple bytes tests"
  [
    parseTestIO "pack-bytes([ 3, 255, 158, 32 ])" "[# 03 ff 9e 20 #]",
    parseTestIO "unpack-bytes([# 10 20 30 #])" "[ 16, 32, 48 ]",
    parseTestIO "encode-utf8(\"Hello!\")" "[# 48 65 6c 6c 6f 21 #]",
    parseTestIO "decode-utf8([# 48 65 6c 6c 6f #])" "\"Hello\"",
    parseTestIO "decode-utf8([# c3 28 #])" "null",
    parseTestIO "unzip(zip([# 41 e2 f3 #]))" "[# 41 e2 f3 #]",
    parseTestIO "unzip(zip([##]))" "[# #]",
    parseTestIO "deserialise(serialise([1, 2, 3]))" "[ 1, 2, 3 ]",
    parseTestIO "unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])" "[# 01 02 03 #]",
    parseTestIO "[# 00 ff #] + [# 01 e3 #]" "[# 00 ff 01 e3 #]",
    parseTestIO "[# 00 ff #] * 3" "[# 00 ff 00 ff 00 ff #]",
    parseTestIO "[# 00 ff 01 e3 #](1)" "255",
    parseTestIO "[# 00 ff 01 e3 #](1,3)" "[# ff 01 #]"
  ]

fileIOTests :: TestTree
fileIOTests = testGroup "Simple File I/O tests"
  [
    parseTestIO "mkdir(\"tmp\")!" "null",
    parseTestIO "read(\"tmp\")!" "[ ]",
    parseTestIO "mkdir(\"tmp/a\")!" "null",
    parseTestIO "mkdir(\"tmp/b\")!" "null",
    parseTestIO "read(\"tmp\")!" "[ \"a\", \"b\" ]",
    parseTestIO "write(\"tmp/hi.txt\", \"Hello\")!" "null",
    parseTestIO "cd(\"tmp\")!" "null",
    parseTestIO "read(\"hi.txt\")!" "\"Hello\"",
    parseTestIO "read" "read",
    parseTestIO "read(\"hi.txt\")" "read(\"hi.txt\")"
  ]

lazyTests :: TestTree
lazyTests = testGroup "Simple lazy evaluation tests"
  [
    parseTestIO "echo" "echo",
    parseTestIO "echo(\"Hello\")" "echo(\"Hello\")",
    parseTestIO "echo(\"Hello\")!" "null",
    parseTestIO "\"Hello\"(0) || \"Z\"" "\"H\"",
    parseTestIO "\"Hello\"(99) || \"Z\"" "\"Z\"",
    parseTestIO "if(2 == 2, echo(\"OK\")!, echo(\"WTF\")!)" "null",
    parseTestIO "true || echo(\"Don't do this\")!" "true",
    parseTestIO "false && echo(\"Don't do this\")!" "false",
    parseTestIO "[# 00 ff #] && echo(\"Just do it\")!" "null"
  ]

dictTests :: TestTree
dictTests = testGroup "Simple tests for dictionaries"
  [
    parseTestIO "{ \"width\": 120, \"height\": 80 }(\"width\")" "120",
    parseTestIO "keys({ \"width\": 120, \"height\": 80 })" "[ \"height\", \"width\" ]",
    parseTestIO "values({ \"width\": 120, \"height\": 80})" "[ 80, 120 ]",
    parseTestIO "count(\"XXXOX\")" "{ \"O\": 1, \"X\": 4 }",
    parseTestIO "count([# 58 58 58 4f 58 #])" "{ 79: 1, 88: 4 }",
    parseTestIO "count([true, true, false, true])" "{ false: 1, true: 3 }",
    parseTestIO "invert({ \"x\": 1, \"y\" : 2, \"z\": 1 })" "{ 1: [ \"z\", \"x\" ], 2: [ \"y\" ] }",
    parseTestIO "count(\"Hello World\").o" "2",
    parseTestIO
      "invert(count(\"big blue bag\"))"
      "{ 1: [ \"u\", \"l\", \"i\", \"e\", \"a\" ], 2: [ \"g\", \" \" ], 3: [ \"b\" ] }",
    parseTestIO "fold(add, values(count(\"Hello, World!\")))" "13"
  ]

number :: Rational -> HiExpr
number r = HiExprValue (HiValueNumber r)

fun :: HiFun -> HiExpr
fun f = HiExprValue (HiValueFunction f)

funApply :: HiFun -> [HiExpr] -> HiExpr
funApply f = HiExprApply (fun f)

evaluation :: HiExpr -> IO (Either SomeException (Either HiError HiValue))
evaluation expr =
  do
  try
    (runHIO
      (eval expr)
      (fromList
        [
          AllowRead,
          AllowWrite,
          AllowTime
        ]))
    :: IO (Either SomeException (Either HiError HiValue))
