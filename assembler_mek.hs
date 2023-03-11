-- MEK
-- checkSameLable
-- addLabelinList
-- __fillFinding
-- translator
-- __simplify

import Data.Char
import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Numeric

-- | The 'Inst' type represents an instruction in the assembly language
data Inst = Inst
  { instType :: String,
    optcBin :: String
  }
  deriving (Show)

-- | The instruction set, represented as a map from instruction names to 'Inst' values
instructionSet :: Map String Inst
instructionSet =
  Map.fromList
    [ ("add", Inst "R" "000"),
      ("nand", Inst "R" "001"),
      ("lw", Inst "I" "010"),
      ("sw", Inst "I" "011"),
      ("beq", Inst "I" "100"),
      ("jalr", Inst "J" "101"),
      ("halt", Inst "O" "110"),
      ("noop", Inst "O" "111")
    ]

-- | The 'AssemblyLine' type represents a single line of assembly code
data AssemblyLine = AssemblyLine
  { label :: Maybe String, -- The label, if any
    opcode :: String, -- The opcode (instruction name)
    operands :: [String] -- The operands
  }
  deriving (Show)

-- | Parses a single line of assembly code into an 'AssemblyLine' value
parseAssemblyLine :: String -> AssemblyLine
parseAssemblyLine line = case words line of
  [] -> AssemblyLine Nothing "" []
  (w : ws) ->
    if w == ".fill"
      then AssemblyLine Nothing w ws
      else case Map.lookup w instructionSet of
        Just _ -> AssemblyLine Nothing w ws
        Nothing -> AssemblyLine (Just w) (head ws) (tail ws)

-- | Parses an entire assembly program into a list of 'AssemblyLine' values
parseAssemblyProgram :: String -> [AssemblyLine]
parseAssemblyProgram = map parseAssemblyLine . lines

-- | Converts a decimal integer to binary
decToBin :: Int -> String
decToBin x = showIntAtBase 2 intToDigit x ""

-- | Converts a decimal integer to binary, using two's complement representation
twosComDecToBin dec =
  padToLength 16 $
    if dec >= 0
      then decToBin dec
      else case reads binStr :: [(Int, String)] of
        [(num, "")] -> decToBin (2 ^ 16 + num)
        _ -> error "Invalid binary representation"
  where
    binStr = decToBin (2 ^ 16 + dec)

-- | Pads a string to a specified length with leading zeroes
padToLength :: Int -> String -> String
padToLength len str = replicate (len - length str) '0' ++ str

-- | Converts an 'AssemblyLine' to its corresponding machine language representation
assembleLine :: AssemblyLine -> String
assembleLine (AssemblyLine _ ".fill" [value]) = twosComDecToBin $ read value
assembleLine (AssemblyLine Nothing op operands) = case Map.lookup op instructionSet of
  Just (Inst "R" instcode) -> instcode ++ rs ++ rt ++ rd ++ "00000000000" where
      [rs, rt, rd] = map (padToLength 3 . decToBin . read) operands
  Just (Inst "I" instcode) -> instcode ++ rs ++ rt ++ imm
    where
      rs = case operands of
        (r : _) -> padToLength 3 $ decToBin $ read r
        [] -> error "Not enough operands for instruction"
      rt = case drop 1 operands of
        (t : _) -> padToLength 3 $ decToBin $ read t
        [] -> error "Not enough operands for instruction"
      imm = twosComDecToBin $ read (last operands)
  Just (Inst "J" instcode) -> instcode ++ rs ++ "0000000000000"
    where
      rs = padToLength 3 $ decToBin $ read (head operands)
  Just (Inst "O" instcode) -> instcode ++ "0000000000000000"
  Nothing -> error $ "Unknown opcode: " ++ op

-- | Assembles an entire assembly program into a machine code binary string
assembleProgram :: FilePath -> IO String
assembleProgram path = readFile path <&> (unlines . map assembleLine . parseAssemblyProgram)

-- | Writes the assembled machine code binary string to a file
writeMachineCodeToFile :: FilePath -> String -> IO ()
writeMachineCodeToFile = writeFile

-- | Main function that assembles the program and writes the output to a file
main :: IO ()
main = do
  let inputPath = "./source/testfiles/demofile copy.txt"
  let outputPath = "./output/machine_code.txt"
  code <- assembleProgram inputPath
  writeMachineCodeToFile outputPath code
  putStrLn "Assembly complete. Machine code written to file."