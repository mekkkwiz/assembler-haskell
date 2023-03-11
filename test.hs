import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char


type Register = Int
type Label = String
type SimbolicAddr = String

type Offset = Either SimbolicAddr Int

type SymbolTable = [(Label,Int)]

data Instruction =  RType (Maybe Label) Register Register Register
                  | IType (Maybe Label) Register Register Offset
                  | JType (Maybe Label) Register Register
                  | OType (Maybe Label)

-- for make show can print Instruction
instance Show Instruction where
  show (RType (Just label) regA regB destReg) = "R type " ++ label ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ show destReg ++ "]"
  show (IType (Just label) regA regB offset) = "I type " ++ label ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ showOffset offset ++ "]"
  show (JType (Just label) regA regB) = "J type " ++ label ++ " [" ++ show regA ++ " " ++ show regB ++ "]"
  show (OType (Just label)) = "O type " ++ label
  show (RType Nothing regA regB destReg) = "R type - [" ++ show regA ++ " " ++ show regB ++ " " ++ show destReg ++ "]"
  show (IType Nothing regA regB offset) = "I type - [" ++ show regA ++ " " ++ show regB ++ " " ++ showOffset offset ++ "]"
  show (JType Nothing regA regB) = "J type - [" ++ show regA ++ " " ++ show regB ++ "]"
  show (OType Nothing) = "O type -"

showOffset :: Offset -> String
showOffset (Left symAddr) = symAddr
showOffset (Right intOffset) = show intOffset

-- IShownstruction Set
opcodes :: Map String String
opcodes = Map.fromList [("add", "000"), ("nand", "001"), ("lw", "010"),
                        ("sw", "011"), ("beq", "100"), ("jalr", "101"),
                        ("halt", "110"), ("noop", "111")]


addToSymbolTable :: SymbolTable -> Label -> Int -> SymbolTable
addToSymbolTable symTable label addr = (label, addr) : symTable

-- Convert instruction into machine code
-- toMachineCode :: SymbolTable -> Instruction -> Maybe Int
-- toMachineCode symTable (RType op rs rt rd) =
--   case Map.lookup op opcodes of
--     Just opcode -> Just $ read (opcode ++ show rs ++ show rt ++ "000" ++ show rd)
--     Nothing -> Nothing

-- toMachineCode symTable (IType op rs rt offset) =
--   case Map.lookup op opcodes of
--     Just opcode -> Just $ read (opcode ++ show rs ++ show rt ++ padOffset offset)
--     Nothing -> Nothing

-- toMachineCode symTable (JType op rs rd) =
--   case Map.lookup op opcodes of
--     Just opcode -> Just $ read (opcode ++ show rs ++ "000" ++ show rd ++ "000000000000")
--     Nothing -> Nothing

-- toMachineCode symTable (OType op) =
--   case Map.lookup op opcodes of
--     Just opcode -> Just $ read (opcode ++ "000000000000000000000000")
--     Nothing -> Nothing

-- -- Pad the offset field to 16 bits
-- padOffset :: Offset -> String
-- padOffset offset
--   | offset >= 0 = padLeft '0' 16 (decToBin offset)
--   | otherwise = padLeft '1' 16 (decToBin (abs offset - 1))

-- Convert decimal number to binary string
decToBin :: Int -> String
decToBin 0 = "0"
decToBin n = reverse $ go n
  where
    go 0 = ""
    go n = let (q, r) = n `divMod` 2 in return (intToDigit r) ++ go q

binToDec :: String -> Int
binToDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

-- Pad a string with a character on the left
padLeft :: Char -> Int -> String -> String
padLeft c n str = replicate (n - length str) c ++ str

-- Parse an assembly instruction
parseInstruction :: SymbolTable -> String -> Instruction
parseInstruction symTable instr =
  case words instr of
    (label : instrName : args) ->
      if instrName `elem` Map.keys opcodes
      then parseWithLabel symTable label instrName args
      else parseWithoutLabel symTable label (instrName : args)
    _ -> error "Invalid instruction"

parseWithLabel :: SymbolTable -> Label -> String -> [String] -> Instruction
parseWithLabel symTable label instrName args =
  case instrName of
    "add" -> RType (Just label) (parseReg $ head args) (parseReg $ args !! 1) (parseReg $ args !! 2)
    "nand" -> RType (Just label) (parseReg $ head args) (parseReg $ args !! 1) (parseReg $ args !! 2)
    "lw" -> IType (Just label) (parseReg $ head args) (parseReg $ args !! 2) (parseOffset symTable $ args !! 1)
    "sw" -> IType (Just label) (parseReg $ head args) (parseReg $ args !! 2) (parseOffset symTable $ args !! 1)
    "beq" -> IType (Just label) (parseReg $ head args) (parseReg $ args !! 1) (parseOffset symTable $ args !! 2)
    "jalr" -> JType (Just label) (parseReg $ head args) (parseReg $ args !! 1)
    "halt" -> OType (Just label)
    "noop" -> OType (Just label)
    _ -> error ("Invalid opcode: parseWithLabel - " ++ instrName)

parseWithoutLabel :: SymbolTable -> String -> [String] -> Instruction
parseWithoutLabel symTable instrName args =
  case instrName of
    "add" -> RType Nothing (parseReg $ head args) (parseReg $ args !! 1) (parseReg $ args !! 2)
    "nand" -> RType Nothing (parseReg $ head args) (parseReg $ args !! 1) (parseReg $ args !! 2)
    "lw" -> IType Nothing (parseReg $ head args) (parseReg $ args !! 1) (parseOffset symTable $ args !! 2)
    "sw" -> IType Nothing (parseReg $ head args) (parseReg $ args !! 1) (parseOffset symTable $ args !! 2)
    "beq" -> IType Nothing (parseReg $ head args) (parseReg $ args !! 1) (parseOffset symTable $ args !! 2)
    "jalr" -> JType Nothing (parseReg $ head args) (parseReg $ args !! 1)
    _ -> error ("Invalid opcode: parseWithoutLabel - " ++ instrName)


parseReg :: String -> Register
parseReg regStr = read [last regStr]

parseOffset :: SymbolTable -> String -> Offset
parseOffset symTable offsetStr
  | all isDigit (dropWhile (== '-') offsetStr) = Right (read offsetStr) -- if input is a number, return it
  | otherwise =
    case lookup offsetStr symTable of -- lookup offsetStr in the symbol table
      Just val -> Right val           -- if found, return its value
      Nothing -> error ("Invalid offset: " ++ offsetStr ++ " don't found in given symbol table") -- if not found, return an error



-- -- Parse label and compute its offset from the current instruction
-- parseLabelOffset :: SymbolTable -> String -> Offset
-- parseLabelOffset symTable label =
--   case Map.lookup label symTable of
--     Just offset -> offset
--     Nothing -> error ("Undefined label: " ++ label)

-- -- Parse label or value for .fill instruction
-- parseLabelOrValue :: SymbolTable -> String -> Offset
-- parseLabelOrValue symTable val =
--   case Map.lookup val symTable of
--     Just offset -> offset
--     Nothing -> read val

-- makeSymbolTable :: [String] -> Map.Map String Int
-- makeSymbolTable instrs = go instrs 0 Map.empty
--   where
--     go [] _ symTable = symTable
--     go (instr:instrs) addr symTable =
--       let (label, rest) = break (==':') instr
--           symTable' = if null label
--                         then symTable
--                         else Map.insert (init label) addr symTable
--       in go instrs (addr + 1) symTable'


-- -- Convert assembly code into machine code
-- assemble :: String -> Maybe [Int]
-- assemble code =
--   let instrs = lines code
--       parsedInstrs = map (parseInstruction symbolTable) instrs
--       machineCode = mapMaybe (fmap (toMachineCode symbolTable)) parsedInstrs
--   in (sequence machineCode Control.Applicative.<|> Nothing)
--   where
--     symbolTable = makeSymbolTable (lines code)

-- -- Example assembly code
-- exampleCode = unlines [
--     "lw 0 5 pos1",
--     "lw 0 1 input",
--     "lw 0 2 subAd",
--     "jalr 2 4",
--     "lw 0 5 pos1",
--     "lw 0 1 input",
--     "lw 0 2 subAd",
--     "jalr 2 4",
--     "halt",
--     "sub4n sw 7 4",
--     "add 7 5 7",
--     "sw 7 1 stack",
--     "add 7 5 7",
--     "add 1 1 1",
--     "add 1 1 3",
--     "lw 0 2 neg1",
--     "add 7 2 7",
--     "lw 7 1 stack",
--     "add 7 2 7",
--     "lw 7 4 stack",
--     "jalr 4 2",
--     "pos1 .fill 1",
--     "neg1 .fill -1",
--     "subAdr .fill sub4n",
--     "input .fill 10",
--     "stack .fill 0"
--   ]

-- main :: IO ()
-- main = do
--   let inputPath = "./source/testfiles/demofile copy.txt"
--   let outputPath = "./output/machine_code.txt"
--   code <- assembleProgram inputPath
--   writeMachineCodeToFile outputPath code
--   putStrLn "Assembly complete. Machine code written to file."
