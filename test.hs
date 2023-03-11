import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Data.Maybe


type Register = Int
type Label = String
type Opcode = String
type SimbolicAddr = String

type Offset = Either SimbolicAddr Int

type SymbolTable = [(Label,Int)]

data Instruction =  Add (Maybe Label) Opcode Register Register Register
                  | Nand (Maybe Label) Opcode Register Register Register
                  | Lw (Maybe Label) Opcode Register Register Offset
                  | Sw (Maybe Label) Opcode Register Register Offset
                  | Beq (Maybe Label) Opcode Register Register Offset
                  | Jalr (Maybe Label) Opcode Register Register
                  | Halt (Maybe Label) Opcode
                  | Noop (Maybe Label) Opcode

-- for make show can print Instruction
instance Show Instruction where
  show (Add (Just label) opcode regA regB destReg) = "Add (" ++ opcode ++ ") " ++ label ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ show destReg ++ "]"
  show (Nand (Just label) opcode regA regB destReg) = "Nand (" ++ opcode ++ ") " ++ label ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ show destReg ++ "]"
  show (Lw (Just label) opcode regA regB offset) = "Lw (" ++ opcode ++ ") " ++ label ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ showOffset offset ++ "]"
  show (Sw (Just label) opcode regA regB offset) = "Sw (" ++ opcode ++ ") " ++ label ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ showOffset offset ++ "]"
  show (Beq (Just label) opcode regA regB offset) = "Beq (" ++ opcode ++ ") " ++ label ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ showOffset offset ++ "]"
  show (Jalr (Just label) opcode regA regB) = "Jalr (" ++ opcode ++ ") " ++ label ++ " [" ++ show regA ++ " " ++ show regB ++ "]"
  show (Halt (Just label) opcode) = "Halt (" ++ opcode ++ ") " ++ label
  show (Noop (Just label) opcode) = "Noop (" ++ opcode ++ ") " ++ label
  show (Add Nothing opcode regA regB destReg) = "Add (" ++ opcode ++ ") -" ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ show destReg ++ "]"
  show (Nand Nothing opcode regA regB destReg) = "Nand (" ++ opcode ++ ") -" ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ show destReg ++ "]"
  show (Lw Nothing opcode regA regB offset) = "Lw (" ++ opcode ++ ") -" ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ showOffset offset ++ "]"
  show (Sw Nothing opcode regA regB offset) = "Sw (" ++ opcode ++ ") -" ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ showOffset offset ++ "]"
  show (Beq Nothing opcode regA regB offset) = "Beq (" ++ opcode ++ ") -" ++ " [" ++ show regA ++ " " ++ show regB ++ " " ++ showOffset offset ++ "]"
  show (Jalr Nothing opcode regA regB) = "Jalr (" ++ opcode ++ ") -" ++ " [" ++ show regA ++ " " ++ show regB ++ "]"
  show (Halt Nothing opcode)  = "Halt (" ++ opcode ++ ") -"
  show (Noop Nothing opcode) = "Noop (" ++ opcode ++ ") -"


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

toMachineCode :: SymbolTable -> Instruction -> Maybe Int
toMachineCode symTable instr = case instr of
  (Add label opcode regA regB destReg) -> encodeRType label opcode regA regB destReg
  (Nand label opcode regA regB destReg) -> encodeRType label opcode regA regB destReg
  (Lw label opcode regA regB offset) -> encodeIType label opcode regA regB offset
  (Sw label opcode regA regB offset) -> encodeIType label opcode regA regB offset
  (Beq label opcode regA regB offset) -> encodeIType label opcode regA regB offset
  (Jalr label opcode regA regB) -> encodeJType label opcode regA regB
  (Halt label opcode) -> encodeOType label opcode
  (Noop label opcode) -> encodeOType label opcode
  where
    encodeRType label opcode regA regB destReg =
      Just $ binToDec $ opcode ++
                        padLeft '0' 3 (decToBin regA) ++
                        padLeft '0' 3 (decToBin regB) ++
                        padLeft '0' 13 "" ++
                        padLeft '0' 3 (decToBin destReg)
    encodeIType label opcode regA regB offset =
      let padOffset = padLeft '0' 16 . decToBin $ either (fromJust . flip lookup symTable) id offset
      in Just $ binToDec $  opcode ++
                            padLeft '0' 3 (decToBin regA) ++
                            padLeft '0' 3 (decToBin regB) ++
                            padOffset
    encodeJType label opcode regA regB =
      Just $ binToDec $ opcode ++
                        padLeft '0' 3 (decToBin regA) ++
                        padLeft '0' 3 (decToBin regB) ++
                        padLeft '0' 16 ""
    encodeOType label opcode =
      Just $ binToDec $ opcode ++ padLeft '0' 22 ""


-- Convert decimal number to binary string
decToBin :: Int -> String
decToBin 0 = "0"
decToBin n
  | n >= 0    = toBinary n
  | otherwise = padLeft '1' 16 $ toBinary (2^16 + n)
  where
    toBinary 0 = "0"
    toBinary n = reverse $ go n
    go 0 = ""
    go n = let (q, r) = n `divMod` 2 in return (intToDigit r) ++ go q




binToDec :: String -> Int
binToDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

-- Pad a string with a character on the left
padLeft :: Char -> Int -> String -> String
padLeft c n str = replicate (n - length str) c ++ str

-- Parse an assembly instruction
parseInstruction :: SymbolTable -> String -> Maybe Instruction
parseInstruction symTable instr =
  case words instr of
    (label : instrName : args) ->
      if instrName == ".fill" then Nothing
      else  if instrName `elem` Map.keys opcodes
            then Just $ parseWithLabel symTable label instrName args
            else Just $ parseWithoutLabel symTable label (instrName : args)
    [instrName] ->
      if instrName `elem` ["halt","noop"]
      then Just $ parseWithoutLabel symTable instrName []
      else error "Invalid instruction"
    _ -> error "Invalid instruction"

parseWithLabel :: SymbolTable -> Label -> String -> [String] -> Instruction
parseWithLabel symTable label instrName args =
  let opcode = Map.lookup instrName opcodes
  in fromMaybe (error ("Invalid opcode: parseWithLabel - " ++ instrName)) $
        case instrName of
          "add"  -> fmap (\op -> Add (Just label) op (parseReg $ head args) (parseReg $ args !! 1) (parseReg $ args !! 2)) opcode
          "nand" -> fmap (\op -> Nand (Just label) op (parseReg $ head args) (parseReg $ args !! 1) (parseReg $ args !! 2)) opcode
          "lw"   -> fmap (\op -> Lw (Just label) op (parseReg $ head args) (parseReg $ args !! 1) (parseOffset symTable $ args !! 2)) opcode
          "sw"   -> fmap (\op -> Sw (Just label) op (parseReg $ head args) (parseReg $ args !! 1) (parseOffset symTable $ args !! 2)) opcode
          "beq"  -> fmap (\op -> Beq (Just label) op (parseReg $ head args) (parseReg $ args !! 1) (parseOffset symTable $ args !! 2)) opcode
          "jalr" -> fmap (\op -> Jalr (Just label) op (parseReg $ head args) (parseReg $ args !! 1)) opcode
          "halt" -> Just $ Halt (Just label) (fromJust opcode)
          "noop" -> Just $ Noop (Just label) (fromJust opcode)
          _      -> Nothing

parseWithoutLabel :: SymbolTable -> String -> [String] -> Instruction
parseWithoutLabel symTable instrName args =
  let opcode = Map.lookup instrName opcodes
  in fromMaybe (error ("Invalid opcode: parseWithoutLabel - " ++ instrName)) $
        case instrName of
          "add"  -> fmap (\op -> Add Nothing op (parseReg $ head args) (parseReg $ args !! 1) (parseReg $ args !! 2)) opcode
          "nand" -> fmap (\op -> Nand Nothing op (parseReg $ head args) (parseReg $ args !! 1) (parseReg $ args !! 2)) opcode
          "lw"   -> fmap (\op -> Lw Nothing op (parseReg $ head args) (parseReg $ args !! 1) (parseOffset symTable $ args !! 2)) opcode
          "sw"   -> fmap (\op -> Sw Nothing op (parseReg $ head args) (parseReg $ args !! 1) (parseOffset symTable $ args !! 2)) opcode
          "beq"  -> fmap (\op -> Beq Nothing op (parseReg $ head args) (parseReg $ args !! 1) (parseOffset symTable $ args !! 2)) opcode
          "jalr" -> fmap (\op -> Jalr Nothing op (parseReg $ head args) (parseReg $ args !! 1)) opcode
          "halt" -> Just $ Halt Nothing (fromJust opcode)
          "noop" -> Just $ Noop Nothing (fromJust opcode)
          _      -> Nothing


parseReg :: String -> Register
parseReg regStr = read [last regStr]

parseOffset :: SymbolTable -> String -> Offset
parseOffset symTable offsetStr
  | all isDigit (dropWhile (== '-') offsetStr) = Right (read offsetStr) -- if input is a number, return it
  | otherwise =
    case lookup offsetStr symTable of -- lookup offsetStr in the symbol table
      Just val -> Right val           -- if found, return its value
      Nothing -> error ("Invalid offset: " ++ offsetStr ++ " don't found in given symbol table") -- if not found, return an error


testTran :: SymbolTable -> [Instruction] -> IO ()
testTran symTable instrs = do
  let machineCodes = map (toMachineCode symTable) instrs
  mapM_ print machineCodes

-- Define the types and functions needed for parsing instructions

main :: IO ()
main = do
  let exampleCode = [
        "lw 0 5 pos1",
        "lw 0 1 input",
        "lw 0 2 subAdr",
        "jalr 2 4",
        "lw 0 5 pos1",
        "lw 0 1 input",
        "lw 0 2 subAdr",
        "jalr 2 4",
        "halt",
        "sub4n sw 7 4 stack",
        "add 7 5 7",
        "sw 7 1 stack",
        "add 7 5 7",
        "add 1 1 1",
        "add 1 1 3",
        "lw 0 2 neg1",
        "add 7 2 7",
        "lw 7 1 stack",
        "add 7 2 7",
        "lw 7 4 stack",
        "jalr 4 2",
        "pos1 .fill 1",
        "neg1 .fill -1",
        "subAdr .fill sub4n",
        "input .fill 10",
        "stack .fill 0" ]

      symTable = [
        ("pos1", 21),
        ("neg1", 22),
        ("sub4n", 9),
        ("input", 24),
        ("stack", 25),
        ("subAdr", 23)]

  let linesOfCode = map (show . parseInstruction symTable) exampleCode
  -- print (length linesOfCode)
  mapM_ putStrLn linesOfCode

  let instructions = map (parseInstruction symTable) exampleCode
  testTran symTable (catMaybes instructions)




-- ["lw 0 5 pos1","lw 0 1 input","lw 0 2 subAd","jalr 2 4","lw 0 5 pos1","lw 0 1 input","lw 0 2 subAd","jalr 2 4","halt","sub4n sw 7 4 stack","add 7 5 7","sw 7 1 stack","add 7 5 7","add 1 1 1","add 1 1 3","lw 0 2 neg1","add 7 2 7","lw 7 1 stack","add 7 2 7","lw 7 4 stack","jalr 4 2","pos1 .fill 1","neg1 .fill -1","subAdr .fill sub4n","input .fill 10","stack .fill 0"]