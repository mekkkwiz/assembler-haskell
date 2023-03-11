-- PACK
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
type Label = String
type Value = String
type AssemblyLine = (Maybe Label, String, String, String, String)
type SymbolTable = [(Label,Int)]

-- data AssemblyTranslator = AssemblyTranslator {
--     machineLang :: [String],        -- All instructions in assembly that have been translated to machine language
--     fillValue :: [SymbolTable],        -- Collect all .fill variables and make list of pairs [(symbolic, values)]
--     assembly :: [AssemblyLine],     -- Save all instructions in assembly line by line
--     labelList :: [Label],           -- All labels in the assembly file
--     errorDetect :: Bool,            -- Whether an error was detected in the assembly file
--     errorDetail :: String           -- Details of the error detected
-- }

opcodes :: Map String String
opcodes = Map.fromList [("add", "000"), ("nand", "001"), ("lw", "010"),
                        ("sw", "011"), ("beq", "100"), ("jalr", "101"),
                        ("halt", "110"), ("noop", "111")]


-- stringReader
stringReader = do  
        contents <- readFile "source/testfiles/demofile.txt"
        putStrLn contents

-- __binToDec
binToDec :: Integral i => i -> Maybe i
binToDec 0 = Just 0
binToDec i | last < 2 =  (fmap (\x -> 2*x + last) (binToDec (div i 10)))
           | otherwise = error "it's not a binary"
           where last = mod i 10

addToSymbolTable :: SymbolTable -> Label -> Int -> SymbolTable
addToSymbolTable symTable label addr = (label, addr) : symTable

setFillToSymbol :: (Int, String) -> Maybe SymbolTable
setFillToSymbol pairs = 
    case words (snd pairs) of
        (label : instruction : value : _) ->
            if instruction `elem` Map.keys opcodes
                then Just $ addToSymbolTable [] label (fst pairs)
                else if instruction == ".fill"
                    then Just $ addToSymbolTable [] label (fst pairs)
                    else Nothing
        _ -> Nothing
                



main :: IO ()
main = do
  let exampleCode = [
        "lw 0 5 pos1",
        "lw 0 1 input",
        "lw 0 2 subAd",
        "jalr 2 4",
        "lw 0 5 pos1",
        "lw 0 1 input",
        "lw 0 2 subAd",
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

  let pairsCode = zip [0..] exampleCode
  let symTable =  foldr (\x acc -> setFillToSymbol x : acc) [] pairsCode

  mapM_ (putStrLn . show) symTable