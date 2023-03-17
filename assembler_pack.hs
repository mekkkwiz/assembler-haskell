-- PACK
import Data.Maybe
import Data.Map (Map)
import Text.Read
import qualified Data.Map as Map
type Label = String
type Value = String
type AssemblyLine = (Maybe Label, String, String, String, String)
type SymbolTable = [(Label,Int)]

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
                    -- then Just $ addToSymbolTable [] label (fst pairs)
                    then case readMaybe value :: Maybe Int of
                      Just _ ->  Just $ addToSymbolTable [] label (fst pairs)
                      -- Nothing -> Nothing
                      Nothing -> Just $ addToSymbolTable [] (label++","++value) (-1)
                    else Nothing
        _ -> Nothing

maybeToSym :: [Maybe b] -> [b]
maybeToSym [] = []
maybeToSym x = map fromJust (filter (isJust) x)

setSymbolTable :: Foldable t => t (Int, String) -> [SymbolTable]
setSymbolTable l = maybeToSym $ foldr (\x acc -> setFillToSymbol x : acc) [] l


setSymbolTable' :: [String] -> SymbolTable -> Int -> SymbolTable
setSymbolTable' codeLines symTable currentLine = foldl updateSymTable symTable (zip codeLines [currentLine..])
  where
    updateSymTable :: SymbolTable -> (String, Int) -> SymbolTable
    updateSymTable symTable (codeLine, currentLine) =
      let wordsInLine = words codeLine
          updatedSymTable =
            if isLabel codeLine
            then case wordsInLine of
              [label, ".fill", value] ->                                       -- .fill label value
                case lookup value symTable of                                  -- check did value is in symbol table
                  Just realValue ->                                            -- if value is in symbol table, add label with real value to symbol table
                    addToSymbolTable symTable label realValue                  -- and return updated symbol table
                  Nothing -> addToSymbolTable symTable label (read value)      -- if value is not in symbol table, add label with value to symbol table
              _ -> addToSymbolTable symTable (head wordsInLine) currentLine    -- if line is not .fill, add label with current line to symbol table
            else symTable                                                      -- if line is not label, return symbol table
      in updatedSymTable

    isLabel :: String -> Bool
    isLabel instr =
      case words instr of
        (label : instrName : _) ->
          (instrName == ".fill") || (instrName `elem` Map.keys opcodes)
        [instrName] ->
          instrName `notElem` ["halt", "noop"]
        _ -> False

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
--   let symTable = setSymbolTable pairsCode []
  -- let symTable =  setSymbolTable pairsCode
  let symTable =  setSymbolTable' exampleCode [] 0
--   let sym = map fromJust (filter (isJust) symTable)

  mapM_ print symTable
