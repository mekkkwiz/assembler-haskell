-- MEK
-- checkSameLable
-- addLabelinList
-- __fillFinding
-- translator
-- __simplify

module AssemblyTranslator (
    translator
) where

import Data.List ( elemIndex, findIndex )
import Numeric ( showIntAtBase )
import Data.Char ( intToDigit )

data Instruction = Instruction {
    name :: [String],
    type_ :: [String],
    optc_bin :: [String]
}

instructions :: Instruction
instructions = Instruction {
    name = ["add", "nand", "lw", "sw", "beq", "jalr", "halt", "noop"],
    type_ = ["R", "R", "I", "I", "I", "J", "O", "O"],
    optc_bin = ["000", "001", "010", "011", "100", "101", "110", "111"]
}

type Label = String
type Value = String
type AssemblyLine = (Maybe Label, String, String, String, String)
type FillPair = (Label, Value)

data AssemblyTranslator = AssemblyTranslator {
    machineLang :: [String],        -- All instructions in assembly that have been translated to machine language
    fillValue :: [FillPair],        -- Collect all .fill variables and make list of pairs [(symbolic, values)]
    assembly :: [AssemblyLine],     -- Save all instructions in assembly line by line
    labelList :: [Label],           -- All labels in the assembly file
    errorDetect :: Bool,            -- Whether an error was detected in the assembly file
    errorDetail :: String           -- Details of the error detected
}

-- Used functions

twosComDecToBin :: Int -> Int -> String
twosComDecToBin dec bit
    | dec >= 0 = let
        bin1 = decToBin dec
        in padToLength bit '0' bin1
    | otherwise = let
        bin2 = decToBin (2^bit + dec)
        in bin2

decToBin :: Int -> String
decToBin dec = showIntAtBase 2 intToDigit dec ""

padToLength :: Int -> a -> [a] -> [a]
padToLength n c xs
    | length xs >= n = xs
    | otherwise = padToLength n c (c:xs)

checkSameLabel :: [Label] -> Bool
checkSameLabel lst = length lst == length (removeDuplicates lst)
    where removeDuplicates = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

addLabelToList :: [AssemblyLine] -> [Label]
addLabelToList lst = [l | (Just l, _, _, _, _) <- lst]

fillFinding :: AssemblyTranslator -> AssemblyTranslator
fillFinding at = let
    fillLines = [(l, v) | (Just l, ".fill", v, _, _) <- assembly at]
    in at { fillValue = fillLines }

binToDec :: String -> Int
binToDec binary = read ("0b" ++ binary) :: Int

translator :: AssemblyLine -> AssemblyTranslator -> AssemblyTranslator
translator (lbl, inst, ra, rb, rd) at = let
    instructions' = instructions
    (Just idx) = elemIndex inst (name instructions')
    t = type_ instructions' !! idx
    optcBin = optc_bin instructions' !! idx
    regDecoder3Bit = padToLength 3 '0' . decToBin . read
    in case t of
        "R" -> let
            machineCode = "0000000" ++ optcBin ++ regDecoder3Bit ra ++ regDecoder3Bit rb ++ "0000000000000" ++ regDecoder3Bit rd
            in at { machineLang = machineLang at ++ [machineCode] }
        "I" -> case inst of
            "lw" -> let
                offset = read rd :: Int
                machineCode = optcBin ++ regDecoder3Bit rb ++ regDecoder3Bit ra ++ twosComDecToBin offset 16
                in at { machineLang = machineLang at ++ [machineCode] }
            "sw" -> let
                offset = read rd :: Int
                machineCode = optcBin ++ regDecoder3Bit rb ++ regDecoder3Bit ra ++ twosComDecToBin offset 16
                in at { machineLang = machineLang at ++ [machineCode] }
            "beq" -> let
                label = rd
                offset = case elemIndex label (labelList at) of
                    Nothing -> error "label not found"
                    Just idx -> idx - length (machineLang at) - 1
                machineCode = optcBin ++ regDecoder3Bit ra ++ regDecoder3Bit rb ++ twosComDecToBin offset 16
                in at { machineLang = machineLang at ++ [machineCode] }
            _ -> let
                offset = read rd :: Int
                machineCode = optcBin ++ regDecoder3Bit rb ++ regDecoder3Bit ra ++ twosComDecToBin offset 16
                in at { machineLang = machineLang at ++ [machineCode] }
        "J" -> let
            machineCode = optcBin ++ regDecoder3Bit ra ++ "000000000000000"
            in at { machineLang = machineLang at ++ [machineCode] }
        "O" -> let
            machineCode = optcBin ++ "000000000000000000000000"
            in at { machineLang = machineLang at ++ [machineCode] }
        _ -> at { errorDetect = True, errorDetail = "Invalid instruction type" }

assemble :: [String] -> AssemblyTranslator
assemble lines = let
    indexedLines = zip [1..] lines
    labeledLines = map labelLine indexedLines
    translation = foldr translator initial atLines
    at = fillFinding translation
    in if checkSameLabel (labelList at)
        then at
        else at { errorDetect = True, errorDetail = "Duplicate labels found" }
    where
        initial = AssemblyTranslator [] [] [] [] False ""
        atLines = addLabelToList labeledLines `zip` labeledLines
        labelLine (i, line) = case words line of
            [] -> (Nothing, "", "", "", "")
            (w:ws) -> if last w == ':'
                        then (Just $ init w, unwords ws, "", "", "")
                        else (Nothing, w, head ws, ws !! 1, ws !! 2)

