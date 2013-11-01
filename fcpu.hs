module Main where
import Data.Int
import Data.Bits
import Data.Text hiding (map, unlines, lines)
import Control.Monad.State

import Test.Hspec

type Word = Int16
data GPReg = RegA
           | RegB
           | RegC
           deriving (Eq, Show, Read)

data SReg = RegZ
          | RegIP
          deriving (Eq, Show, Read)

data Instr = Ld GPReg Word
           | Ldm GPReg Word
           | Stm Word GPReg
           | Add GPReg GPReg GPReg
           | Sub GPReg GPReg GPReg
           | Inc GPReg
           | Dec GPReg
           | Or GPReg Word
           | Xor GPReg Word
           | And GPReg Word
           | Cmp GPReg Word
           | Jne Word
           | Prt  GPReg
           | Stp
           deriving (Show, Eq)

mapTuple :: (a->b) -> (a,a) -> (b,b)
mapTuple f (a1,a2) = (f a1, f a2)

str2GPReg :: String ->  GPReg
str2GPReg "a" = RegA
str2GPReg "A" = RegA
str2GPReg "b" = RegB
str2GPReg "B" = RegB
str2GPReg "c" = RegC
str2GPReg "C" = RegC


str2Int16 :: String -> Int16
str2Int16 s = read s :: Int16

str2Instr :: String -> Instr
str2Instr str = case opcode of
                     "load"   ->  Ld   (str2GPReg op1) (str2Int16 op2)
                     "loadm"  ->  Ldm  (str2GPReg op1) (str2Int16 op2)
                     "setm"   ->  Stm  (str2Int16 op1) (str2GPReg op2)
                     "add"    ->  Add  (str2GPReg op1) (str2GPReg op2) (str2GPReg op3)
                     "sub"    ->  Sub  (str2GPReg op1) (str2GPReg op2) (str2GPReg op3)
                     "inc"    ->  Inc  (str2GPReg op1)
                     "dec"    ->  Dec  (str2GPReg op1)
                     "or"     ->  Or   (str2GPReg op1) (str2Int16 op2)
                     "xor"    ->  Xor  (str2GPReg op1) (str2Int16 op2)
                     "and"    ->  And  (str2GPReg op1) (str2Int16 op2)
                     "cmp"    ->  Cmp  (str2GPReg op1) (str2Int16 op2)
                     "jne"    ->  Jne  (str2Int16 op1)
                     "print"  ->  Prt  (str2GPReg op1)
                     "stop"   ->  Stp
        where text = unpack $ strip $ pack str
              (opcode, operands) = mapTuple unpack $ breakOn (pack " ") (pack text)
              ops = map (unpack . strip) $ splitOn (pack ",") (pack operands)
              op1 = ops !! 0
              op2 = ops !! 1
              op3 = ops !! 2

testCodeString :: [String]
testCodeString =  [
                "load a, 1",
                "load b, 2",
                "load c, 3",
                "inc a",
                "inc b",
                "inc c",
                "sub a, c, b",
                "dec a",
                "stop"
               ]

interpInstr :: [Instr] -> Cpu -> Cpu
interpInstr instr initState = snd $ runState (mapM_ execInstr instr) initState


testCode :: [Instr]
testCode = map str2Instr testCodeString
testRun ::  ((), Cpu)
testRun = runState (runCode testCode) initialState


data Cpu = Cpu {
    _regA  :: Int16,
    _regB  :: Int16,
    _regC  :: Int16,
    _regZ  :: Int16,
    _regIP :: Int16,
    _run   :: Bool
    } deriving (Show, Eq)

initialState :: Cpu
initialState = Cpu {
    _regA = 0,
    _regB = 0,
    _regC = 0,
    _regZ = 0,
    _regIP = 0,
    _run = True
    }

type CpuState = State Cpu
runCode :: [Instr] -> CpuState ()
runCode is = do
    mapM_ execInstr is


execInstr :: Instr -> CpuState ()
{- LD reg <val> -}
execInstr (Ld RegA val) = do
    st <- get
    put st { _regA = val, _regIP = _regIP st + 1 }
execInstr (Ld RegB val) = do
    st <- get
    put st { _regB = val, _regIP = _regIP st + 1 }
execInstr (Ld RegC val) = do
    st <- get
    put st { _regC = val, _regIP = _regIP st + 1 }

{- LDM reg, address -}
{- STM reg, address -}

{- ADD reg1, reg2, reg3 -}
execInstr (Add RegA _ _) = do
    st <- get
    put st { _regA = _regB st + _regC st, _regIP = _regIP st + 1 }
execInstr (Add RegB _ _) = do
    st <- get
    put st { _regB = _regA st + _regC st, _regIP = _regIP st + 1 }
execInstr (Add RegC _ _) = do
    st <- get
    put st { _regC = _regA st + _regB st, _regIP = _regIP st + 1  }

{- SUB reg1, reg2, reg3 -}
execInstr (Sub RegA RegB RegC) = do
    st <- get
    put st { _regA = _regB st - _regC st, _regIP = _regIP st + 1 }
execInstr (Sub RegA RegC RegB) = do
    st <- get
    put st { _regA = _regC st - _regB st, _regIP = _regIP st + 1 }
execInstr (Sub RegB RegA RegC) = do
    st <- get
    put st { _regB = _regA st - _regC st, _regIP = _regIP st + 1 }
execInstr (Sub RegB RegC RegA) = do
    st <- get
    put st { _regB = _regC st - _regA st, _regIP = _regIP st + 1 }
execInstr (Sub RegC RegA RegB) = do
    st <- get
    put st { _regC = _regA st - _regB st, _regIP = _regIP st + 1 }
execInstr (Sub RegC RegB RegA) = do
    st <- get
    put st { _regC = _regB st - _regA st, _regIP = _regIP st + 1 }

{- INC reg -}
execInstr (Inc RegA) = do
    st <- get
    put st { _regA = _regA st + 1, _regIP = _regIP st + 1 }
execInstr (Inc RegB) = do
    st <- get
    put st { _regB = _regB st + 1, _regIP = _regIP st + 1 }
execInstr (Inc RegC) = do
    st <- get
    put st { _regC = _regC st + 1, _regIP = _regIP st + 1 }

{- DEC reg -}
execInstr (Dec RegA) = do
    st <- get
    put st { _regA = _regA st - 1, _regIP = _regIP st + 1 }
execInstr (Dec RegB) = do
    st <- get
    put st { _regB = _regB st - 1, _regIP = _regIP st + 1 }
execInstr (Dec RegC) = do
    st <- get
    put st { _regC = _regC st - 1, _regIP = _regIP st + 1 }

{- OR reg -}
execInstr (Or RegA val) = do
    st <- get
    put st { _regA =  _regA st .|. val, _regIP = _regIP st + 1 }
execInstr (Or RegB val) = do
    st <- get
    put st { _regB =  _regB st .|. val, _regIP = _regIP st + 1 }
execInstr (Or RegC val) = do
    st <- get
    put st { _regC =  _regC st .|. val, _regIP = _regIP st + 1 }

{- XOR reg -}
execInstr (Xor RegA val) = do
    st <- get
    put st { _regA =  _regA st `xor` val, _regIP = _regIP st + 1 }
execInstr (Xor RegB val) = do
    st <- get
    put st { _regB =  _regB st `xor` val, _regIP = _regIP st + 1 }
execInstr (Xor RegC val) = do
    st <- get
    put st { _regC =  _regC st `xor` val, _regIP = _regIP st + 1 }

{- AND reg -}
execInstr (And RegA val) = do
    st <- get
    put st { _regA =  _regA st .&. val, _regIP = _regIP st + 1 }
execInstr (And RegB val) = do
    st <- get
    put st { _regB =  _regB st .&. val, _regIP = _regIP st + 1 }
execInstr (And RegC val) = do
    st <- get
    put st { _regC =  _regC st .&. val, _regIP = _regIP st + 1 }

{- CMP reg -}
execInstr (Cmp RegA val) = do
    st <- get
    put st { _regZ = (cmpReg (_regA st) val)  , _regIP = _regIP st + 1 }
execInstr (Cmp RegB val) = do
    st <- get
    put st { _regZ = (cmpReg (_regB st) val)  , _regIP = _regIP st + 1 }
execInstr (Cmp RegC val) = do
    st <- get
    put st { _regZ = (cmpReg (_regC st) val)  , _regIP = _regIP st + 1 }

{- Jne ofs -}
execInstr (Jne val) = do
    st <- get
    put st { _regIP = if ((_regZ st) == 0) then _regIP st + val else _regIP st + 1 }

{- Stp -}
execInstr (Stp) = do
    st <- get
    put st { _run = False }


cmpReg :: Word -> Word -> Word
cmpReg r v = if (r == v) then 1 else 0



main ::  IO ()
main = do
    print "whateverg"


testIt :: IO ()
testIt = hspec $ do

    describe "str2GPReg" $ do
        it "converts a to register A" $
            str2GPReg "a" `shouldBe` RegA
        it "converts b to register B" $
            str2GPReg "b" `shouldBe` RegB
        it "converts c to register C" $
            str2GPReg "c" `shouldBe` RegC

    describe "str2Instr" $ do
        it "parses (load a,1) correctly" $
            str2Instr "load a,1" `shouldBe` (Ld RegA 1)
        it "parses (load c,1) correctly" $
            str2Instr "load b,1" `shouldBe` (Ld RegB 1)
        it "parses (load b,1) correctly" $
            str2Instr "load c,1" `shouldBe` (Ld RegC 1)

        it "parses (loadm a,1) correctly" $
            str2Instr "loadm a,1" `shouldBe` (Ldm RegA 1)
        it "parses (loadm c,1) correctly" $
            str2Instr "loadm b,1" `shouldBe` (Ldm RegB 1)
        it "parses (loadm b,1) correctly" $
            str2Instr "loadm c,1" `shouldBe` (Ldm RegC 1)

        it "parses (setm a,1) correctly" $
            str2Instr "setm 1,a" `shouldBe` (Stm 1 RegA)
        it "parses (setm c,1) correctly" $
            str2Instr "setm 1,b" `shouldBe` (Stm 1 RegB)
        it "parses (setm b,1) correctly" $
            str2Instr "setm 1,c" `shouldBe` (Stm 1 RegC)

        it "parses (add a, b, c) correctly" $
            str2Instr "add a, b, c" `shouldBe` (Add RegA RegB RegC)
        it "parses (add b, a, c) correctly" $
            str2Instr "add b, a, c" `shouldBe` (Add RegB RegA RegC)
        it "parses (add c, a, b) correctly" $
            str2Instr "add c, a, b" `shouldBe` (Add RegC RegA RegB)

        it "parses (inc a) correctly" $
            str2Instr "inc a" `shouldBe` (Inc RegA)
        it "parses (inc b) correctly" $
            str2Instr "inc b" `shouldBe` (Inc RegB)
        it "parses (inc c) correctly" $
            str2Instr "inc c" `shouldBe` (Inc RegC)

        it "parses (dec a) correctly" $
            str2Instr "dec a" `shouldBe` (Dec RegA)
        it "parses (dec b) correctly" $
            str2Instr "dec b" `shouldBe` (Dec RegB)
        it "parses (dec c) correctly" $
            str2Instr "dec c" `shouldBe` (Dec RegC)

        it "parses (xor a, 42) correctly" $
            str2Instr "xor a, 42" `shouldBe` (Xor RegA 42)
        it "parses (xor b, 42) correctly" $
            str2Instr "xor b, 42" `shouldBe` (Xor RegB 42)
        it "parses (xor c, 42) correctly" $
            str2Instr "xor c, 42" `shouldBe` (Xor RegC 42)

        it "parses (and a, 42) correctly" $
            str2Instr "and a, 42" `shouldBe` (And RegA 42)
        it "parses (and b, 42) correctly" $
            str2Instr "and b, 42" `shouldBe` (And RegB 42)
        it "parses (and c, 42) correctly" $
            str2Instr "and c, 42" `shouldBe` (And RegC 42)

        it "parses (cmp a, 13) correctly" $
            str2Instr "cmp a, 13" `shouldBe` (Cmp RegA 13)
        it "parses (cmp b, 13) correctly" $
            str2Instr "cmp b, 13" `shouldBe` (Cmp RegB 13)
        it "parses (cmp c, 13) correctly" $
            str2Instr "cmp c, 13" `shouldBe` (Cmp RegC 13)

        it "parses (jne 15) correctly" $
            str2Instr "jne 15" `shouldBe` (Jne 15)
        it "parses (jne -15) correctly" $
            str2Instr "jne -15" `shouldBe` (Jne (-15))

        it "skips whitespace in (jne    -15)" $
            str2Instr "jne    -15   "  `shouldBe` (Jne (-15))
        it "skips whitespace in (add  b,  a,   c)" $
            str2Instr "add   b,   a,     c" `shouldBe` (Add RegB RegA RegC)


    describe "execInstr" $ do
        context "load ..." $ do
            it "executes (load a,1) correctly" $
                (interpInstr [(Ld RegA 1)] initialState) `shouldBe` initialState { _regA = 1, _regIP = 1 }
            it "executes (load b,1) correctly" $
                (interpInstr [(Ld RegB 1)] initialState) `shouldBe` initialState { _regB = 1, _regIP = 1 }
            it "executes (load c,1) correctly" $
                (interpInstr [(Ld RegC 1)] initialState) `shouldBe` initialState { _regC = 1, _regIP = 1 }
        context "inc ..." $ do
            it "executes (load a,1 ; inc a) correctly" $
                (interpInstr [(Ld RegA 1), (Inc RegA)] initialState) `shouldBe` initialState { _regA = 2, _regIP = 2 }
        context "cmp&jmp ..." $ do
            it "executes (cmp a,1 ; jne 42) correctly" $
                (interpInstr [(Cmp RegA 1), (Jne 42)] initialState) `shouldBe` initialState { _regIP = 43 }
