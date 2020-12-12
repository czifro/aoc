module AOC2020.Day8 ( runBootCode
                    , fixAndRunBootCode
                    , solve
                    ) where

import Data.List ( union
                 , splitAt
                 , (\\)
                 , null
                 )
import Debug.Trace

{-

--- Day 8: Handheld Halting ---

Your flight to the major airline hub reaches cruising altitude without incident. While you consider checking the in-flight menu for one of those drinks that come with a little umbrella, you are interrupted by the kid sitting next to you.

Their handheld game console won't turn on! They ask if you can take a look.

You narrow the problem down to a strange infinite loop in the boot code (your puzzle input) of the device. You should be able to fix it, but first you need to be able to run the code in isolation.

The boot code is represented as a text file with one instruction per line of text. Each instruction consists of an operation (acc, jmp, or nop) and an argument (a signed number like +4 or -20).

    acc increases or decreases a single global value called the accumulator by the value given in the argument. For example, acc +7 would increase the accumulator by 7. The accumulator starts at 0. After an acc instruction, the instruction immediately below it is executed next.
    jmp jumps to a new instruction relative to itself. The next instruction to execute is found using the argument as an offset from the jmp instruction; for example, jmp +2 would skip the next instruction, jmp +1 would continue to the instruction immediately below it, and jmp -20 would cause the instruction 20 lines above to be executed next.
    nop stands for No OPeration - it does nothing. The instruction immediately below it is executed next.

For example, consider the following program:

nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6

These instructions are visited in this order:

nop +0  | 1
acc +1  | 2, 8(!)
jmp +4  | 3
acc +3  | 6
jmp -3  | 7
acc -99 |
acc +1  | 4
jmp -4  | 5
acc +6  |

First, the nop +0 does nothing. Then, the accumulator is increased from 0 to 1 (acc +1) and jmp +4 sets the next instruction to the other acc +1 near the bottom. After it increases the accumulator from 1 to 2, jmp -4 executes, setting the next instruction to the only acc +3. It sets the accumulator to 5, and jmp -3 causes the program to continue back at the first acc +1.

This is an infinite loop: with this sequence of jumps, the program will run forever. The moment the program tries to run any instruction a second time, you know it will never terminate.

Immediately before the program would run an instruction a second time, the value in the accumulator is 5.

Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?

--- Part Two ---

After some careful analysis, you believe that exactly one instruction is corrupted.

Somewhere in the program, either a jmp is supposed to be a nop, or a nop is supposed to be a jmp. (No acc instructions were harmed in the corruption of this boot code.)

The program is supposed to terminate by attempting to execute an instruction immediately after the last instruction in the file. By changing exactly one jmp or nop, you can repair the boot code and make it terminate correctly.

For example, consider the same program from above:

nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6

If you change the first instruction from nop +0 to jmp +0, it would create a single-instruction infinite loop, never leaving that instruction. If you change almost any of the jmp instructions, the program will still eventually find another jmp instruction and loop forever.

However, if you change the second-to-last instruction (from jmp -4 to nop -4), the program terminates! The instructions are visited in this order:

nop +0  | 1
acc +1  | 2
jmp +4  | 3
acc +3  |
jmp -3  |
acc -99 |
acc +1  | 4
nop -4  | 5
acc +6  | 6

After the last instruction (acc +6), the program terminates by attempting to run the instruction below the last instruction in the file. With this change, after the program terminates, the accumulator contains the value 8 (acc +1, acc +1, acc +6).

Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp). What is the value of the accumulator after the program terminates?

-}

type InstructionPointer = Int
type Register = Int

data State = IDLE
           | RUNNING
           | FAULTED
           | TERMINATED
           deriving (Show, Eq)

data Instruction = NOP Int
                 | JMP Int
                 | ACC Int
                 | TERM
                 deriving (Show, Eq)

data LoopMode = NO_LOOPS
              | ALLOW_LOOPS
              deriving (Show, Eq)

data HandheldGameConsole = HandheldGameConsole
                           { state              :: State
                           , register           :: Register
                           , instructionPointer :: InstructionPointer
                           , instructionSet     :: [(Int, Instruction)]
                           , stackTrace         :: [(Int, Instruction)]
                           , loopMode           :: LoopMode
                           } deriving (Show, Eq)

makeHandheldGameConsole :: LoopMode -> [(Int, Instruction)] -> HandheldGameConsole
makeHandheldGameConsole loop instructions = HandheldGameConsole {state = IDLE, register = 0, instructionPointer = 0, instructionSet = instructions, stackTrace = [], loopMode = loop}

readInt :: String -> Int
readInt = read

parseInstructions :: [String] -> [Instruction]
parseInstructions = foldl step []
 where
   step :: [Instruction] -> String -> [Instruction]
   step is x = is ++ [parseInstruction . words $ x]
   parseInstruction :: [String] -> Instruction
   parseInstruction ["nop", n] = NOP $ readInt . filter (/='+') $ n
   parseInstruction ["jmp", j] = JMP $ readInt . filter (/='+') $ j
   parseInstruction ["acc", a] = ACC $ readInt . filter (/='+') $ a
   parseInstruction i = error ("Invalid instruction: " ++ unwords i)

runConsole :: HandheldGameConsole -> HandheldGameConsole
runConsole console | state console == FAULTED = console
                   | state console == TERMINATED || instructionPointer console >= (length . instructionSet $ console) = console { state = TERMINATED }
                   | state console == IDLE = runConsole $ console { state = RUNNING }
                   | state console == RUNNING = runConsole nextConsole
                   | otherwise = error "Unexpected state"
 where
   (index, currInstr) = (!! max 0 (instructionPointer console)) $ instructionSet console
   hasLooped = (index, currInstr) `elem` stackTrace console
   nextState = if hasLooped && loopMode console == NO_LOOPS then FAULTED else if currInstr == TERM then TERMINATED else RUNNING
   nextInstructionPointer = case currInstr of
     JMP j -> instructionPointer console + j
     _     -> instructionPointer console + 1
   nextRegister = case currInstr of
     ACC a -> register console + a
     _     -> register console
   nextStackTrace = (index, currInstr) : stackTrace console
   nextConsole = case nextState of
     FAULTED -> console { state = FAULTED }
     TERMINATED -> console { state = TERMINATED }
     _       -> console { instructionPointer = nextInstructionPointer, register = nextRegister, stackTrace = nextStackTrace }

runBootCode :: [String] -> Int
runBootCode = register . runConsole . makeHandheldGameConsole NO_LOOPS . zipInstructionIndices . parseInstructions
 where
   zipInstructionIndices :: [Instruction] -> [(Int, Instruction)]
   zipInstructionIndices is = zip [0..(length is - 1)] is

fixBootCode :: [(Int, Instruction)] -> [(Int, Instruction)]
fixBootCode instructions = fixInstruction instructions (head viableFixes)
 where
   instructionsWithTerm = zip [0..] ((map snd instructions) ++ [TERM])
   graph = (instructionsWithTerm, foldl addEdge [] instructionsWithTerm)
    where
      addEdge edges (index, instr) = edges ++ edge
       where edge = case instr of
               JMP j -> if index + j >= length instructionsWithTerm then [] else  [((index, instr), (!! max 0 (index + j)) instructionsWithTerm)]
               _     -> if index + 1 >= length instructionsWithTerm then [] else [((index, instr), (!! max 0 (index + 1)) instructionsWithTerm)]

   getPathsTo :: ([(Int, Instruction)], [((Int, Instruction), (Int, Instruction))]) -> [(Int, Instruction)] -> (Int, Instruction) -> [(Int, Instruction)]
   getPathsTo (v, e) visited node = newVisited `union` indirectConnectedNodes
    where
      connectedNewNodes = (map fst . filter ((==node) . snd)) e \\ visited
      newVisited = visited `union` connectedNewNodes
      indirectConnectedNodes = concatMap (getPathsTo (v, e) newVisited) connectedNewNodes

   doesFixingTerminate :: [(Int, Instruction)] -> (Int, Instruction) -> Bool
   doesFixingTerminate termInstr (index, instr) = not (any (\i -> fst i == index) termInstr) && case instr of
     JMP _ -> any (\i -> fst i == (index + 1)) termInstr
     NOP n -> any (\i -> fst i == (index + n)) termInstr
     _     -> False

   fixInstruction :: [(Int, Instruction)] -> (Int, Instruction) -> [(Int, Instruction)]
   fixInstruction instr (index, fix) = case fix of
     JMP v -> instr0 ++ ((index, NOP v):instr1)
     NOP v -> instr0 ++ ((index, JMP v):instr1)
     _     -> instr
    where (instr0, _:instr1) = splitAt index instr

   terminatingInstructions = getPathsTo graph [] (last instructionsWithTerm)
   possibleFixes = filter (doesFixingTerminate terminatingInstructions) instructions
   viableFixes = filter (((head instructions) `elem`) . (getPathsTo graph [])) possibleFixes

fixAndRunBootCode :: [String] -> Int
fixAndRunBootCode rawInstructions = register . runConsole . makeHandheldGameConsole NO_LOOPS $ fixedInstructions
 where
   zipInstructionIndices :: [Instruction] -> [(Int, Instruction)]
   zipInstructionIndices is = zip [0..(length is - 1)] is

   instructions = zipInstructionIndices . parseInstructions $ rawInstructions
   fixedInstructions = fixBootCode instructions

solvePart1 :: IO ()
solvePart1 = print . runBootCode . lines =<< readFile "inputs/day8.txt"

solvePart2 :: IO ()
solvePart2 = print . fixAndRunBootCode . lines =<< readFile "inputs/day8.txt"

solve :: IO ()
solve = do
  putStr "Part 1: "
  solvePart1
  putStr "Part 2: "
  solvePart2
