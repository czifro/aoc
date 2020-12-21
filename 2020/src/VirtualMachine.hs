module VirtualMachine where

t = 0

-- data MachineState a = IDLE
--                     | RUNNING
--                     | EXITED a
--                     deriving (Show, Eq)

-- newtype Memory a = Memory a deriving (Show, Eq)

-- newtype Configuration a = Config a deriving (Show, Eq)

-- newtype Opcode a = Opcode a deriving (Show, Eq)

-- class ROM a where
--   parse :: Show (a, b) => b -> a
--   opcode :: Show (a, o) => a -> Int -> Opcode o

-- class VirtualMachine a where
--   init :: Show (a, b) => b -> a
--   rom :: Show (a, r) => ROM r => a -> r
--   instructionPointer :: Show a => a -> Int
--   moveInstructionPointer :: Show a => a -> Int -> a
--   memory :: Show (a, m) => a -> Memory m
--   state :: Show (a, s) => a -> MachineState s
--   setState :: Show (a, s) => a -> MachineState s -> a
--   config :: Show (a, c) => a -> Configuration c
--   executeOpcode :: Show (a, o) => a -> Opcode o -> a

-- runVM :: VirtualMachine vm => vm -> vm
-- runVM virtMac = case state virtMac of
--   EXITED _ -> virtMac
--   _        ->
--  where
--    currIP = instructionPointer virtMac


-- runInVM :: Show (prog, vm) => VirtualMachine vm => prog -> vm
-- runInVM program = runVM
--  where
--    virtMac = init program
