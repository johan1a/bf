module Main where

import Debug.Trace
import Data.Char

type Command = String
type Source = [Command]
type Buffer = [Int]
type ProgramState = (Source, Int, Buffer, Int)

bufferSize = 100

helloWorld = run "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

run :: String -> IO ()
run s = do 
    execute (programState s)
    putStrLn ""
    return ()

debug :: String -> IO ProgramState
debug s = execute $ programState s

programState :: String -> ProgramState
programState string = (toChunks string, 0, makeBuffer, 0)

toChunks :: String -> [String]
toChunks = map (: [])

charToString :: Char -> String
charToString ss = [ss]

makeBuffer :: [Int]
makeBuffer = replicate bufferSize 0

execute :: ProgramState -> IO ProgramState
execute ps
    | terminated ps = return ps
    | otherwise = do
        nextState <- executeCommand ps $ getCurrentCmd ps
        execute  nextState

terminated (source, currentCmd, buffer, pointer) = currentCmd == length source

getCurrentCmd (source, currentCmd, buffer, pointer) = source !! currentCmd

executeCommand :: ProgramState -> Command -> IO ProgramState
executeCommand ps ">" = return $ incPointer ps
executeCommand ps "<" = return $ decPointer ps
executeCommand ps "+" = return $ incBuffer ps
executeCommand ps "-" = return $ decBuffer ps
executeCommand ps "." = do
    printChar ps
    return $ step ps
executeCommand ps "," = do
    s <- readLn :: IO Int
    return $ step $ store s ps
executeCommand ps "[" 
    | zeroBuffer ps = return $ condJumpForward ps
executeCommand ps "]" 
    | nonZeroBuffer ps = return $ condJumpBack ps
executeCommand ps _ = return $ step ps

trimnl = reverse . dropWhile (=='\n') . reverse

store c (source, currentCmd, buffer, pointer) = (source, currentCmd, update pointer c buffer, pointer)

-- Step forward to the next command
step :: ProgramState -> ProgramState
step = inc 1

incPointer :: ProgramState -> ProgramState
incPointer ps = step $ inc 3 ps

decPointer :: ProgramState -> ProgramState
decPointer ps = step $ dec 3 ps

incBuffer (source, currentCmd, buffer, pointer) = step (source, currentCmd, increment buffer pointer, pointer )

decBuffer (source, currentCmd, buffer, pointer) = step (source, currentCmd, decrement buffer pointer, pointer )

printChar :: (t, t1, [Int], Int) -> IO ()
printChar (_, _, buffer, pointer) = putChar $ chr (buffer !! pointer)

zeroBuffer (_, _, buffer, pointer) = (buffer !! pointer) == 0

nonZeroBuffer (_, _, buffer, pointer) = (buffer !! pointer) /= 0

condJumpBack :: ProgramState -> ProgramState
condJumpBack (source, currentCmd, buffer, pointer) = condJump_ (source, currentCmd - 1, buffer, pointer) (-1) 1

condJumpForward :: ProgramState -> ProgramState
condJumpForward (source, currentCmd, buffer, pointer) = condJump_ (source, currentCmd + 1, buffer, pointer) 1 1

condJump_ :: ProgramState -> Int -> Int -> ProgramState
condJump_ ps modifier 0 = ps
condJump_ (source, currentCmd, buffer, pointer) modifier bracketCount 
    | (source !! currentCmd) == "[" && bracketCount == 1 && modifier == -1 = (source, currentCmd + 1, buffer, pointer) 
    | (source !! currentCmd) == "]" && bracketCount == 1 && modifier == 1 = (source, currentCmd + 1, buffer, pointer) 
    | (source !! currentCmd) == "[" = condJump_ (source, currentCmd + modifier, buffer, pointer) modifier (bracketCount + modifier)
    | (source !! currentCmd) == "]" = condJump_ (source, currentCmd + modifier, buffer, pointer) modifier (bracketCount - modifier)
    | otherwise = condJump_ (source, currentCmd + modifier, buffer, pointer) modifier bracketCount

increment :: Buffer -> Int -> Buffer
increment buffer pointer = update pointer ((buffer !! pointer) + 1) buffer

decrement :: Buffer -> Int -> Buffer
decrement buffer pointer = update pointer ((buffer !! pointer) - 1) buffer

update :: Num a => Int -> a -> [a] -> [a]
update i x ss = take i ss ++ [x] ++ drop (i + 1) ss

inc :: Num a => Int -> ProgramState -> ProgramState
inc 1 (a, b, c, d) = (a, b + 1, c, d)
inc 3 (a, b, c, d) = (a, b, c, d + 1)

dec :: Num a => Int -> ProgramState -> ProgramState
dec 1 (a, b, c, d) = (a, b - 1, c, d)
dec 3 (a, b, c, d) = (a, b, c, d - 1)

wtf = run "+++[>+++++<-]>>+<[>>++++>++>+++++>+++++>+>>+<++[++<]>---]>++++.>>>.+++++.>------.<--.+++++++++.>+.+.<<<<---.[>]<<.<<<.-------.>++++.<+++++.+.>-----.>+.<++++.>>++.>-----.<<<-----.+++++.-------.<--.<<<.>>>.<<+.>------.-..--.+++.-----<++.<--[>+<-]>>>>>--.--.<++++.>>-.<<<.>>>--.>.<<<<-----.>----.++++++++.----<+.+++++++++>>--.+.++<<<<.[>]<.>>,[>>+++[<+++++++>-]<[<[-[-<]]>>[>]<-]<[<+++++>-[<+++>-[<-->-[<+++>-[<++++[>[->>]<[>>]<<-]>[<+++>-[<--->-[<++++>-[<+++[>[-[-[-[->>]]]]<[>>]<<-]>[<+>-[<->-[<++>-[<[-]>-]]]]]]]]]]]]]<[    -[-[>+<-]>]    <[<<<<.>+++.+.+++.-------.>---.++.<.>-.++<<<<.[>]>>>>>>>>>]    <[[<]>++.--[>]>>>>>>>>]    <[<<++..-->>>>>>]    <[<<..>>>>>]    <[<<..-.+>>>>]    <[<<++..---.+>>>]    <[<<<.>>.>>>>>]    <[<<<<-----.+++++>.----.+++.+>---.<<<-.[>]>]    <[<<<<.-----.>++++.<++.+++>----.>---.<<<.-[>]]    <[<<<<<----.>>.<<.+++++.>>>+.++>.>>]    <.>]>,]<<<<<.<+.>++++.<----.>>---.<<<-.>>>+.>.>.[<]>++.[>]<.>[Translates brainfuck to C. Assumes no-change-on-EOF or EOF->0.Generated C does no-change-on-EOF, and uses unistd.h read and write calls.Daniel B Cristofani (cristofdathevanetdotcom)http://www.hevanet.com/cristofd/brainfuck/]"