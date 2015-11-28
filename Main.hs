module Main where

import Debug.Trace
import Data.Char


type Command = String
type Source = [Command]
type Buffer = [Int]
type ProgramState = (Source, Int, Buffer, Int)

--main = do
--  print "Inside main"
--  --program <- (execute (programState ">>+++"))
--  --return ()

bufferSize = 100000

helloWorld = run "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
firstHelloWorld = debug "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]"

main = print "blabla"

run :: String -> IO ()
run s = do 
    execute (programState s)
    putStrLn ""
    return ()

debug :: String -> IO ProgramState
debug s = execute $ programState s


programState :: String -> ProgramState
programState string = (toSource string, 0, makeBuffer, 0)

toSource :: String -> Source
toSource ss = toChunks ss 

toChunks :: String -> [String]
toChunks [] = []
toChunks (s:ss) = [[s]] ++ toChunks ss

charToString :: Char -> String
charToString ss = [ss]

makeBuffer :: [Int]
makeBuffer = take bufferSize $ repeat 0

execute :: ProgramState -> IO ProgramState
execute (source, currentCmd, buffer, pointer) 
    | currentCmd == (length source) = return (source, currentCmd, buffer, pointer)
    | otherwise = do
        nextState <- (executeCommand (source, currentCmd, buffer, pointer) (source !! currentCmd) )
        execute  nextState

executeCommand :: ProgramState -> Command -> IO ProgramState
executeCommand ps ">" = return $ incPointer ps
executeCommand ps "<" = return $ decPointer ps
executeCommand ps "+" = return $ incBuffer ps
executeCommand ps "-" = return $ decBuffer ps
executeCommand (source, currentCmd, buffer, pointer) "." = do
    putChar $ chr (buffer !! pointer)
    return (source, currentCmd + 1, buffer, pointer)
executeCommand (source, currentCmd, buffer, pointer) "[" 
    | (buffer !! pointer) == 0 = return $ condJumpForward (source, currentCmd, buffer, pointer)
    | otherwise = return $ (source, currentCmd + 1, buffer, pointer)

executeCommand (source, currentCmd, buffer, pointer) "]" 
    | (buffer !! pointer) /= 0 = return $ condJumpBack (source, currentCmd, buffer, pointer)
    | otherwise = return $ (source, currentCmd + 1, buffer, pointer)

executeCommand ps _ = return $ step ps

-- Step forward to the next command
step :: ProgramState -> ProgramState
step (source, currentCmd, buffer, pointer) = (source, currentCmd + 1, buffer, pointer)

incPointer (source, currentCmd, buffer, pointer) = (source, currentCmd + 1, buffer, pointer + 1)

decPointer (source, currentCmd, buffer, pointer) = (source, currentCmd + 1, buffer, pointer - 1)

incBuffer (source, currentCmd, buffer, pointer) = (source, currentCmd + 1, (increment buffer pointer), pointer )

decBuffer (source, currentCmd, buffer, pointer) = (source, currentCmd + 1, (decrement buffer pointer), pointer )

condJumpBack :: ProgramState -> ProgramState
condJumpBack (source, currentCmd, buffer, pointer) = condJump_ (source, (currentCmd - 1), buffer, pointer) (-1) 1

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
update i x ss = (take i ss) ++ [x] ++ (drop (i + 1) ss)


wtf = run "+++[>+++++<-]>>+<[>>++++>++>+++++>+++++>+>>+<++[++<]>---]>++++.>>>.+++++.>------.<--.+++++++++.>+.+.<<<<---.[>]<<.<<<.-------.>++++.<+++++.+.>-----.>+.<++++.>>++.>-----.<<<-----.+++++.-------.<--.<<<.>>>.<<+.>------.-..--.+++.-----<++.<--[>+<-]>>>>>--.--.<++++.>>-.<<<.>>>--.>.<<<<-----.>----.++++++++.----<+.+++++++++>>--.+.++<<<<.[>]<.>>,[>>+++[<+++++++>-]<[<[-[-<]]>>[>]<-]<[<+++++>-[<+++>-[<-->-[<+++>-[<++++[>[->>]<[>>]<<-]>[<+++>-[<--->-[<++++>-[<+++[>[-[-[-[->>]]]]<[>>]<<-]>[<+>-[<->-[<++>-[<[-]>-]]]]]]]]]]]]]<[    -[-[>+<-]>]    <[<<<<.>+++.+.+++.-------.>---.++.<.>-.++<<<<.[>]>>>>>>>>>]    <[[<]>++.--[>]>>>>>>>>]    <[<<++..-->>>>>>]    <[<<..>>>>>]    <[<<..-.+>>>>]    <[<<++..---.+>>>]    <[<<<.>>.>>>>>]    <[<<<<-----.+++++>.----.+++.+>---.<<<-.[>]>]    <[<<<<.-----.>++++.<++.+++>----.>---.<<<.-[>]]    <[<<<<<----.>>.<<.+++++.>>>+.++>.>>]    <.>]>,]<<<<<.<+.>++++.<----.>>---.<<<-.>>>+.>.>.[<]>++.[>]<.>[Translates brainfuck to C. Assumes no-change-on-EOF or EOF->0.Generated C does no-change-on-EOF, and uses unistd.h read and write calls.Daniel B Cristofani (cristofdathevanetdotcom)http://www.hevanet.com/cristofd/brainfuck/]"