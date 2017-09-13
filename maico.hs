{-# OPTIONS -XRecordWildCards #-}
module Main where

import Control.Exception (catch,IOException)
import Data.Char
import Data.List
import Data.Maybe
import Prelude hiding (print, catch)
import System.Console.Readline
import System.Environment
import System.IO hiding (print)
import System.Random
import Data.Array.IO
import Control.Monad


type Question = String
type Answer   = String
type Help     = String
type DataBase = [(Question,Answer,Help)]
type Punctuation = Int


{- METERLO COMO UN SISTEMA DE BONUSES -}
grammars :: DataBase
grammars =  [("hability in the past","","could can't be used in positive if it's not a general ability"),
             ("past habitual actions", "", "would can only be used in + for actions /non stative verbs"),
             ("past simple vs present perfect", "", "past simple -> time reference , past perfect qué pasa con las referencias"),
             ("deseo del presente o futuro: WISH +","PAST SIMPLE",""),
             ("deseo del pasado: WISH+","PAST PERFECT",""),
             ("compleinng: WISH+","WOULD",""),
             ("Ojala pase el examen: I HOPE I","PASS THE EXAM",""),
             ("Wish pero más intenso","IF ONLY",""),
             ("¿Como es el grammar?: IT'S TIME+","CLAUSE",""),
             ("EXPRESS PREFERENCE PRESENT: OJALÁ FUERA RICO, WOULD RATHER...", "BE RICH", ""),
             ("EXPRESO LO Q HUBIERA PREFERIDO EN EL PASADO. I'D RATHER (do) IT", "DID","")]

patterns :: DataBase
patterns =  [("BE FORBIDDEN","TO DO",""),
             ("BE ALLOWED", "TO DO",""),
             ("BE PERMITTED", "TO DO",""),
             ("MANAGED", "TO DO",""), 
             ("SUCEED", "IN DOING",""),
             ("BE CAPABLE","OF DOING", "NOUN / ING"),
             ("TENDS", "TO DO", ""),
             ("KEEP", "DOING","CONTINUE DOING"),
             ("BE USED TO", "DOING", ""),
             ("GET USED TO", "DOING", ""),
             ("LOOK FORWARD TO", "DOING", ""),
             ("BE JUST ABOUT","TO DO","BE JUST ABOUT to hire her for a second project"),
             ("BE ON THE POINT","OF DOING",""),
             ("BE LIKELY/UNLIKELY","TO DO",""),
             ("BE BOUND","TO DO","algo que pensas que tiene que pasar")]
             
adjectives :: DataBase
adjectives = [("NERVOUS","ABOUT DOING",""),
              ("WORRIED", "ABOUT DOING",""),
              ("BAD", "AT DOING",""),
              ("GOOD", "AT DOING",""),
              ("CLEVER", "AT DOING",""),
              ("SKILLED", "AT DOING",""),
              ("BE SORRY", "FOR DOING",""),
              ("RESPONSIBLE", "FOR DOING",""),
              ("INTERESTED", "IN DOING",""),
              ("CAPABLE", "OF DOING",""),
              ("AFRAID", "OF DOING",""),
              ("FRIGHTENED", "OF DOING",""),
              ("TERRIFIED", "OF DOING",""),
              ("BORED", "WITH DOING","")]
              

  
verbs :: DataBase
verbs = [("WARN","SB ABOUT DOING", ""),
         ("APOLOGISE", "FOR DOING", ""),
         ("ARREST","SB FOR DOING", ""),
         ("SUCCED", "IN DOING", ""),
         ("INSIST", "ON DOING", ""),
         ("OBJECT", "TO DOING", "objection"),
         ("FINISH","DOING", ""),
         ("DON'T MIND", "DOING", ""),
         ("AFFORD", "TO DO", ""),
         ("AGREE", "TO DO", ""),
         ("ARRANGE", "TO DO", ""),
         ("ASK", "TO DO", ""),
         ("APPEAR", "TO DO", ""),
         ("ATTEMPT","TO DO", ""),
         ("CHOOSE", "TO DO", ""),
         ("DECIDE", "TO DO", ""),
         ("EXPECT", "TO DO", ""),
         ("HELP", "TO DO", ""),
         ("INTEND", "TO DO", ""),
         ("LEARN", "TO DO", ""),
         ("MANAGE", "TO DO", ""),
         ("OFFER", "TO DO", ""),
         ("PRETEND", "TO DO", ""),
         ("PROMISE", "TO DO", ""),
         ("REFUSE", "TO DO", ""),
         ("SEEM", "TO DO", ""),
         ("WOULD LOVE/LIVE/PREFER/HATE", "TO DO", ""),
         ("WOULD MIND", "DOING", "")]
         
desambiguar :: DataBase
desambiguar = [ ("TRY intentar hacer algo dificil", "TO DO", ""),
                ("TRY experimentar a ver que pasa", "DOING",""),        
                ("REGRET lo que estoy por hacer", "TO DO",""),        
                ("REGRET algo del pasado ", "DOING",""),        
                ("FORGET me olvide algo del pasado ", "DOING",""),        
                ("REMEMBER me acorde de  algo del pasado ", "DOING",""),        
                ("REMEMBER/FORGET de cerrar la puerta ", "TO DO",""),        
                ("After having luch, we went on walking. ¿Habían estado caminando?", "SI",""),        
                ("After having luch, we went on to walk. ¿Habían estado caminando?", "NO",""),       
                ("MEAN involve","DOING", ""),
                ("MEAN intend", "TO DO", ""),
                ("LIKE general", "DOING", ""),
                ("LIKE antes que la alternativa", "TO DO", ""),
                ("(SENSE VERB) HEAR entero","DO",""),
                ("(SENSE VERB) HEAR una parte", "DOING",""),
                ("Especialmente ejemplo","ESPECIALLY",""),
                ("Especialmente hecho para ti", "SPECIALLY DONE",""),
                ("Raise vs Rise, cual precisa objeto?","RAISE",""),
                ("Look vs Seem: Cuál es solo físico?","LOOK", "")
                ] 
         








-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
    
    {-seleccionar modo: grammar ,etc -}
    {-tener scores guardados de todo -}
main :: IO()
main = do ls <- shuffle (grammars ++ patterns ++ adjectives ++ verbs ++ desambiguar)
          putStrLn "Are you ready?? The exam begins NOW!"
          startExam ls [] 0 1
          
 --            fuente      errores                    num iteracion     
startExam :: DataBase -> DataBase -> Punctuation -> Int -> IO()
startExam ls xs punc n | ls == [] = do putStrLn $ "Exam finished. Punctuation:" ++ show punc ++ " out of " ++ show n ++ "(" ++ show ((1/fromIntegral(n))*100*fromIntegral(punc)) ++ "%)"
                                       putStrLn "Tus errores fueron:"
                                       machaqueCabeza xs

                                  
startExam ls xs punc n = do let (question,answer,help) = head ls 
                            putStrLn $ "Question "++ show n
                            putStrLn question
                            line <- getLine
                            putStrLn (if answer==line then "Cooooorrecto!" else "Incorrecto. Respuesta correcta: " ++ question ++ " " ++ answer)
                            startExam (if answer== line then tail ls else (tail ls)++[head ls]) (if answer== line then xs else (head ls):xs) (if answer== line then punc +1 else punc) (n+1)
          
                        
machaqueCabeza :: DataBase -> IO()
machaqueCabeza ((q,a,_):es) = do putStrLn $ q++" "++a 
                                 machaqueCabeza es
machaqueCabeza []     =  do putStrLn $ "NO SEAS VAGO NI IDIOTA, ESTUDIÁ MUCHO"
                            putStrLn "QUERÉS EMPEZAR DE VUELTA?"
                            answer <- getLine
                            if (answer=="YES") then main else return()


