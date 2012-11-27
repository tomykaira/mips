module Main where

import Text.ParserCombinators.Parsec hiding (State)
import System.Environment (getArgs)
import Data.List (intercalate, foldl')
import Data.Maybe (fromMaybe)
import Control.Applicative ((<*))
import Control.Monad.State

data SlideExp = List [String] | Center String | Paragraph String | WaitCommand
              deriving (Show, Eq)
data Slide = Slide { slideTitle :: String, slideBody :: [SlideExp] }
             deriving (Show)

{- A slide file contains 0 or more slides -}
slideFile :: GenParser Char st [Slide]
slideFile = many slide <* eof

slide :: GenParser Char st Slide
slide =
    do title <- titleLine
       many eol
       body <- many expression
       many eol
       return (Slide { slideTitle = title, slideBody = body })

titleLine :: GenParser Char st String
titleLine = char '#' >> spaces >> normalLine

expression :: GenParser Char st SlideExp
expression = (listExp <|> centerExp <|> paragraph <|> command) <* many eol


listExp :: GenParser Char st SlideExp
listExp =
    fmap List $ many1 listLine
    where
      listLine = char '-' >> spaces >> normalLine

centerExp :: GenParser Char st SlideExp
centerExp =
    do string "|||"
       spaces
       content <- many (noneOf "\n|")
       spaces
       string "|||"
       eol
       return (Center content)

command :: GenParser Char st SlideExp
command = string "*WAIT*" >> return WaitCommand

paragraph :: GenParser Char st SlideExp
paragraph =
    fmap (Paragraph . intercalate " ") $ many1 normalLine

normalLine :: GenParser Char st String
normalLine =
    do
      firstLetter <- (noneOf "#|-*\n")
      rest <- many (noneOf "\n")
      eol
      return (firstLetter : rest)

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseSlideFile :: String -> Either ParseError [Slide]
parseSlideFile input = parse slideFile "(unknown)" input

type LineDefinition = (Int, Int, String)
type LineState a = State Int a

width :: Int
width = 80
height :: Int
height = 30

encodeSlide :: Slide -> [LineDefinition]
encodeSlide Slide {slideTitle = title, slideBody = body} =
    titleLines ++ bodyLines
    where
      titleLines = [(3, 0, title), (0, 1, replicate width '-')]
      bodyLines = concat (evalState (mapM encodeBlock body) 3)

wordWrap :: Int -> String -> [String]
wordWrap maxLength str =
    (map (\line -> intercalate " " line) . reverse) lines
    where
      lines :: [[String]]
      lines = foldr addWord [] (words str)

      addWord :: String -> [[String]] -> [[String]]
      addWord word [] = [[word]]
      addWord word (top : rest) = 
          if tooLong (word : top) then
              ([word] : top : rest)
          else
              ((word : top) : rest)

      tooLong :: [String] -> Bool
      tooLong line = length (intercalate " " line) >= maxLength

inc :: LineState Int
inc =
    do
      i <- get
      put (i + 1)
      return i

encodeBlock :: SlideExp -> LineState [LineDefinition]
encodeBlock exp =
    encodeBody exp <* inc

encodeBody :: SlideExp -> LineState [LineDefinition]
encodeBody (Paragraph content) =
    mapM (\line ->
             do lineNum <- inc
                return (1, lineNum, line)) (wordWrap (width-2) content)

encodeBody (List items) =
    fmap concat $ mapM encodeItem items
    where
      encodeItem item =
          do
            lineNum <- inc
            let firstDefinition = (2, lineNum, "- " ++ first)
            restDefinition <- mapM (\line -> inc >>= (\n -> return (4, n, line))) other
            return (firstDefinition : restDefinition)
          where
            (first : other) = wordWrap (width-5) item

encodeBody (Center content) =
    mapM placeCenter (wordWrap (width-10) content)
    where
      placeCenter str =
          inc >>= (\n -> return (width `div` 2 - length str `div` 2, n, str))

encodeBody (WaitCommand) =
    error "WaitCommand should be removed in processCommands"

printSlide :: [LineDefinition] -> IO()
printSlide definitions =
    do mapM_ (\(x, y, content) -> putStrLn $ show y ++ " " ++ show x ++  " " ++ content) definitions
       putStrLn "-1"

processCommands :: [Slide] -> [Slide]
processCommands slides =
    foldl' insertWaitingSlides [] slides
    where
      insertWaitingSlides slides slide =
          slides ++ ([slide] `fromMaybe` fmap copySlide (findWaitCommand slide))

      findWaitCommand (Slide {slideTitle = title, slideBody = body}) =
          let (before, after) = span ((/=) WaitCommand) body in
          if null after then Nothing else Just(title, before, tail after)

      copySlide (title, before, after) =
          insertWaitingSlides [(Slide {slideTitle = title, slideBody = before})] (Slide {slideTitle = title, slideBody = before ++ after})

main :: IO ()
main = do
  args <- getArgs
  let file = (head args)
  content <- readFile file
  case parseSlideFile content of
    Left(e)   -> putStrLn $ "Error: " ++ (show e)
    Right slides ->
        do
          (putStrLn . show . length) slides
          mapM_ (printSlide . encodeSlide) (processCommands slides)
