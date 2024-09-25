module GameMove (doMoveCommand) where
import GameModel
import GameUtils
import GameIO
import GameDisplay
import IdrisGen
import System.Process
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import Control.Applicative
import Text.Printf
import Data.Char (digitToInt)

data IdrisSExp = IdrisNil | IdrisNum Int | IdrisStr String | IdrisKey String
                 | IdrisBracketed [IdrisSExp] deriving (Eq, Ord, Show)

parseEscapedChar :: P.Parsec String u Char
parseEscapedChar = (P.char '\\' *> P.anyChar) <|> P.anyChar
parseIdrisStr :: P.Parsec String u String
parseIdrisStr = (P.char '"') *> P.manyTill parseEscapedChar (P.try $ P.char '"')

parseIdrisSExp :: P.Parsec String u IdrisSExp
parseIdrisSExp =
      (P.string "nil" *> pure IdrisNil)
  <|> ((IdrisNum . fromIntegral) <$> P.integer P.haskell)
  <|> (IdrisStr <$> parseIdrisStr)
  <|> (IdrisKey <$> (P.char ':' *> P.many (P.alphaNum <|> P.char '-')))
  <|> (P.char '(' *> (IdrisBracketed <$> P.sepBy parseIdrisSExp (P.optional $ P.char ' ')) <* P.char ')')

readRawIdrisMessage :: Handle -> IO BS.ByteString
readRawIdrisMessage h = do
  lenData <- BS.hGet h 6
  let Just len = parseHexNumber (BS8.unpack lenData)
  remainingData <- BS.hGet h len
  return remainingData

parseHexNumber :: String -> Maybe Int
parseHexNumber nStr = fmap fromIntegral $
  eitherToJust $ P.parse (number 16 P.hexDigit) "" nStr
  where
    number base baseDigit
        = do
            digits <- P.many1 baseDigit
            let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            seq n (return n)
  
writeIdrisMessage :: Handle -> BS.ByteString -> IO ()
writeIdrisMessage h bs = do
  hPutStr h (printf "%06x" (BS.length bs))
  BS.hPut h bs
  hFlush h

readIdrisMessage :: Handle -> IO IdrisSExp
readIdrisMessage h = do
  msg <- readRawIdrisMessage h
  let Right sexp = P.parse parseIdrisSExp "" (BS8.unpack msg)
  return sexp

processIdrisMessagesUntilReturn :: Handle -> IO Bool
processIdrisMessagesUntilReturn h = do
  msg <- readIdrisMessage h
  case msg of
    IdrisBracketed [IdrisKey "warning", IdrisBracketed [IdrisStr source, IdrisBracketed [IdrisNum low, IdrisNum high], _, IdrisStr warning, _], _] -> do
      putStrLn $ "At " ++ (show source) ++ " " ++ (show low) ++ "-" ++ (show high) ++ ": " ++ warning
      processIdrisMessagesUntilReturn h
    IdrisBracketed [IdrisKey "return", IdrisBracketed [IdrisKey "ok", IdrisBracketed []], _] ->
      return True
    IdrisBracketed [IdrisKey "return", _, _] ->
      return False
    _ -> processIdrisMessagesUntilReturn h
  
processMove :: Coord -> String -> String -> IO ()
processMove c@(Coord x y) varPrefix expectedType = do
  let factCheck = "module CheckMove\n\
                 \  import ProofSweeperBase\n\
                 \  import ProofSweeperKnown\n\
                 \  import ProofSweeperPlay\n\
                 \  total\n\
                 \  checkMove : MineFact (MkCoord " ++ (show x) ++ " " ++ (show y)
                      ++ ") " ++ expectedType ++ "\n\
                 \  checkMove = " ++ varPrefix ++ "_" ++ (show x) ++ "_"
                      ++ (show y) ++ "\n"
  writeFile "CheckMove.idr" factCheck
  (Just hIn, Just hOut, _, procHandle) <- createProcess (proc "idris2" ["--ide-mode"]) {
      std_in = CreatePipe
    , std_out = CreatePipe
  }
  writeIdrisMessage hIn (BS8.pack "((:load-file \"CheckMove.idr\") 1)")
  result <- processIdrisMessagesUntilReturn hOut
  writeIdrisMessage hIn (BS8.pack "((:metavariables 100) 1)")
  result2 <- processIdrisMessagesUntilReturn hOut
  terminateProcess procHandle

  if result && not result2
    then putStrLn "There are holes remaining"
    else return ()

  if (not $ result && result2)
    then putStrLn "Not accepting move due to failure"
    else do
      Just game <- loadGame
      case M.lookup (Coord x y) (gameStatus game) of
        Just statusNow -> do
          let newStatus = statusNow { cellVisibility = CellKnown }
          let counts = gameToMineCounts game
          let game' = expandNotMinesWithoutNeighbours
                       (game { gameStatus = M.insert c newStatus (gameStatus game) })
                       counts
          saveGame game'
          saveIdrisGame game' counts
          putStrLn $ displayGame game' counts
        Nothing -> putStrLn "Proof of something that is not true - did you abuse believe_me / assert_total etc...?"
  
doMoveCommand :: String -> String -> String -> IO ()
doMoveCommand commandName xStr yStr =
    case (parseNumber xStr, parseNumber yStr) of
      (Just x, Just y) ->
        case commandName of
          "mine" -> processMove (Coord x y) "mineAt" "IsMine"
          "notmine" -> processMove (Coord x y) "noMineAt" "IsNotMine"
          _ -> putStrLn "First argument to move must be mine or notmine"
      _ -> putStrLn "Expected numbers as second and third argument to move"
