
module Hyper where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hGetContents, hIsEOF, hIsReadable, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Data.String.Utils
import Data.List.Split (splitOn)
import Data.List (intercalate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Request = Request
        { requestLine :: RequestLine
        , requestHeaders :: [Header]
        , requestBody :: Body
        }
        deriving (Show)

data RequestLine = RequestLine
        { method :: String
        , uri :: String
        , version :: String
        }
        deriving (Show)

data Header = Header
        { name :: String
        , value :: String
        }
        deriving (Show)

type Body = String

data Response = Response
        { statusLine :: StatusLine
        , responseHeaders :: [Header]
        , responseBody :: Body
        }

instance Show Response where
  show response =
        (show $ statusLine response)
        ++ newLine
        ++ intercalate newLine (map show (responseHeaders response))
        ++ newLine
        ++ newLine
        ++ (show $ responseBody response)

data StatusLine = StatusLine
        { statusVersion :: String
        , status :: String
        , reason :: String
        }

instance Show StatusLine where
  show (StatusLine version status reason) =
    version ++ " " ++ status ++ " " ++ reason

type Processor = Request -> Response

parseRequestLine :: String -> RequestLine
parseRequestLine line =
        let [method, uri, version] = words $ rstrip line
        in RequestLine method uri version

parseHeader :: String -> Header
parseHeader line =
        let isNotColon = \c -> c /= ':'
            name = takeWhile isNotColon line
            value = strip $ drop 1 (dropWhile isNotColon line)
        in Header name value

parseRequest :: String -> Request
parseRequest contents =
  let [requestLineAndHeaders, body] = splitOn (newLine ++ newLine) contents
      requestEnveloppe = splitOn newLine requestLineAndHeaders
      requestLine = parseRequestLine $ requestEnveloppe !! 0
      headers = map (\line -> parseHeader line) (drop 1 requestEnveloppe)
        in Request requestLine headers body

connect :: Socket -> IO ()
connect socket = do
  (handle, hostName, portNumber) <- accept socket
  hSetBuffering handle NoBuffering
  forkIO $ process handle
  connect socket

newLine :: String
newLine = "\r\n"

helloWorldResponse :: Response
helloWorldResponse = Response (StatusLine "HTTP/1.1" "200" "OK") [] "Hello world!"

writeResponse :: Handle -> Response -> IO ()
writeResponse handle response = do hPutStrLn handle (show response)

bufferSize :: Int
bufferSize = 2048

readAll :: Handle -> String -> IO String
readAll handle input = do
  line <- B.hGetNonBlocking handle bufferSize
  if line == C.empty
  then return input
  else readAll handle (input ++ C.unpack line)

process :: Handle -> IO ()
process handle = do
  contents <- readAll handle []
  let request = parseRequest contents
        in do
          putStrLn $ show request
          writeResponse handle helloWorldResponse

main :: IO ()
main = withSocketsDo $ do
  socket <- listenOn $ PortNumber 3000
  connect socket

