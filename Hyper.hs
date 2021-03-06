{-
 - Hyper is an HTTP server.
 -}
module Hyper where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hGetContents, hIsEOF, hIsReadable, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Data.String.Utils
import Data.List.Split (splitOn)
import Data.List (intercalate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Time
import System.Directory (doesFileExist)
import Text.Regex

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

instance Show RequestLine where
  show (RequestLine method uri version) =
    method ++ " " ++ uri ++ " " ++ version

data Header = Header
        { name :: String
        , value :: String
        }

instance Show Header where
  show header = (name header) ++ ":" ++ " " ++ (value header)

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

type Processor = Handle -> Request -> IO ()

type Route = (String, Processor)

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

newLine :: String
newLine = "\r\n"

writeResponse :: Handle -> Response -> IO ()
writeResponse handle response = do
  putStrLn $ show $ statusLine response
  hPutStrLn handle (show response)

ok :: String -> Processor
ok content = \handle request -> writeResponse handle (Response (StatusLine "HTTP/1.1" "200" "OK") [] content)

notFound :: Processor
notFound = \handle request -> writeResponse handle (Response (StatusLine "HTTP/1.1" "404" "Not Found") [] "")

bufferSize :: Int
bufferSize = 4096

readAll :: Handle -> String -> IO String
readAll handle input = do
  line <- B.hGetNonBlocking handle bufferSize
  if line == C.empty && (not $ null input)
  then return input
  else readAll handle (input ++ C.unpack line)

match :: Request -> Route -> Bool
match request route = --uri (requestLine request) == fst route
  let path = uri $ requestLine request
      expression = mkRegex $ fst route
        in case matchRegex expression path of
          Nothing -> False
          Just _ -> True

getProcessor :: Request -> [Route] -> Processor
getProcessor request routes =
  foldl (\result route -> if match request route then snd route else result) notFound routes

logRequest :: Request -> IO ()
logRequest request = do
  time <- fmap show getZonedTime
  let path = show $ requestLine request
        in putStrLn $ time ++ " " ++ path

process :: Handle -> [Route] -> IO ()
process handle routes = do
  contents <- readAll handle []
  let request = parseRequest contents
      processor = getProcessor request routes
        in do
          logRequest request
          processor handle request

connect :: Socket -> [Route] -> IO ()
connect socket routes = do
  (handle, hostName, portNumber) <- accept socket
  hSetBuffering handle NoBuffering
  forkIO $ process handle routes
  connect socket routes

serve :: [Route] -> IO ()
serve dispatcher  = withSocketsDo $ do
  socket <- listenOn $ PortNumber 3000
  connect socket dispatcher

mimeTypes =
  [ ("html", "text/html")
  ]

mimeType :: String -> String
mimeType path =
  let extension = last $ splitOn "." path
        in foldl (\result pair -> if extension == fst pair then snd pair else result) "application/octet-stream" mimeTypes

serveFile :: String -> Processor
serveFile path = \handle request -> do
  exists <- doesFileExist path
  if exists
  then do
    content <- readFile path
    let contentType = Header "Content-Type" (mimeType path)
        contentLength = Header "Content-Length:" (show $ length content)
        in do
          content <- readFile path
          putStrLn $ show contentType
          writeResponse handle (Response (StatusLine "HTTP/1.1" "200" "0K") [contentType, contentLength] content)
  else
    notFound handle request

serveDirectory :: String -> String -> Processor
serveDirectory directory prefix = \handle request ->
  let path = drop (length prefix) (uri $ requestLine request)
      filePath = directory ++ path
        in (serveFile filePath) handle request

routes :: [Route]
routes =
  [ ("/",               ok "Welcome to the home page!")
  , ("/intranet",       ok "Welcome to the intranet!")
  , ("/www/.*",         serveDirectory "www" "/www")
  ]

main :: IO ()
main = serve routes

