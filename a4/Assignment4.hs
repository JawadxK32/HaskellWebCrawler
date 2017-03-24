--Jawad Khudadad #500526749, Ievgeny Krupchenko #500567156
--DATE: 2016.04.15
--CPS506 Winter 2016 Assignment4

module Assignment4 where
  import Network.HTTP.Client
  import Network.HTTP.Types.Status (statusCode)
  import Network.URI

  import Text.Regex.Posix

  import Data.List
  import Data.List.Split

  import Control.Parallel

  import System.Environment
  import System.Exit

  import Data.Map (Map)
  import qualified Data.Map as Map

  main :: IO ()
  main = do
  	startOn "http://cps506.sarg.ryerson.ca" []

  startOn :: String -> [String] -> IO()
  startOn url [] = case parseURI url of
    Just _ -> webStats url 3
    Nothing -> print "Need Valid Address!"

  splitTags :: String -> String -> Integer -> IO()
  splitTags body url searchURL = do
    let tags = concat(Prelude.map extractTags $ splitOn ">" body)
    let links = concat(Prelude.map extractLinks $ splitOn "/a>" body)
    let tagsF = Data.List.nub tags
    putStrLn ("URL: " ++ url)
    countHTMLTags tagsF body
    crawlF links searchURL searchURL

   webStats :: String -> Integer -> IO()
   webStats url searchURL = do 
    putStrLn url
    mgr <- newManager defaultManagerSettings
    sendRequest <- parseUrl url
    returnMSG <- httpLbs sendRequest mgr
    if ((show $ statusCode $ responseStatus returnMSG) <= (show 299))
      then do 
        splitTags (show (responseBody returnMSG)) url searchURL
      else 
        print("")

  crawlF :: [String] -> Integer -> Integer -> IO()
  crawlF _ 0 _ = print ""
  crawlF [] _ _ = print ""
  crawlF (link:links) counter searchURL = do
    putStrLn (show counter)
    webStats link searchURL
    crawlF links (counter - 1) searchURL

  countHTMLTags :: [String] -> String -> IO()
  countHTMLTags (tag:tags) html = do
    let counter = length (splitOn tag html) - 1
    putStrLn (tag ++ " " ++ (show counter))
    countHTMLTags tags html
    countHTMLTags [] _ = putStrLn ("------------------")

  getLinks :: String -> [String]
  getLinks text = do
    let (_, link, content) = text =~ "(http://[^\"]*)" :: (String, String, String)
    if not (link == "" && content == "")
      then do 
        Prelude.map last (text =~ "(http://[^\"]*)" :: [[String]])
      else []

  getTags :: String -> [String]
  getTags text = do
    let (_, tag, content) = text =~ "<([a-zA-Z][a-zA-Z0-9]*[ ])" :: (String, String, String)
    if not (tag == "" && content == "")
      then do 
        Prelude.map last (text =~  "<([a-zA-Z][a-zA-Z0-9]*[ ])" :: [[String]])
      else []
