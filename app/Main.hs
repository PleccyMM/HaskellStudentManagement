module Main (main) where

import Lib
import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import Control.Monad (mzero)

data Student = Student {
    firstName :: !Text,
    lastName :: !Text,
    age :: Int,
    modules :: [Text]
} deriving Show

instance FromJSON Student where
    parseJSON (Object v) = Student
        <$> v .:? "firstName"
        <*> v .: "lastName"
        <*> v .: "age"
        <*> v .: "modules"
    parseJSON _ = mzero

instance ToJSON Student where
    toJSON (Student firstName lastName age modules) = 
        object [
            "firstName" .= firstName,
            "lastName" .= lastName,
            "age" .= age,
            "modules" .= modules
        ]

parseJSONFromFile :: FilePath -> IO (Either String [Student])
parseJSONFromFile p = do
    c <- B.readFile p
    return $ eitherDecode c

main :: IO ()
main = do
    result <- parseJSONFromFile "test.json"
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right people -> mapM_ print people