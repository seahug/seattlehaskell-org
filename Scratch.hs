module Main (main) where

import Util.Slack
import System.Environment

main :: IO ()
main = do
    token <- getEnv "SLACK_API_TOKEN"
    result <- inviteUser token "xxx"
    putStrLn $ "result=" ++ show result
