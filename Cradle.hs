module Main 
where

import System.Environment

main :: IO()
main = getArgs >>= pp . head
		  

parse :: String -> String
parse s = s

emit :: String -> String

pp = putStrLn . parse