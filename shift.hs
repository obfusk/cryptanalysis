#!/usr/bin/runhaskell

import Data.Char
import System.Environment

main = do [n] <- getArgs; interact (shift (read n) . normalize)

normalize = map toLower . filter (\x -> isAsciiLower x || isAsciiUpper x)

shift n = map (\x -> chr (((ord x) - (ord 'a') + n) `mod` 26 + (ord 'a')))
