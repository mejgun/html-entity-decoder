{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Decoder

main :: IO ()
main = print $ decode "can&#00000000039;t &amp;"
