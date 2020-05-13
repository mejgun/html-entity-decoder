{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Decoder

main :: IO ()
main = print $ decode "can&#039;t &amp;"
