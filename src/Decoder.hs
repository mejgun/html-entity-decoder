{-# LANGUAGE OverloadedStrings #-}

module Decoder
  ( decode
  )
where

import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe                     ( fromMaybe )
import           Map                            ( entities )

decode :: T.Text -> T.Text
decode t = let (a, b) = T.foldl foldF (T.empty, T.empty) t in T.concat [a, b]

foldF :: (T.Text, T.Text) -> Char -> (T.Text, T.Text)
foldF (res, acc) c = case T.singleton c of
  "&" -> (T.concat [res, acc], "&")
  ";" ->
    let acc'   = T.concat [acc, ";"]
        newRes = fromMaybe acc' $ HM.lookup (fixEnt acc') entities
    in  (T.concat [res, newRes], T.empty)
  char -> if T.null acc
    then (T.concat [res, char], acc)
    else (res, T.concat [acc, char])

fixEnt :: T.Text -> T.Text
fixEnt t =
  let c  = T.count "0" t
      t' = iterate (T.replace "&#0" "&#") t !! c
  in  T.toLower t'

