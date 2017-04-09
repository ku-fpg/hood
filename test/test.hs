#!/usr/bin/env stack
{-# LANGUAGE DeriveGeneric #-}
-- stack --resolver lts-8.4 runghc --package hood
-- Minimum program to reproduce https://github.com/ku-fpg/hood/issues/5

import           Debug.Hood.Observe
import           GHC.Generics


data Hoge =
  Hoge
    { hoge1 :: Int
    , hoge2 :: String
    } deriving Generic

instance Observable Hoge


main :: IO ()
main = runO $ print $ f undefined


f :: Hoge -> Bool
f = observe "f" f'
f' :: Hoge -> Bool
f' h@(Hoge _h1 _h2) = error "ERROR"
