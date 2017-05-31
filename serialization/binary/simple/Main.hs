{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Binary
import GHC.Generics (Generic)

data Foo = Foo deriving (Generic, Show)
data Boo = Boo Int deriving (Generic, Show)

instance Binary Foo
instance Binary Boo

main :: IO ()
main = do
  print $ encode Foo
  print $ show $ (decode $ encode Foo :: Foo)
  print $ encode $ Boo 5
  print $ show $ (decode $ encode $ Boo 5 :: Boo)
