module Main ( main ) where
import Distribution.Simple.Test.LibV09 ( stubMain )
import My ( tests )
main :: IO ()
main = stubMain tests
