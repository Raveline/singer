{-# LANGUAGE OverloadedStrings #-}
module Spec
  (
    main
  ) where

import Control.Monad (join)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Test.Hspec
import Tweets (fixture)
import Singer.Stream
import Singer.Model (Thread(..))
import Streaming.Prelude (head_)

main :: IO ()
main = hspec $
  describe "Streaming outputs threads" $ do
    let source :: Maybe Thread
        source = join $ streamThreads' (usingBS fixture) "Test" & head_
        thread :: Thread
        thread = fromMaybe (error "No thread fetched !") source
    it "Even with tweets in between" $
      length (tweets thread) `shouldBe` 3
    it "Threads are in the proper order" $
      tweets thread `shouldBe` ["Beginning of thread", "Middle of thread", "End of thread"]
