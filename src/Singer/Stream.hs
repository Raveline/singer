{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Singer.Stream
  (
    -- * Main tools
    streamThreads
  , streamThreads'
    -- * Tweet sources
  , usingFile
  , usingBS
  , baseFilter
  , module Singer.Model
  ) where

import           Control.Monad                   (guard)
import           Control.Monad.Trans.Resource    (MonadResource)
import qualified Data.ByteString.Lazy            as BS (ByteString)
import qualified Data.ByteString.Streaming       as SBS
import           Data.ByteString.Streaming.Aeson (streamParse)
import           Data.Function                   ((&))
import           Data.JsonStream.Parser          (Parser, arrayOf, value)
import           Data.Maybe                      (isNothing)
import           Data.Text                       (Text)
import           Singer.Model                    (Thread (..), Tweet (..),
                                                  TweetAncestors, TweetId (..),
                                                  TweetSrc (..), fromTweetSrc,
                                                  mkThread)
import           Streaming
import qualified Streaming.Prelude               as S

usingFile :: (MonadResource m) => FilePath -> S.Stream (S.Of TweetSrc) m ()
usingFile = void . streamParse (arrayOf parserTweet) . SBS.readFile

usingBS :: (Monad m) => BS.ByteString -> S.Stream (S.Of TweetSrc) m ()
usingBS = void . streamParse (arrayOf parserTweet) . SBS.fromLazy

parserTweet :: Parser TweetSrc
parserTweet = value

-- | Basic validation: we do not pick retweets, and we only pick replies to self.
validTweet :: Text -> TweetSrc -> Bool
validTweet screenName t = not (retweeted t) && maybe True (== screenName) (inReplyToScreenName t)

-- | Morphs Twitter source representation to a more strongly typed model
toTweet :: (Monad m) => S.Stream (S.Of TweetSrc) m r -> S.Stream (S.Of Tweet) m r
toTweet = S.catMaybes . S.map fromTweetSrc

-- | A base filter that only removes answers and retweets
baseFilter :: (Monad m) => Text -> S.Stream (S.Of TweetSrc) m r -> S.Stream (S.Of TweetSrc) m r
baseFilter screenName = S.filter (validTweet screenName)

foundParent :: Maybe Tweet -> Tweet -> Maybe Tweet
foundParent child' parent = pure $ parent { child = child' }

didNotFindParent :: Maybe Tweet -> Tweet -> Maybe Tweet
didNotFindParent child' parent = maybe (pure parent) (const child')  (replyTo =<< child')

extractThread :: Maybe Tweet -> Maybe Thread
extractThread tweet = do
  t <- tweet
  completedThread t
  hasMultipleTweets t
  mkThread t
    where
      completedThread   = guard . isNothing . replyTo
      hasMultipleTweets = void . child


handleTweet :: Maybe Tweet -> Tweet -> Maybe Tweet
handleTweet child' parent = let
  childOfParent = pure (id_ parent) == (child' >>= replyTo)
  in if childOfParent
        then foundParent child' parent
        else didNotFindParent child' parent

toThread :: (Monad m) => S.Stream (S.Of Tweet) m () -> S.Stream (S.Of Thread) m ()
toThread = S.catMaybes . S.scan handleTweet Nothing extractThread

-- | This is the main utility of this library. It streams threads read from
-- a file containing a valid JSON with your tweets.
-- Since this demands opening a file, you will need to wrap your call in a
-- `runResourceT`.
--
-- Basic example:
--
-- > runResourceT $ streamThreads filepath "yourname" id
streamThreads :: (MonadResource m)
  => FilePath
  -- ^ Twitter archive file name
  -> Text
  -- ^ Your twitter screen name, used to identify threads
  -> (Thread -> Bool)
  -- ^ Thread filter - use id if you don't want to filter anything
  -> Stream (S.Of Thread) m ()
  -- ^ Result as a stream of threads.
streamThreads filepath screenName pred' =
  usingFile filepath
  & baseFilter screenName
  & toTweet
  & toThread
  & S.filter pred'

-- | Variant of `streamThreads` that let you replace the initial source by
-- an arbitrary provider (e.g., a file with `usingBS`; or potentially, a
-- twitter API call).
streamThreads' :: (Monad m)
  => S.Stream (S.Of TweetSrc) m ()
  -> Text
  -> Stream (S.Of Thread) m ()
streamThreads' source screenName =
  source & baseFilter screenName & toTweet & toThread
