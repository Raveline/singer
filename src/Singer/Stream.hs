{-# LANGUAGE FlexibleContexts #-}
module Singer.Stream
  (
    -- * Main tools
    getThreads
  , buildAncestors
    -- * Stream helpers
  , tweetStream
  , baseFilter
    -- * Debugging utilities
  , printThreads
  , module Singer.Model
  ) where

import           Control.Monad.State             (MonadState, gets, modify)
import           Control.Monad.Trans.Resource    (MonadResource)
import           Data.ByteString.Streaming       as BS
import           Data.ByteString.Streaming.Aeson (streamParse)
import           Data.Foldable                   (traverse_)
import           Data.Function                   ((&))
import           Data.JsonStream.Parser          (Parser, arrayOf, value)
import qualified Data.Map                        as M
import           Data.Maybe                      (mapMaybe)
import           Data.Text                       (Text)
import qualified Data.Text.IO                    as TIO (putStrLn)
import           Singer.Model                    (Thread (..), Tweet (..),
                                                  TweetAncestors, TweetId (..),
                                                  TweetSrc (..), fromTweetSrc,
                                                  mkThread)
import           Streaming
import qualified Streaming.Prelude               as S

getTweets :: (MonadResource m) => FilePath -> S.Stream (S.Of TweetSrc) m (Maybe String, BS.ByteString m ())
getTweets = streamParse (arrayOf parserTweet) . BS.readFile

mapTweets :: (MonadState TweetAncestors m) => Tweet -> m ()
mapTweets x = do
  withChild <- findChild x
  maybe (finishThread withChild) (storeTweet withChild) (replyTo x)

findChild :: (MonadState TweetAncestors m) => Tweet -> m Tweet
findChild current =
  gets (M.lookup (id_ current))
  >>= maybe (pure current) (associateChild current)

associateChild :: (MonadState TweetAncestors m) => Tweet -> Tweet -> m Tweet
associateChild parent child' = do
  modify (M.delete (id_ child'))
  pure $ parent { child = Just child' }

-- This tweet is not a reply to something: it is the first of a thread
-- or a single tweet.
finishThread :: (MonadState TweetAncestors m) => Tweet -> m ()
finishThread t = modify $ M.insert (id_ t) t

-- This tweet is an answer to a non-yet met parent, we store it and its parent id.
storeTweet :: (MonadState TweetAncestors m) => Tweet -> TweetId -> m ()
storeTweet current parentId = modify $ M.insert parentId current

parserTweet :: Parser TweetSrc
parserTweet = value

validTweet :: Text -> TweetSrc -> Bool
validTweet screenName t = not (retweeted t) && (maybe True (== screenName) (inReplyToScreenName t))

-- | Simple producer of tweets from a JS file.
tweetStream :: (MonadResource m) => FilePath -> S.Stream (S.Of TweetSrc) m ()
tweetStream = void . getTweets

-- | Morphs Twitter source representation to a more strongly typed model
toTweet :: (MonadState TweetAncestors m) => S.Stream (S.Of TweetSrc) m r -> S.Stream (S.Of Tweet) m r
toTweet = S.catMaybes . S.map fromTweetSrc

-- | A base filter that only removes answers and retweets
baseFilter :: (MonadState TweetAncestors m) => Text -> S.Stream (S.Of TweetSrc) m r -> S.Stream (S.Of TweetSrc) m r
baseFilter screenName = S.filter (validTweet screenName)

-- | Morphs a stream of tweets into a list of tweet with their children
toThreads :: (MonadState TweetAncestors m) => S.Stream (S.Of TweetSrc) m r -> m r
toThreads = S.mapM_ mapTweets . toTweet

-- | A stream builder that composes:
-- - A source of tweet, typically built using `tweetStream`
-- - A filter for source tweets, a basic one is provided in `baseFilter`
-- All these are then fed into a converter that will trace tweets
-- history.
--
-- Since fetching the tweets typically require a MonadResource and
-- tweet history is constructed using a MonadState (over a Map), you
-- need to run this into a `runResourceT` and a `execStateT f M.empty` to
-- evaluate the stream.
buildAncestors :: (MonadResource m, MonadState TweetAncestors m)
  => S.Stream (S.Of TweetSrc) m r
  -> (S.Stream (S.Of TweetSrc) m r -> S.Stream (S.Of TweetSrc) m r)
  -> m r
buildAncestors source filters =
  source & filters & toThreads

-- | Main utility. You build the first parameter using `buildAncestors`.
-- Basic example:
--
--
-- > import Singer.Stream
-- > import qualified Data.Map as M (empty)
-- > source :: IO TweetAncestors
-- > source =
-- >    let = buildAncestors (tweetStream "tweets.js") (baseFilter "MyTweeterNickName")
-- >    in runResourceT $ execStateT stream M.empty
-- >
-- > main :: IO ()
-- > main = getThreads source >>= printThreads
--
-- You can easily intercalate something like the
getThreads :: IO TweetAncestors -> IO [Thread]
getThreads f = mapMaybe mkThread . M.elems <$> f

-- | For debugging purpose
printThreads :: [Thread] -> IO ()
printThreads =
  let go x = putStrLn (show (firstTweetAt x))
           >> traverse_ TIO.putStrLn  (tweets x)
           >> putStrLn "-------------------------------"
  in traverse_ go
