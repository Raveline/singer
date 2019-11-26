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
import           Data.Map                        (Map)
import qualified Data.Map                        as M
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
toTweet = S.mapMaybe fromTweetSrc

-- | A base filter that only removes answers and retweets
baseFilter :: (Monad m) => Text -> S.Stream (S.Of TweetSrc) m r -> S.Stream (S.Of TweetSrc) m r
baseFilter screenName = S.filter (validTweet screenName)


data ThreadState = ThreadState
     { nextTweet     :: Tweet
     , ongoingThread :: Map TweetId [Tweet]
     } deriving (Show)

addOngoingThread :: Tweet -> Map TweetId [Tweet] -> Map TweetId [Tweet]
addOngoingThread t ongoing = let
  insertTweet replies = pure $ t : concat replies
  updateRepliesList k = M.alter insertTweet k ongoing
  in maybe ongoing updateRepliesList (replyTo t)

getAndDelete :: Ord k => k -> Map k v -> (Maybe v, Map k v)
getAndDelete = M.updateLookupWithKey (const $ const Nothing)

attachReplies :: Tweet -> Maybe [Tweet] -> Tweet
attachReplies t replies = t {children = concat replies}


handleHead :: Tweet -> ThreadState
handleHead t = let
  initOngoingThread = maybe mempty (`M.singleton` [t]) (replyTo t)
  in ThreadState t initOngoingThread

handleTail' :: Tweet -> Map TweetId [Tweet] -> ThreadState
handleTail' t ongoing = let
  (replies, ongoing') = getAndDelete (id_ t) ongoing
  current = attachReplies t replies
  ongoingWithCurrent = addOngoingThread current ongoing'
  in ThreadState current ongoingWithCurrent

handleTail :: Tweet -> ThreadState -> ThreadState
handleTail t = handleTail' t . ongoingThread

handleTweet :: Maybe ThreadState -> Tweet -> Maybe ThreadState
handleTweet st t = pure $ maybe (handleHead t) (handleTail t) st


extractThread :: Maybe ThreadState -> Maybe Thread
extractThread st = let

  go :: Tweet -> Maybe Thread
  go t = do
    guard . isNothing $ replyTo t
    guard . not . null $ children t
    mkThread t

  in st >>= go . nextTweet


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
streamThreads filepath screenName pred'
  = usingFile filepath
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
