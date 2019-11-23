{-# LANGUAGE FlexibleContexts #-}
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

import           Control.Monad.Trans.Resource    (MonadResource)
import qualified Data.ByteString.Lazy            as BS (ByteString)
import qualified Data.ByteString.Streaming       as SBS
import           Data.ByteString.Streaming.Aeson (streamParse)
import           Data.Function                   ((&))
import           Data.JsonStream.Parser          (Parser, arrayOf, value)
import           Data.Text                       (Text)
import           Singer.Model                    (Thread (..), Tweet (..),
                                                  TweetAncestors, TweetId (..),
                                                  TweetSrc (..), fromTweetSrc,
                                                  mkThread)
import           Streaming
import           Streaming.Internal (Stream(Effect, Return))
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

foundParent :: (Monad m) => S.Stream (S.Of Tweet) m () -> Maybe Tweet -> Tweet -> S.Stream (S.Of Thread) m ()
foundParent src child' parent =
  let withChild = parent { child = child' }
      asThread = maybe (toThread' src Nothing) S.yield . mkThread $ withChild
  in maybe asThread (const $ toThread' src (Just withChild)) (replyTo parent)

didNotFindParent :: (Monad m) => S.Stream (S.Of Tweet) m () -> Maybe Tweet -> Tweet -> S.Stream (S.Of Thread) m ()
didNotFindParent src child' parent =
  let onIsReply = toThread' src (pure parent)
      onIsNotReply = toThread' src Nothing
      startNewThread = maybe onIsNotReply (const onIsReply) (replyTo parent)
      continueCurrentThread = toThread' src child'
  in maybe startNewThread (const continueCurrentThread) child'

handleTweet :: (Monad m) => Maybe Tweet -> Tweet -> S.Stream (S.Of Tweet) m () -> S.Stream (S.Of Thread) m ()
handleTweet child' parent src =
  let childOfParent = pure (id_ parent) == (child' >>= replyTo)
  in if childOfParent
        then foundParent src child' parent
        else didNotFindParent src child' parent

toThread' :: (Monad m) => S.Stream (S.Of Tweet) m () -> Maybe Tweet -> S.Stream (S.Of Thread) m ()
toThread' src current = Effect $ do
  n <- S.next src
  case n of
    Left l -> pure $ Return l
    Right r -> pure $ uncurry (handleTweet current) r

toThread :: (Monad m) => S.Stream (S.Of Tweet) m () -> S.Stream (S.Of Thread) m ()
toThread src = toThread' src Nothing

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
