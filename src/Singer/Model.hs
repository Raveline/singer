{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Singer.Model
  (
    Tweet(..)
  , TweetId(..)
  , Thread(..)
  , TweetSrc(..)
  , TweetAncestors
  , fromTweetSrc
  , mkThread
  ) where

import           Data.Aeson         (FromJSON (..), camelTo2, defaultOptions,
                                     fieldLabelModifier, genericParseJSON)
import           Data.Foldable      (foldl')
import           Data.List.NonEmpty (unfoldr)
import qualified Data.List.NonEmpty as NE (head)
import qualified Data.Map           as M
import           Data.Text          (Text, unpack)
import           Data.Time          (UTCTime, defaultTimeLocale, parseTimeM)
import           GHC.Generics
import           Text.Read          (readMaybe)

newtype TweetId = TweetId { getTweetId :: Text }
  deriving newtype (Eq, Ord, Show, FromJSON)

type ParentId = TweetId

type TweetAncestors = M.Map ParentId Tweet

-- | Internal representation of tweets as given by twitter.
data TweetSrc =
  TweetSrc { retweeted           :: Bool
           , favoriteCount       :: Text
           , inReplyToStatusId   :: Maybe TweetId
           , id                  :: TweetId
           , createdAt           :: Text
           , inReplyToScreenName :: Maybe Text
           , fullText            :: Text
           }
  deriving (Eq, Show, Generic)

data Tweet =
  Tweet { favNb   :: Int
        , id_     :: TweetId
        , replyTo :: Maybe TweetId
        , date    :: UTCTime
        , content :: Text
        , child   :: Maybe Tweet
        }
  deriving (Eq, Ord, Show)

data Thread =
  Thread { firstTweetAt :: UTCTime
         , totalFav     :: Int
         , tweets       :: [Text]
         }
  deriving (Eq, Ord, Show)

tweetUnfolder :: Tweet -> (Tweet, Maybe Tweet)
tweetUnfolder t = ( t { child = Nothing }, child t )

mkThread' :: Tweet -> Thread
mkThread' t =
  let unfolded = unfoldr tweetUnfolder t
      extract (favs, txts) tweet = (favs + favNb tweet, txts <> pure (content tweet))
      threadLikesAndTexts = foldl' extract (0, mempty) unfolded
  in uncurry (Thread (date . NE.head $ unfolded)) threadLikesAndTexts

mkThread :: Tweet -> Maybe Thread
mkThread t = maybe Nothing (const . pure . mkThread' $ t) $ child t

fromTweetSrc :: TweetSrc -> Maybe Tweet
fromTweetSrc (TweetSrc _ fc iRT tId cAt _ fT) =
  Tweet <$> readMaybe (unpack fc)
        <*> pure tId
        <*> pure iRT
        <*> parseTimeM True defaultTimeLocale "%c" (unpack cAt)
        <*> pure fT
        <*> pure Nothing

instance FromJSON TweetSrc where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
