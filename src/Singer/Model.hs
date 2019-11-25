{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Singer.Model
  ( Tweet(..)
  , TweetId(..)
  , Thread(..)
  , TweetSrc(..)
  , TweetAncestors
  , fromTweetSrc
  , mkThread
  )
where

import           Control.Monad                  ( guard )
import           Data.Aeson                     ( FromJSON(..)
                                                , camelTo2
                                                , defaultOptions
                                                , fieldLabelModifier
                                                , genericParseJSON
                                                )
import           Data.Monoid                    ( Sum(..) )
import           Data.Tree                      ( Tree
                                                , unfoldTree
                                                )
import qualified Data.Map                      as M
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , parseTimeM
                                                )
import           GHC.Generics
import           Text.Read                      ( readMaybe )

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
        , children   :: [Tweet]
        }
  deriving (Eq, Ord, Show)

data Thread =
  Thread { firstTweetAt :: UTCTime
         , totalFav     :: Int
         , tweets       :: Tree Text
         }
  deriving (Eq, Show)

tweetUnfolder :: Tweet -> (Tweet, [Tweet])
tweetUnfolder t = (t { children = mempty }, children t)

mkThread' :: Tweet -> Thread
mkThread' t =
  let unfolded            = unfoldTree tweetUnfolder t
      texts               = fmap content unfolded
      threadLikesAndTexts = getSum $ foldMap (Sum . favNb) unfolded
  in  Thread (date t) threadLikesAndTexts texts

mkThread :: Tweet -> Maybe Thread
mkThread t = do
  guard (not $ null $ children t)
  pure $ mkThread' t

fromTweetSrc :: TweetSrc -> Maybe Tweet
fromTweetSrc (TweetSrc _ fc iRT tId cAt _ fT) =
  Tweet
    <$> readMaybe (unpack fc)
    <*> pure tId
    <*> pure iRT
    <*> parseTimeM True defaultTimeLocale "%c" (unpack cAt)
    <*> pure fT
    <*> pure mempty

instance FromJSON TweetSrc where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
