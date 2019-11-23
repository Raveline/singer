{-# LANGUAGE QuasiQuotes #-}
module Tweets
  (
    fixture
  ) where

import Data.String.QQ
import Data.ByteString.Lazy

fixture :: ByteString
fixture =
  [s|
    [{
      "retweeted" : false,
      "favorite_count" : "4",
      "id" : "1193836505728520020",
      "in_reply_to_status_id" : "1193836505728520019",
      "created_at" : "Mon Nov 11 10:22:58 +0000 2019",
      "full_text" : "End of thread",
      "in_reply_to_screen_name" : "Test"
    },{
      "retweeted" : false,
      "favorite_count" : "0",
      "id" : "1193836505728520019",
      "in_reply_to_status_id" : "1193836505728520017",
      "created_at" : "Mon Nov 11 10:21:54 +0000 2019",
      "full_text" : "Middle of thread",
      "in_reply_to_screen_name" : "Test"
    },{
      "retweeted" : false,
      "favorite_count" : "0",
      "id" : "1193836505728520018",
      "created_at" : "Mon Nov 11 10:20:54 +0000 2019",
      "full_text" : "This is not in a thread"
    },{
      "retweeted" : false,
      "favorite_count" : "0",
      "id" : "1193836505728520017",
      "created_at" : "Mon Nov 11 10:19:54 +0000 2019",
      "full_text" : "Beginning of thread"
    },{
      "retweeted" : false,
      "favorite_count" : "0",
      "id" : "1193836505728520016",
      "created_at" : "Mon Nov 11 10:20:54 +0000 2019",
      "full_text" : "This is something else entirely"
    },
    ]
  |]
