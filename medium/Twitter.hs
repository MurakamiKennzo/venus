-- Design a simplified version of Twitter where users can post tweets, follow/unfollow another user and is able to see the 10 most recent tweets in the user's news feed. Your design should support the following methods:

-- postTweet(userId, tweetId): Compose a new tweet.
-- getNewsFeed(userId): Retrieve the 10 most recent tweet ids in the user's news feed. Each item in the news feed must be posted by users who the user followed or by the user herself. Tweets must be ordered from most recent to least recent.
-- follow(followerId, followeeId): Follower follows a followee.
-- unfollow(followerId, followeeId): Follower unfollows a followee.

module Twitter
  (
    twitter
  ) where

import Control.Monad.State ( StateT(..)
                           , lift )
import Data.List ( delete )

twitter :: IO ((), Twitter)
twitter = runStateT twitter' (Twitter [] [])

twitter' :: StateT Twitter IO ()
twitter' = do
  args <- lift getLine
  newsFeed <- whatArgs $ words args
  if newsFeed ==Nothing
    then twitter'
    else lift (print newsFeed) >> twitter'

whatArgs :: [String] -> StateT Twitter IO (Maybe [Tweet])
whatArgs ("postTweet":user:tweet:_) = postTweet (read user) (read tweet) >> return Nothing
whatArgs ("getNewsFeed":user:_) = getNewsFeed (read user) >>= return . Just
whatArgs ("follow":follower:followee:_) = follow (read follower) (read followee) >> return Nothing
whatArgs ("unfollow":follower:followee:_) = unfollow (read follower) (read followee) >> return Nothing
whatArgs _ = lift $ print "no parse" >> return Nothing

postTweet :: User -> Tweet -> StateT Twitter IO ()
postTweet user tweet = StateT $ \twitter -> return ((), insertPost user tweet twitter)

getNewsFeed :: User -> StateT Twitter IO [Tweet]
getNewsFeed user = StateT $ \twitter -> return (getNewsFeed' user twitter, twitter)

follow :: User -> User -> StateT Twitter IO ()
follow follower followee = StateT $ \twitter -> return ((), twitter { follows = (Follow follower followee): follows twitter })

unfollow :: User -> User -> StateT Twitter IO ()
unfollow follower followee = StateT $ \twitter -> return ((), twitter { follows = delete (Follow follower followee) (follows twitter) })

getNewsFeed' :: User -> Twitter -> [Tweet]
getNewsFeed' user (Twitter posts follows) = take 10 $ foldr (\(Post a b) c -> if a `elem` users then b:c else c) [] posts
  where followees = foldr (\(Follow a b) c -> if a == user then b:c else c) [] follows
        users = user: followees

insertPost :: User -> Tweet -> Twitter -> Twitter
insertPost user tweet twitter = twitter { posts = (Post user tweet): posts twitter }

data Twitter = Twitter { posts :: [Post]
                       , follows :: [Follow] }

data Post = Post User Tweet

data Follow = Follow { follower :: User
                     , followee :: User } deriving (Eq)

type User = Int
type Tweet = Int
