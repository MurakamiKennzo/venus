-- Given a linked list, rotate the list to the right by k places, where k is non-negative.

module RotateRight
  (
    rotateRight
  ) where

rotateRight :: Link a -> Int -> Link a
rotateRight link k = let l = length' link
                     in  foldr (:->) Empty .
                         fmap fst .
                         reverse' .
                         sortOn' ((`div` l) . snd) $
                         tuple k <$> link <*> fromList [0 ..]

infixr 2 :-> 

data Link a = Empty | a :-> Link a deriving (Show)

instance Functor Link where
  fmap f Empty = Empty
  fmap f (a :-> link) = f a :-> fmap f link

instance Applicative Link where
  pure a = a :-> Empty
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (a :-> linka) <*> (b :-> linkb) = a b :-> linka <*> linkb

instance Semigroup (Link a) where
  (<>) = (+>)

instance Monoid (Link a) where
  mempty = Empty

instance Foldable Link where
  foldMap f Empty = mempty
  foldMap f (a :-> link) = f a <> foldMap f link 

reverse' :: Link a -> Link a
reverse' l = rev l Empty
  where rev :: Link a -> Link a -> Link a
        rev Empty a = a
        rev (a :-> link) b = rev link (a :-> b)

infixr 5 +>
(+>) :: Link a -> Link a -> Link a
Empty +> a = a
a +> Empty = a
(a :-> linka) +> b = a :-> linka +> b 

sortOn' :: (Ord b) => (a -> b) -> Link a -> Link a
sortOn' _ Empty = Empty
sortOn' f (a :-> link) = insert' f a . sortOn' f $ link

insert' :: (Ord b) => (a -> b) -> a -> Link a -> Link a
insert' _ a Empty = a :-> Empty
insert' f a (b :-> link) = if f a >= f b then b :-> insert' f a link else a :-> b :-> link

fromList :: [a] -> Link a
fromList [] = Empty
fromList (x:xs) = x :-> fromList xs

length' :: Link a -> Int
length' = foldl (\a b -> a + 1) 0

tuple :: Int -> a -> Int -> (a, Int)
tuple a b c = (b, a + c)
