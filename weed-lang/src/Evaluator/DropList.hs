module Evaluator.DropList where

data DropItem a = K a | D a deriving (Eq, Show, Ord)

unDropItem :: DropItem a -> a
unDropItem (K a) = a
unDropItem (D a) = a

instance Functor DropItem where
  fmap f (K a) = K . f $ a
  fmap f (D a) = D . f $ a

forceDrop :: DropItem a -> DropItem a
forceDrop (K x) = D x
forceDrop (D x) = D x

newtype DropList a = DropList {getItems :: [DropItem a]} deriving (Eq, Show)

getKept :: DropList a -> [a]
getKept (DropList xs) = [x | K x <- xs]

instance Semigroup (DropList a) where
  DropList a <> DropList b = DropList $ a <> b

instance Monoid (DropList a) where
  mempty = DropList []

instance One (DropList a) where
  type OneItem (DropList a) = a
  one = DropList . (:[]) . K

toDropList :: [a] -> DropList a
toDropList = DropList . map K

instance Functor DropList where
  fmap f (DropList xs) = DropList $ map (fmap f) xs

instance Applicative DropList where
  pure = one
  DropList fs <*> DropList xs = DropList $ do
    fItem <- fs
    xItem <- xs
    let val = unDropItem fItem (unDropItem xItem)
    return $ case (fItem, xItem) of
      (K _, K _) -> K val
      _          -> D val

instance Monad DropList where
  DropList xs >>= f = DropList $ do
    item <- xs
    let DropList ys = f (unDropItem item)
    yItem <- ys
    return $ case item of
      K _ -> yItem
      D _ -> forceDrop yItem

zipOne :: (a -> b -> c) -> DropItem a -> DropItem b -> DropItem c
zipOne f (K a) (K b) = K $ f a b
zipOne f a b = D $ f (unDropItem a) (unDropItem b)

zipMOne :: Monad m => (a -> b -> m c) -> DropItem a -> DropItem b -> m (DropItem c)
zipMOne f (K a) (K b) = K <$> f a b
zipMOne f a b = D <$> f (unDropItem a) (unDropItem b)

zipWithDropList :: (a -> b -> c) -> DropList a -> DropList b -> DropList c
zipWithDropList f (DropList xs) (DropList ys) = DropList $ zipWith (zipOne f) xs ys

zipWithMDropList :: Monad m => (a -> b -> m c) -> DropList a -> DropList b -> m (DropList c)
zipWithMDropList f (DropList xs) (DropList ys) = DropList <$> zipWithM (zipMOne f) xs ys

replicateDropList :: Int -> a -> DropList a
replicateDropList n = DropList . map K . replicate n

replicateMDropList :: Monad m => Int -> m a -> m (DropList a)
replicateMDropList n x = do
  l <- replicateM n x
  return $ toDropList l

consDropList :: DropItem a -> DropList a -> DropList a
consDropList x (DropList xs) = DropList (x : xs)

consKeep :: a -> DropList a -> DropList a
consKeep x (DropList xs) = DropList $ (K x):xs

liftPredicate :: ([a] -> [Bool]) -> DropList a -> DropList Bool
liftPredicate p dl@(DropList xs) = DropList $ go xs (p $ getKept dl)
  where
    go :: [DropItem a] -> [Bool] -> [DropItem Bool]
    go [] _ = []
    go (D _ : rest) bs = D False : go rest bs
    go (K _ : rest) (b:bs) = K b : go rest bs
    go (K _ : rest) [] = K False : go rest []

-- okay, we are explicitly not giving DropList
-- foldable / traversible instances (which would let us just use mapM and sequence)
-- becuase they would necessarily have to violate drop semantics, and I
-- don't want to expose that. so we're just reimplementing it here, with
-- the explicit acknowledgement that this is ignoring K/D semantics on purpose.
-- uses of this should still fit with the desired list semantics of "to the rest of WEED,
-- dropped values don't exist" and must be necessary to make the types work
-- TODO: this might change later if I decide it's a good idea.
mapMDropList :: Monad m => (a -> m b) -> DropList a -> m (DropList b)
mapMDropList _ (DropList []) = return mempty
mapMDropList f' (DropList (x:xs)) = do
  y <- f' $ unDropItem x
  ys <- mapMDropList f' (DropList xs)
  let y' = case x of
        (K _) -> K y
        (D _) -> D y
  return $ y' `consDropList` ys

sequenceDropList :: Monad m => DropList (m a) -> m (DropList a)
sequenceDropList (DropList []) = return mempty
sequenceDropList (DropList (x:xs)) = do
  y <- unDropItem x
  ys <- sequenceDropList $ DropList xs
  let y' = case x of
       K _ -> K y
       D _ -> D y
  return $ y' `consDropList` ys
