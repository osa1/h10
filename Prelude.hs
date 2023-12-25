{-
This is an example and a test. `cargo run` type checks this file and prints
inferred types. Type signatures below are deliberately omitted to check inferred
types. Eventually this will be a prelude for all h10 programs.
-}

--------------------------------------------------------------------------------
-- Fuctions

id x = x

const x _ =  x

f . g = \x -> f (g x)

flip f x y = f y x

--------------------------------------------------------------------------------
-- Bool

data Bool
  = False
  | True

not False = True
not True = False

otherwise = True

True  && x  = x
False && _  = False

True  || _  = True
False || x  = x

--------------------------------------------------------------------------------
-- Maybe

data Maybe a
  = Nothing
  | Just a

maybe n f Nothing  =  n
maybe n f (Just x) =  f x

--------------------------------------------------------------------------------
-- Either

data Either a b
  = Left a
  | Right b

either f g (Left x)  =  f x
either f g (Right y) =  g y

--------------------------------------------------------------------------------
-- Functor

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Functor (Either a) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right (f b)

instance Functor [] where
  fmap _ [] = []
  fmap f (x : xs) = f x : fmap f xs

instance Functor ((->) r) where
  -- TODO: Allow simple patterns in instances, write this as `fmap = (.)`.
  fmap f1 f2 = f1 . f2

--------------------------------------------------------------------------------
-- Applicative

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  -- TODO: Support "simple pattern bindings" (H2010 4.4.3.2) in instances to
  -- allow `pure = Just`.
  pure a = Just a

  Just f <*> m = fmap f m
  Nothing <*> _ = Nothing

instance Applicative [] where
  pure a = [a]
  fs <*> xs = concat (fmap (\f -> fmap f xs) fs)

instance Applicative ((->) r) where
  -- TODO: Support simple pattern bindings, write this as `pure = const`.
  pure a = const a
  (<*>) f g x = f x (g x)

--------------------------------------------------------------------------------
-- Monad

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b)  -> m b

instance Monad Maybe where
  Just a >>= f = f a
  Nothing >>= _ = Nothing

instance Monad [] where
  xs >>= f = concat (fmap (\x -> f x) xs)

instance Monad ((->) r) where
  f >>= k = \r -> k (f r) r

--------------------------------------------------------------------------------
-- Some utilities to demonstrate inferring predicates

(<$) = fmap . const

($>) = flip (<$)

void x = () <$ x

m >> k = m >>= \_ -> k

join x = x >> id

--------------------------------------------------------------------------------

map :: (a -> b) -> [a] -> [b]
map = fmap

[]       ++ l = l
(x : xs) ++ l = x : (xs ++ l)

concat [] = []
concat (l : ls) = l ++ concat ls
