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

{-
FIXME: Fails with "unbound type variable `a`"
instance Functor (Either a) where
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right (f b)
-}

-- TODO: Functor []
-- TODO: Functor ((->) r)

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

-- TODO: Applicative []
-- TODO: Applicative ((->) r)

--------------------------------------------------------------------------------
-- Monad

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b)  -> m b

instance Monad Maybe where
  Just a >>= f = f a
  Nothing >>= _ = Nothing

-- TODO: Monad []
-- TODO: Monad ((->) r)

--------------------------------------------------------------------------------
-- Some utilities to demonstrate inferring predicates

(<$) = fmap . const

($>) = flip (<$)

void x = () <$ x

m >> k = m >>= \_ -> k

-- FIXME: Panics with "predicate not in HNF: `Monad ((->) a)`
-- join x = x >> id
