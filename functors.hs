 {-# LANGUAGE InstanceSigs #-}

import Prelude hiding (Maybe(..))
import Control.Applicative

-- Functors to Monads with Maybes and Lists

data Maybe a = Just a | Nothing deriving (Show, Eq)
data List a = Empty | Cons a (List a) deriving (Show, Eq)

fromList :: [a] -> List a
fromList []     = Empty
fromList (x:xs) = Cons x (fromList xs)

listConcat :: List a -> List a -> List a
Empty `listConcat` ys       = ys
(Cons x xs) `listConcat` ys = Cons x (xs `listConcat` ys)


-- Functors
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Empty       = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs) 

-- Applicatives
instance Applicative Maybe where
  pure :: a -> Maybe a
  pure x = Just x

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing  <*> _ = Nothing
  (Just f) <*> x = fmap f x

instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Empty

  (<*>) :: List (a -> b) -> List a -> List b
  Empty <*> _       = Empty
  (Cons f fs) <*> x = (fmap f x) `listConcat` (fs <*> x)

-- Monads
instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b 
  Nothing  >>= _ = Nothing
  (Just x) >>= f = f x

data Person = Person 
  { name   :: String
  , mother :: Maybe Person
  , father :: Maybe Person
  } deriving (Show)

ben, harris, sandy, lola, neesa, sam :: Person
ben    = Person "Ben" (Just sandy) (Just harris)
harris = Person "Harris" (Just lola) (Just sam)
sandy  = Person "Sandy" (Just neesa) Nothing
lola   = Person "Lola" Nothing Nothing
sam    = Person "Sam" Nothing Nothing
neesa  = Person "Neesa" Nothing Nothing

bothGrandMothers, bothGrandMothersM, bothGrandMothersDo :: Person -> Maybe (String, String)
bothGrandMothers person =
  case father person of
    Nothing  -> Nothing
    Just dad -> 
      case mother person of
        Nothing  -> Nothing
        Just mom -> 
          case mother dad of
            Nothing       -> Nothing
            Just grandma1 -> 
              case mother mom of
                Nothing       -> Nothing
                Just grandma2 -> Just (name grandma1, name grandma2)


bothGrandMothersM person =
  father person >>= (\dad ->
    mother person >>= (\mom ->
      mother dad >>= (\grandma1 ->
        mother mom >>= (\grandma2 ->
          return (name grandma1, name grandma2)
  ))))

bothGrandMothersDo person = do
  dad      <- father person
  mom      <- mother person
  grandma1 <- mother dad
  grandma2 <- mother mom
  return (name grandma1, name grandma2)






