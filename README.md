From Maybe to Monads
====================

WHY?
---
I'm of the opinion the best way to understand something is to **build it yourself**. 

`Monads` are difficult to explain partially because they are such an abstract concept. To make the idea more 'concrete', let's create a `Monad` ourselves! We begin by redefining the  `Maybe` type, which it turns out is a `Monad`. To be a `Monad`, something must first implement the `Functor` and `Applicative` typeclasses, so we'll build our abstractions up step-by-step. If none of these terms mean anything to you yet, don't worry! Along the way we'll see how each level of abstraction builds on the last, and we'll use examples to see how this added power makes our code both simpler and more expressive.

OK, let's get started!

SETUP
----
Boy do I hate it when a tutorial assumes you already know everything there is to know about setup. This section is an attempt to get you up and running with a minimal setup for testing out your ideas.

First, we *will* assume you have `ghci` installed. If not, here's [where to get it](https://www.haskell.org/ghc/download_ghc_7_10_3).

Once `ghci` is installed, navigate to a folder and create your test file - say `monad-tutorial.hs`. Let's put some dummy code in it.

```haskell
-- ./monad-tutorial.hs

data TrafficLight = Red | Yellow | Green deriving (Show)

allCarsGo :: TrafficLight -> String
allCarsGo Red    = "No"
allCarsGo Yellow = "Slow"
allCarsGo Green  = "Us Kids Know!"
```

Now go to `ghci`. Type `:l monad-tutorials.hs` to load the file (tab auto-completes!). If everything is good, anything you defined in the file should now be at the top level of your `ghci` prompt. Type `Red` and you should get `Red` back. Type `:t Red` and you should get the type of `Red` - `TrafficLight`. Try this out on the function we defined. Also, if you make any changes to your file, all you need to do is type `:r` to reload the last loaded file.

Now I'm going to update the tutorial file with some boilerplate.
```haskell
-- ./monad-tutorial.hs

{-# LANGUAGE InstanceSigs #-}

import Prelude hiding (Maybe(..))
import Control.Applicative
```

The first line `{-# LANGUAGE InstanceSigs #-}` is simply a language extension that allows us to give our typeclass instances type signatures. This isn't strictly necessary, but I find it useful for edification purposes.

The next two lines are import statements. First, we reload the `Prelude`, which is Haskell's core library, but we exclude the Maybe type constructor, as we want to implement it ourselves. Then, we import Control.Applicative, as it isn't imported by default (Functor and Monad are). 

DATA
----
You can't make a monad out of nothing! You need a data type, and this data type needs to have a kind of `* -> *`. In other words, it needs to be a data type that takes exactly one parameter.

```haskell
-- ./monad-tutorial.hs
-- ...

-- recreate Haskell's Maybe data type. We add 'show' for convenience.
data Maybe a = Just a | Nothing deriving (Show)
```

Example Use:
```haskell 
Just 4
Just "Hello"
Nothing
```

-- Maybe is meant to represent a value that *may not exist*. A good example for this use case is database lookup. Imagine I have a database of people. Each person has a mother or father. I want to query for relatives based on a starting Person. However a value in our database could be missing, so we represent this with the Maybe type.

```haskell
-- ./monad-tutorial.hs
-- ...

data Person = Person 
  { name   :: String
  , mother :: Maybe Person
  , father :: Maybe Person
  } deriving (Show)

ben, harris, sandy, lola, neesa, sam :: Person
ben    = Person "Ben" (Just sandy) (Just harris)
harris = Person "Harris" (Just lola) (Just sam)
sandy  = Person "Sandy" (Just neesa) (Just al)
lola   = Person "Lola" Nothing Nothing
sam    = Person "Sam" Nothing Nothing
neesa  = Person "Neesa" Nothing Nothing
al     = Person "Al" Nothing Nothing

```

If you're unfamiliar with Haskell's record syntax, it is essentially just a shorthand for creating accessor functions.

We can still make a Person in the traditional way, like 
```haskell
Person "Ben" (Just sandy) (Just harris)
```
but now we have a way to access the internal values, so
```haskell
bensMom = mother ben 
  -- => Person "Sandy" (Just (Person "Neesa" Nothing Nothing)) (Just (Person "Al" Nothing Nothing))
```

but also notice that this *doesn't* work
```haskell
bensMaternalGrandma = mother (mother ben)
  -- => Error: Couldn't match expected type ‘Person’
  --              with actual type ‘Maybe Person’
```

That's because `mother` is a function of type `Person -> Maybe Person`. We can't simply chain these together because our first `mother ben` returns something incompatible with the next call to `mother`. We need to be explicit about how to *unpack* our `Maybe Person`.

```haskell
-- ./monad-tutorial.hs
-- ...

maternalGrandma :: Person -> Maybe Person
maternalGrandma person =
  case mother person of
    Nothing  -> Nothing
    Just mom -> mother mom  
```

Okay, that wasn't so bad. Now let's imagine I want to write a function that takes a person, and returns his two grandmothers, but only if they both exist.

```haskell
-- ./monad-tutorial.hs
-- ...

bothGrandMothers :: Person -> Maybe (Person, Person)
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
                Just grandma2 -> Just (grandma1, grandma2)
```

This seems unwieldy. We follow one operation from the next, returning Nothing for any faliure, and pushing the computation forward only if it succeeds. There must be a higher-order way to encapsulate this sort of behavior. There is! **Monads!** But first, we need to build up to it. Let's start with something familiar - map. 

Map
---

`map` is a familiar function for anyone whose been using Haskell for more than a minute. If you need a refresher, `map` is a higher-order function that takes a function and a list, and returns that list with each element passed through the function. So
```haskell
map (\x -> x * 2) [1,2,3]
-- => [2,4,6] 
```

`map` has a type signature of `(a -> b) -> [a] -> [b]`. Part of it's power is in it's higher-ordered-ness. Map doesn't care what the function is, as long as the input of the function matches what the list contains.
```haskell
map (\x -> x * 2) [1,2,3]
-- => [2,4,6] 
map (\x -> show x ++ " is a number") [1,2,3]
-- => ["1 is a number","2 is a number","3 is a number"]
```

We aren't dealing with `[]` though, we're dealing with `Maybe`. What would it mean to map over a maybe?

```haskell
-- ./monad-tutorial.hs
-- ...

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just x) = Just (f x)
```

```haskell
maybeMap (\x -> x * 2) (Just 1)
-- => Just 2 
maybeMap (\x -> show x ++ " is a number") (Just 1)
-- => Just "1 is a number"
maybeMap (\x -> show x ++ " is a number") Nothing
-- => Nothing
```

To *map* over a Maybe we: 

* unpack it
* if its a `Nothing`, just return `Nothing`
* if its a `Just x`, apply our `f` to `x`, and rewrap it in a `Just`

Functors
========

`Functor` is a typeclass, and it expects a type of kind `* -> *`. All this means is it expects a type that itself expects one parameter. Examples are Maybe, List - anything defined like `data Type a = ...`

The *minimal complete definition* (i.e. what you need to define, even if other functions are supported on the typeclass) for a `Functor` is `fmap`. If this sounds familiar, it's because `fmap` is a generalization of `map`. First, let's look at the type signature for `fmap` from the docs.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

This looks awfully similar to map, except for those `f`s. So what does the Functor instance for `[]` look like?

```haskell
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap = map
```

It ***is*** `map`! That's because a `Functor` is just a generalization of the idea of `map`. You can think of it as a way to define a data type that is 'mappable'

So, `Functor` defines the class of things that can be "mapped over", and `fmap` defines the function that performs this mapping. (in essence, the function that *is* the class)

Now let's redefine it for Maybe

```haskell
-- ./monad-tutorial.hs
-- ...

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
```

Note: this is identical to how we defined our `maybeMap` function. 

```haskell
fmap (*2) Nothing 
--  => Nothing
fmap (*2) (Just 4)
-- => Exists 8
(+5) <$> (Exists 5)
-- => Exists 10 
```

`<$>` is just an infix version of `fmap` which is sometimes more reasonable to work with.

But ***why*** `fmap` you ask? Because now we can write more general functions that can work on *any* type that can be mapped over

```haskell
doubleFunctor = fmap (*2)
doubleFunctor [1,2,3]
-- => [2,4,6]
doubleFunctor (Just 4)
-- => Just 8
```

This comes in handy when you want to define some transformation, but the specific data type is irrelevant. As long as the data type implements functor, your transformations can be applied to it. This has a similar usefulness to Java's interfaces.

Applicative
===========

To demonstrate the usefulness of the `Applicative` typeclass, we'll first think about the limitations of `Functor`. `Functor` works great when I have a pure function `a -> b` that I want to map over some `Functor` instance `f a`. But what if I wanted to write map a function `(a -> b -> c)` over an `f a` and and `f b` - something like this:

```haskell
fmap (*) (Just 2) (Just 4)
```

It seems obvious what I want (`Just 8`), but that's simply not how `fmap` works. Furthermore, the logic breaks down for other Functor instances

```haskell
fmap (*) [1,2] [3,4]
```

One possible result could look like `[3, 8]` (zip behavior) but another could be `[3, 6, 4, 8]` (set-multiply behavior).

To accomplish something like this, we're going to need the `Applicative` typeclass.

Once again, let's look at the typeclass definition in the docs

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  <*> :: f (a -> b) -> f a -> f b
```

Okay, let's break that down. An `Applicative` is defined over a single type, just like `Functor`. Furthermore, this typeclass has a type-constraint - it has to first be a `Functor` (what'd I tell ya!).

`pure` appears to 'wrap' a regular type into the given `Applicative` instance
`<*>` looks similar to `fmap` (or `<$>`, it's infix equivalent), except instead of taking a regular function `a -> b`, it takes a 'wrapped' function `f (a -> b)`.

All this type stuff is a little abstract. What would an implementation for Maybe look like?

```haskell
instance Applicative Maybe where
  pure :: a -> Maybe a
  pure x = Just x

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing  <*> _ = Nothing
  (Just f) <*> x = fmap f x
```

So `pure` does the only logical thing it can do - wraps the value in a `Just`.
`<*>` similarly does what's expected. It unwraps the `Maybe (a -> b)` - if its a `Nothing`, it can't map it, so we just return `Nothing`. If it is `Just f`, then we map the function onto the value. Note how we rely on the `Functor` implementation - if `x` is a `Nothing`, the mapping still produces a `Nothing`. This means our `Applicative Maybe` only combines a wrapped function with a wrapped value if neither is a `Nothing` - just like you'd expect!

```haskell
fmap (*2) Nothing    === Nothing
fmap (*2) (Exists 4) === Exists 8
(+5) <$> (Exists 5)  === Exists 10 
```

> I never finished the writeup but I really should! Code is complete though, so take a look there for the final step of upgrading an Applicative to a Monad and some examples of the full power it offers.
