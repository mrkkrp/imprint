-- |
-- Module      :  Data.Imprint
-- Copyright   :  © 2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This package provides a solution for serialization of arbitrary Haskell
-- values, monomorphic functions, and closures without relying on remote
-- tables or Template Haskell, with minimum boilderplate.
--
-- To use the package, be sure to enable the following language extensions:
--
--     * [DataKinds](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#datatype-promotion)
--     * [StaticPointers](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#static-pointers)
--     * [TypeOperators](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-operators)
--
-- The following form of import is recommended:
--
-- > import Data.Imprint (Imprint, Col (..), Dict (..), (<:>))
-- > import qualified Data.Imprint as I
--
-- To serialize a value, we must first create an 'Imprint' of it. If the
-- value in question is an instance of the 'Binary' type class, then the
-- 'binary' function should be used:
--
-- > intImprint :: Imprint 'Z Int
-- > intImprint = I.binary (static Dict) 4
--
-- The @static@ keyword has to do with the concept of static pointers, see
-- the link above. We won't go into the details here, but it suffices to say
-- that we need to have an evidence of existence of 'Binary' instance in
-- serializable form. @static@, being a keyword, not a function, has to be
-- used like this and cannot be put inside 'binary', because it creates a
-- pointer to a concrete thing that is passed to it. This little ceremony of
-- passing @static Dict@ as the first argument of 'binary' every time you
-- create an 'Imprint' of a value that has a 'Binary' instance is the only
-- boilderplate we have to put up with, though.
--
-- To create an 'Imprint' of a function or indeed almost anything that has
-- no 'Binary' instance, we use the 'static' function:
--
-- > funImprint :: Imprint 'Z (Int -> String -> String)
-- > funImprint = I.static (static f)
-- >   where
-- >     f n str = str ++ show n
--
-- The @f@ function we want to serialize may be defined anywhere. Note that
-- the resulting 'Imprint' is opaque and has no sign of how it was created
-- (with 'binary' or with 'static').
--
-- Finally, there is a way to apply an 'Imprint' of a value to an 'Imprint'
-- of a function with @('<:>')@:
--
-- > closureImprint :: Imprint ('Z ':~> Int) (String -> String)
-- > closureImprint = funImprint <:> intImprint
--
-- Note how the applied arguments are collected in the phantom type (the
-- first argument of 'Imprint' type constructor). There is no requirement to
-- apply all arguments, you may transmit a partially applied function all
-- right.
--
-- Now, to serialization. That is quite simple, because 'Imprint' is an
-- instance of 'Binary' and so it is perfectly serializable. On the
-- receiving site, you however must know the full type of 'Imprint',
-- including the collection of applied arguments in order to restore it.
--
-- If a more dynamic approach is desirable, we could adopt the
-- representation of closures used in @distributed-process@ as a special
-- case with the following type of 'Imprint':
--
-- > Imprint ('Z ':~> ByteString) (Process ())
--
-- In that case we would need to serialize all the arguments beforehand and
-- put the deserializing code into the @ByteString -> Process ()@ function.
--
-- Finally, we give the guarantee that if you have a value of the type
-- @'Imprint' as a@, then you can have the @a@ value back, see 'restore':
--
-- > restore :: Imprint bs a -> a

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Imprint
  ( -- * Types
    Imprint
  , Col (..)
  , Dict (..)
    -- * Creation of imprints
  , binary
  , static
  , (<:>)
    -- * Elimination of imprints
  , restore )
where

import Data.Binary
import Data.Constraint (Dict (..), withDict)
import Data.Proxy
import Data.Typeable (Typeable)
import GHC.Fingerprint
import GHC.StaticPtr
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection

-- | @'Imprint' bs a@ is an image of @a@ that is isomorphic to @a@ and
-- serializable.

data Imprint (bs :: Col *) a where
  BinaryImprint :: Typeable a
    => StaticPtr (Dict (Binary a))
    -> a
    -> Imprint 'Z a
  StaticImprint :: Typeable a
    => StaticPtr a
    -> Imprint 'Z a
  AppImprint
    :: Imprint bs (b -> a)
    -> Imprint 'Z b
    -> Imprint (bs ':~> b) a

-- | This helper type is used to build the phantom type holding types of the
-- arguments applied to an 'Imprint' of a function.

data Col a where
  Z :: Col a
  (:~>) :: Col a -> a -> Col a

infixl 4 :~>

instance Typeable a => Binary (Imprint 'Z a) where
  put = \case
    BinaryImprint dict a -> do
      putWord8 0
      put (someTypeRep (Proxy :: Proxy a))
      putStatic dict
      withDict (deRefStaticPtr dict) (put a)
    StaticImprint ptr -> do
      putWord8 1
      put (someTypeRep (Proxy :: Proxy a))
      putStatic ptr
  get = do
    h <- getWord8
    case h of
      0 -> withTypeRep (Proxy :: Proxy a) $ do
        dict <- getStatic
        a <- withDict (deRefStaticPtr dict) get
        return (binary dict a)
      1 -> withTypeRep (Proxy :: Proxy a) $
        static <$> getStatic
      _ -> fail "Data.Imprint: decoding failure, invalid header"

instance (Binary (Imprint bs (b -> a)), Typeable a, Typeable b) => Binary (Imprint (bs ':~> b) a) where
  put (AppImprint ab a) = do
    putWord8 2
    put ab
    put a
  get = do
    h <- getWord8
    case h of
      2 -> AppImprint <$> get <*> get
      _ -> fail "Data.Imprint: decoding failure, invalid header"

----------------------------------------------------------------------------
-- Creation of imprints

-- | Create an 'Imprint' of a value with 'Binary' instance.
--
-- > intImprint :: Imprint 'Z Int
-- > intImprint = I.binary (static Dict) 4

binary :: Typeable a => StaticPtr (Dict (Binary a)) -> a -> Imprint 'Z a
binary = BinaryImprint

-- | Create an 'Imprint' of a value without 'Binary' instance.
--
-- > funImprint :: Imprint 'Z (Int -> String -> String)
-- > funImprint = I.static (static f)
-- >   where
-- >     f n str = str ++ show n

static :: Typeable a => StaticPtr a -> Imprint 'Z a
static = StaticImprint

-- | Apply 'Imprint' of a value to an 'Imprint' of a function building a
-- closure.
--
-- > closureImprint :: Imprint ('Z ':~> Int) (String -> String)
-- > closureImprint = funImprint <:> intImprint

(<:>) :: Imprint bs (b -> a) -> Imprint 'Z b -> Imprint (bs ':~> b) a
(<:>) = AppImprint
infixl 4 <:>

----------------------------------------------------------------------------
-- Elimination of imprints

-- | Restore a value from its 'Imprint'.

restore :: Imprint as a -> a
restore = \case
  BinaryImprint _ a -> a
  StaticImprint ptr -> deRefStaticPtr ptr
  AppImprint f x ->
    restore f (restore x)

----------------------------------------------------------------------------
-- Helpers

putStatic :: StaticPtr a -> Put
putStatic ptr = do
  let (Fingerprint hi lo) = staticKey ptr
  put hi
  put lo

getStatic :: Get (StaticPtr a)
getStatic = do
  key <- Fingerprint <$> get <*> get
  case unsaferLookupStaticPtr key of
    Nothing -> fail "Data.Imprint: lookup of static pointer failed"
    Just ptr -> return ptr

withTypeRep :: forall a b. Typeable a => Proxy a -> Get b -> Get b
withTypeRep Proxy m = do
  trep <- get
  if someTypeRep (Proxy :: Proxy a) == trep
    then m
    else fail "Data.Imprint: type rep mismatch"

unsaferLookupStaticPtr :: StaticKey -> Maybe (StaticPtr a)
unsaferLookupStaticPtr = unsafePerformIO . unsafeLookupStaticPtr
