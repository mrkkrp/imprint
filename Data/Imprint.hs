-- |
-- Module      :  Data.Imprint
-- Copyright   :  © 2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- TODO Write something here.

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
  ( Imprint
  , Col (..)
  , Dict (..)
  , binary
  , static
  , (<:>)
  , restore
  )
where

import Data.Binary
import Data.Constraint (Dict (..), withDict)
import Data.Proxy
import Data.Typeable (Typeable)
import GHC.Fingerprint
import GHC.StaticPtr
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection

data Imprint (as :: Col *) a where
  BinaryImprint :: Typeable a
    => StaticPtr (Dict (Binary a))
    -> a
    -> Imprint 'Z a
  StaticImprint :: Typeable a
    => StaticPtr a
    -> Imprint 'Z a
  AppImprint
    :: Imprint as (a -> b)
    -> Imprint 'Z a
    -> Imprint (as ':+ a) b

data Col a where
  Z :: Col a
  (:+) :: Col a -> a -> Col a

infixl 4 :+

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

instance (Binary (Imprint bs (b -> a)), Typeable a, Typeable b) => Binary (Imprint (bs ':+ b) a) where
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
-- API

binary :: Typeable a => StaticPtr (Dict (Binary a)) -> a -> Imprint 'Z a
binary = BinaryImprint

static :: Typeable a => StaticPtr a -> Imprint 'Z a
static = StaticImprint

infixl 4 <:>

(<:>) :: Imprint as (a -> b) -> Imprint 'Z a -> Imprint (as ':+ a) b
(<:>) = AppImprint

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
