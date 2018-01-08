{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}
{-# LANGUAGE TypeOperators       #-}

module Data.ImprintSpec (spec) where

import Data.Binary
import Data.Imprint (Imprint, Col (..), Dict (..), (<:>))
import Test.Hspec
import qualified Data.Imprint as I

spec :: Spec
spec = do
  testS "binary serialization works" intImprint $ \n ->
    n `shouldBe` 4
  testS "static serialization works" funImprint $ \f -> do
    f 4 "Here we have: " `shouldBe` "Here we have: 4"
    f 10 "And 10 == " `shouldBe` "And 10 == 10"
  testS "closure serialization works" closureImprint $ \f ->
    f "Here we have: " `shouldBe` "Here we have: 4"

----------------------------------------------------------------------------
-- Test imprints

intImprint :: Imprint 'Z Int
intImprint = I.binary (static Dict) 4

funImprint :: Imprint 'Z (Int -> String -> String)
funImprint = I.static (static f)
  where
    f n str = str ++ show n

closureImprint :: Imprint ('Z ':~> Int) (String -> String)
closureImprint = funImprint <:> intImprint

----------------------------------------------------------------------------
-- Helpers

testS :: forall bs a. Binary (Imprint bs a)
  => String
  -> Imprint bs a
  -> (a -> IO ())
  -> Spec
testS name imprint m =
  it name $ do
    let x = decode (encode imprint) :: Imprint bs a
    m (I.restore x)
