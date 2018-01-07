{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeOperators  #-}

module Main (main) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Imprint (Imprint, Col (..), Dict (..), (<:>))
import qualified Data.Imprint as I

main :: IO ()
main = return ()

foo1 :: Imprint 'Z Int
foo1 = I.binary (static Dict) 4

foo2 :: Imprint 'Z String
foo2 = I.binary (static Dict) "So here we go!"

foo3 :: Imprint 'Z (Int -> String -> String)
foo3 = I.static (static f)
  where
    f n str = str ++ show n

foo4 :: Imprint ('Z ':+ Int ':+ String) String
foo4 = foo3 <:> foo1 <:> foo2

-- λ> runPut (put foo4)
-- λ> let input = it
-- λ> input
-- λ> :set -XTypeOperators
-- λ> :set -XDataKinds
-- λ> let foo4' = runGet get input :: Imprint ('Z ':+ Int ':+ String) String
-- I.restore foo4'
