{-# language RankNTypes, TypeInType, TypeOperators, GADTs, MagicHash,
     NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Data.Typeable.Finger where

import GHC.Types
import GHC.Base
import Data.Either
import qualified GHC.Arr as A
import GHC.Types ( TYPE )
import Data.Type.Equality
import GHC.List ( splitAt, foldl' )
import GHC.Word
import GHC.Show
import GHC.TypeLits ( KnownSymbol, symbolVal' )
import GHC.TypeNats ( KnownNat, natVal' )
import Unsafe.Coerce ( unsafeCoerce )
import Data.Coerce

import GHC.Fingerprint.Type
import {-# SOURCE #-} GHC.Fingerprint

newtype SFingerprint a = SFingerprint {getFingerprint :: Fingerprint}

eqFingerprint :: forall k1 k2 (a :: k1) (b :: k2).
                 SFingerprint a -> SFingerprint b -> Maybe (a :~~: b)
eqFingerprint a b
  | getFingerprint a == getFingerprint b = Just $! unsafeCoerce HRefl
  | otherwise                            = Nothing

eqFingerprintE :: forall k1 k2 (a :: k1) (b :: k2) c.
                 SFingerprint a -> SFingerprint b -> Either (a :~~: b -> c) (a :~~: b)
eqFingerprintE a b
  | getFingerprint a == getFingerprint b = Right $! unsafeCoerce HRefl
  | otherwise                            = Left unsafeCoerce

{- Note [Fun vs app fingerprints]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Fun and App combine the fingerprints of their components in exactly the same
way. Why won't this affect the (probabilistic) injectivity of fingerprinting?
Note the kinds. In funSFingerprint,

  a :: TYPE r1

while in appSFingerprint,

  a :: k1 -> k2

So it is never possible to call both funSFingerprint and appSFingerprint with
the same first argument.
-}

funSFingerprint :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
          (a :: TYPE r1) (b :: TYPE r2).
          SFingerprint a -> SFingerprint b -> SFingerprint ((a -> b) :: Type)
funSFingerprint a b = SFingerprint $ fingerprintFingerprints [getFingerprint a, getFingerprint b]

appSFingerprint :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
           SFingerprint (a :: k1 -> k2)
        -> SFingerprint (b :: k1)
        -> SFingerprint (a b)
appSFingerprint a b = SFingerprint $ fingerprintFingerprints [getFingerprint a, getFingerprint b]
