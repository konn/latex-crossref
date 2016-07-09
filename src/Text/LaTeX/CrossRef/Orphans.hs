{-# LANGUAGE DeriveAnyClass, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.LaTeX.CrossRef.Orphans () where
import GHC.Generics           (Generic)
import Text.LaTeX.Base.Syntax (LaTeX (..))

deriving instance Generic LaTeX
