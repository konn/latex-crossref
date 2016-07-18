{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell, TupleSections, TypeSynonymInstances           #-}
module Text.LaTeX.CrossRef.Types where
import           Control.Lens              (makeLenses)
import           Control.Lens              (makePrisms)
import           Control.Lens.TH           (makeClassy)
import           Control.Monad.State.Class (MonadState (..))
import           Control.Monad.Tardis      (getPast)
import           Control.Monad.Tardis      (sendFuture)
import           Control.Monad.Tardis      (Tardis)
import           Data.Hashable             (Hashable)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM
import           Data.HashSet              (HashSet)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Text.LaTeX.Base.Syntax    (TeXArg (..))
import           Text.LaTeX.Base.Syntax    (LaTeX (..))

type Level = Int
type Label = Text
data RefVal = Number Integer
            | String Text
            deriving (Read, Show, Eq, Ord)

data Counter = Counter { _name       :: Text
                       , _dependents :: HashSet Text
                       }
             deriving (Read, Show, Eq, Generic)

data RefItem = Item Level
             | Environment Text
             | Section Level
             deriving (Read, Show, Eq, Ord,
                       Hashable, Generic)

makePrisms ''RefItem
makePrisms ''TeXArg
makeClassy ''LaTeX

type Counters = HashMap RefItem Integer
type Context = [RefItem]
type LabelDic = HashMap Label LaTeX
data RefFState = RefFState { _context  :: Context
                           , _counters :: Counters
                           , _resolved :: LabelDic
                           } deriving (Show, Eq)
makeLenses ''RefFState

newtype RefBState = RefBState { _numbers :: LabelDic
                              } deriving (Show, Eq)
makeLenses ''RefBState

type Machine = Tardis RefBState RefFState

emptyRefBState :: RefBState
emptyRefBState = RefBState HM.empty

emptyRefFState :: RefFState
emptyRefFState = RefFState [] HM.empty HM.empty

emptyRefState :: (RefBState, RefFState)
emptyRefState = (emptyRefBState, emptyRefFState)

data Numeral = Arabic
             | SmallRoman
             | CapitalRoman
             | SmallGreek
             | CapitalGreek
             | SmallAlpha
             | LargeAlpha
             deriving (Read, Show, Eq, Ord,
                       Generic, Hashable)

data LabelFormat = ThisCounter Numeral
                 | OtherCounter Numeral RefItem
                 | Str LaTeX
                 deriving (Show, Eq, Generic)

type Formats = HashMap RefItem [LabelFormat]
data RefOptions = RefOptions { subsumes     :: HashMap RefItem [RefItem]
                             , numberedEnvs :: HashSet Text
                             , formats      :: Formats
                             , remainLabel  :: Bool
                             , useHyperlink :: Bool
                             }
                deriving (Show, Eq, Generic)

-- instance MonadState RefFState Machine where
--   get = getPast
--   put = sendFuture
