{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell, TupleSections, TypeSynonymInstances           #-}
module Text.LaTeX.CrossRef.Types where
import           Control.Lens              (makeLenses)
import           Control.Lens              (makePrisms)
import           Control.Lens              ((^?))
import           Control.Lens              (ix)
import           Control.Lens.TH           (makeClassy)
import           Control.Monad.State.Class (MonadState (..))
import           Control.Monad.Tardis      (getPast)
import           Control.Monad.Tardis      (sendFuture)
import           Control.Monad.Tardis      (Tardis)
import           Data.Hashable             (Hashable)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HM
import           Data.HashSet              (HashSet)
import           Data.Maybe                (fromMaybe)
import           Data.Reflection           (Given)
import           Data.Reflection           (given)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
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
             deriving (Read, Show, Eq, Hashable, Generic)

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
data RefFState = RefFState { _context  :: Context
                           , _counters :: Counters
                           , _resolved :: HashMap Label Integer
                           } deriving (Read, Show, Eq)
makeLenses ''RefFState

newtype RefBState = RefBState { _numbers :: HashMap Label Integer
                              } deriving (Read, Show, Eq)
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
                 | Str Text
                 deriving (Read, Show, Eq, Ord,
                           Generic, Hashable)

type Formats = HashMap RefItem [LabelFormat]
data RefOptions = RefOptions { subsumes     :: HashMap RefItem [RefItem]
                             , numberedEnvs :: HashSet Text
                             , formats      :: Formats
                             }
                deriving (Read, Show, Eq,
                          Hashable, Generic)

formatCounter :: Given RefOptions => Counters -> RefItem -> Integer -> Text
formatCounter cnts ri i = foldMap format fmts
  where
    dic = formats given
    fmts = fromMaybe [ThisCounter Arabic] (HM.lookup ri dic)
    format :: LabelFormat -> Text
    format (ThisCounter p) = formatNumeral p i
    format (OtherCounter p c) = formatNumeral p (fromMaybe 0 (HM.lookup c cnts))
    format (Str p) = p

formatNumeral :: Numeral -> Integer -> Text
formatNumeral Arabic i = T.pack $ show i
formatNumeral SmallRoman i =
  let digit = ["0", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x"]
  in fromMaybe (T.pack $ show i) $ digit ^? ix (fromInteger i)
formatNumeral CapitalRoman i =
  let digit = ["0", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"]
  in fromMaybe (T.pack $ show i) $ digit ^? ix (fromInteger i)
formatNumeral SmallGreek i =
  fromMaybe (T.pack $ show i) $
  map T.singleton (['α'..'ω']) ^? ix (fromInteger i)
formatNumeral CapitalGreek i =
  fromMaybe (T.pack $ show i) $
  map T.singleton (['Α'..'Ρ'] ++ ['Τ'..'Ω']) ^? ix (fromInteger i)

formatNumeral SmallAlpha i =
  fromMaybe (T.pack $ show i) $
  map T.singleton ['a'..'z'] ^? ix (fromInteger i)
formatNumeral LargeAlpha i =
  fromMaybe (T.pack $ show i) $
  map T.singleton ['A'..'Z'] ^? ix (fromInteger i)

instance MonadState RefFState Machine where
  get = getPast
  put = sendFuture
