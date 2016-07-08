{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE TupleSections, TypeSynonymInstances                       #-}
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

type Context = [RefItem]
data RefFState = RefFState { _context  :: Context
                           , _counters :: HashMap RefItem Integer
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

data RefOptions = RefOptions { subsumes     :: HashMap RefItem [RefItem]
                             , numberedEnvs :: HashSet Text
                             }
                deriving (Read, Show, Eq,
                          Hashable, Generic)


instance MonadState RefFState Machine where
  get = getPast
  put = sendFuture
