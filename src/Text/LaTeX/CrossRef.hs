{-# LANGUAGE FlexibleContexts, OverloadedStrings, ViewPatterns #-}
module Text.LaTeX.CrossRef
    ( RefOptions(..), procCrossRef
    ) where
import Text.LaTeX.CrossRef.Orphans ()
import Text.LaTeX.CrossRef.Types

import           Control.Lens           (at, folded, maximumOf)
import           Control.Lens           (toListOf, use, uses, view, (%=))
import           Control.Lens           ((.=), (.~), (^?))
import           Control.Lens.Extras    (is)
import           Control.Monad          (forM_, when)
import           Control.Monad.Tardis   (evalTardis, getsFuture)
import           Control.Monad.Tardis   (modifyBackwards)
import           Control.Monad.Tardis   (runTardis)
import           Data.Foldable          (for_)
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import           Data.Maybe             (fromMaybe)
import           Data.Reflection        (Given (..), give)
import qualified Data.Text              as T
import           Text.LaTeX.Base.Render (render)
import           Text.LaTeX.Base.Syntax (LaTeX (..), TeXArg (..))

default (Integer)

procCrossRef :: RefOptions -> LaTeX -> LaTeX
procCrossRef opts lat = give opts $ evalTardis (procCrossRef' lat) emptyRefState

runCrossRef :: RefOptions -> LaTeX -> (LaTeX, (RefBState, RefFState))
runCrossRef opts lat = give opts $ runTardis (procCrossRef' lat) emptyRefState

procCrossRef' :: Given RefOptions => LaTeX -> Machine LaTeX
procCrossRef' lat = do
  p <- refProc lat
  dic <- use resolved
  modifyBackwards $ numbers .~ dic
  return p

resetDependents :: Given RefOptions => RefItem -> Machine ()
resetDependents ri = do
  let RefOptions revDeps _ = given
  forM_ (fromMaybe [] $ HM.lookup ri revDeps) $ \k ->
    counters . at k .= Nothing

tick :: Given RefOptions => RefItem -> Machine ()
tick ri = do
  counters . at ri %= (Just . maybe 1 succ)
  resetDependents ri
  for_ (ri ^? _Item) $ \l ->
         counters %= HM.filterWithKey (\ k _ -> fromMaybe True ((>= l) <$> k ^? _Item))
  for_ (ri ^? _Section) $ \lvl -> do
    ctx <- use context
    case break (\s -> maybe False (>= lvl) (s ^? _Section)) ctx of
      (_, []) -> context %= (Section lvl :)
      (_, bs) -> context .= Section lvl :
                             reverse
                             (takeWhile (maybe True (<lvl) . (^? _Section))
                             $ reverse bs)

saveCounter :: RefItem -> T.Text -> Machine ()
saveCounter ri lab = do
  i <- uses (counters . at ri) (fromMaybe 0)
  resolved . at lab .= Just i

fixArgs :: [TeXArg] -> [LaTeX]
fixArgs = toListOf (folded._FixArg)

withCtx :: Given RefOptions => RefItem -> Machine a -> Machine a
withCtx ri act = (context %= (ri:)) *> act <* (context %= tail)

refProc :: Given RefOptions => LaTeX -> Machine LaTeX
refProc i@(TeXComm "ref" (fixArgs -> [arg])) =
  getsFuture $ maybe i (TeXRaw . T.pack . show) . HM.lookup (render arg) . view numbers
refProc (TeXComm "label" (fixArgs -> [arg])) = do
  c <- uses context head
  saveCounter c (render arg)
  return TeXEmpty
refProc (TeXComm "item" arg) = do
  c <- uses context head
  when (is _Item c) $ tick c
  TeXComm "item" <$> mapM (mapArgM refProc) arg
refProc (TeXComm sect args)
  | Just n <- parseSection (T.pack sect) = do
  tick (Section n)
  TeXComm sect <$> mapM (mapArgM refProc) args
refProc i@(TeXCommS "item") = do
  c <- uses context head
  when (is _Item c) $ tick c
  return i
refProc (TeXEnv "enumerate" args body) = do
  lvl <- uses context (maybe 1 succ . maximumOf (folded . _Item))
  i <- withCtx (Item lvl) $ do
    TeXEnv "enumerate" <$> mapM (mapArgM refProc) args
                       <*>  (refProc body)
  counters . at (Item lvl) .= Nothing
  return i
refProc (TeXEnv name args body)
  | T.pack name `HS.member` numberedEnvs given =
    withCtx (Environment $ T.pack name) $ do
      tick (Environment $ T.pack name)
      TeXEnv name <$> mapM (mapArgM refProc) args <*> (refProc body)
  | otherwise = TeXEnv name <$> mapM (mapArgM refProc) args <*> refProc body
-- refProc (TeXMath Parentheses lat2) = undefined
-- refProc (TeXMath Square lat2) = undefined
-- refProc (TeXMath Dollar lat2) = undefined
refProc (TeXBraces i) = TeXBraces <$> refProc i
refProc (TeXSeq i1 i2) = TeXSeq <$> refProc i1 <*> refProc i2
refProc (TeXComm name args) = TeXComm name <$> mapM (mapArgM refProc) args
-- refProc (TeXCommS i) = undefined
-- refProc (TeXLineBreak i1 i2) = undefined
refProc i = return i

mapArgM :: Monad m => (LaTeX -> m LaTeX) -> TeXArg -> m TeXArg
mapArgM f (FixArg arg) = FixArg <$> f arg
mapArgM f (OptArg arg) = OptArg <$> f arg
mapArgM f (MOptArg args) = MOptArg <$> mapM f args
mapArgM f (SymArg arg) = SymArg <$> f arg
mapArgM f (MSymArg args) = MSymArg <$> mapM f args
mapArgM f (ParArg arg) = ParArg <$> f arg
mapArgM f (MParArg args) = MParArg <$> mapM f args

parseSection :: T.Text -> Maybe Int
parseSection comm
  | ss <- T.splitOn "sub" comm
  , (ks, ["section"]) <- span T.null ss = Just $ length ks
  | otherwise = Nothing
