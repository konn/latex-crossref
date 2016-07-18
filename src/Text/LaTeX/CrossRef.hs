{-# LANGUAGE FlexibleContexts, OverloadedStrings, ViewPatterns #-}
module Text.LaTeX.CrossRef
    ( RefOptions(..), Numeral(..), LabelFormat(..),
      RefItem(..), Formats, procCrossRef
    ) where
import Text.LaTeX.CrossRef.Orphans ()
import Text.LaTeX.CrossRef.Types

import           Control.Lens           (at, folded, ix, maximumOf, toListOf)
import           Control.Lens           (use, uses, view, (%=), (.=), (.~))
import           Control.Lens           ((^?))
import           Control.Lens.Extras    (is)
import           Control.Monad          (forM_, when)
import           Control.Monad.Tardis   (evalTardis, getsFuture)
import           Control.Monad.Tardis   (modifyBackwards)
import           Data.Foldable          (for_)
import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import           Data.Maybe             (fromMaybe)
import           Data.Reflection        (Given (..), give)
import qualified Data.Text              as T
import           Text.LaTeX.Base.Render (render)
import           Text.LaTeX.Base.Syntax (LaTeX (..), MathType (..), TeXArg (..))
import           Text.Numeral.Roman     (toRoman)

default (Integer)

procCrossRef :: RefOptions -> LaTeX -> LaTeX
procCrossRef opts lat = give opts $ evalTardis (procCrossRef' lat) emptyRefState

procCrossRef' :: Given RefOptions => LaTeX -> Machine LaTeX
procCrossRef' lat = do
  p <- refProc lat
  dic <- use resolved
  modifyBackwards $ numbers .~ dic
  return p

resetDependents :: Given RefOptions => RefItem -> Machine ()
resetDependents ri = do
  let RefOptions revDeps _ _ _ _ = given
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

saveCounter :: Given RefOptions => T.Text -> Machine ()
saveCounter lab = do
  i <- formatCounter
  resolved . at lab .= Just i

fixArgs :: [TeXArg] -> [LaTeX]
fixArgs = toListOf (folded._FixArg)

withCtx :: Given RefOptions => RefItem -> Machine a -> Machine a
withCtx ri act = (context %= (ri:)) *> act <* (context %= tail)

refProc :: Given RefOptions => LaTeX -> Machine LaTeX
refProc i@(TeXComm "ref" (fixArgs -> [arg])) = do
  let tag = render arg
  lat <- getsFuture $ fromMaybe i  . HM.lookup tag . view numbers
  return $ if useHyperlink given
           then TeXComm "hyperref" [FixArg (TeXSeq "#" arg), FixArg lat]
           else lat
refProc l@(TeXComm "label" (fixArgs -> [arg])) = do
  saveCounter (render arg)
  return $ if remainLabel given then l else TeXEmpty
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
refProc (TeXMath Parentheses lat2) = TeXMath Parentheses <$> refProc lat2
refProc (TeXMath Square lat2) = TeXMath Square <$> refProc lat2
refProc (TeXMath Dollar lat2) = TeXMath Dollar <$> refProc lat2
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

formatCounter :: Given RefOptions => Machine LaTeX
formatCounter = do
  ctrs <- use counters
  ri   <- uses context head
  i    <- uses counters (fromMaybe 0 . view (at ri))
  return $ formatCounter' ctrs ri i

formatCounter' :: Given RefOptions => Counters -> RefItem -> Integer -> LaTeX
formatCounter' cnts ri i = foldMap format fmts
  where
    dic = formats given
    fmts = fromMaybe [ThisCounter Arabic] (HM.lookup ri dic)
    format :: LabelFormat -> LaTeX
    format (ThisCounter p) = formatNumeral p i
    format (OtherCounter p c) = formatNumeral p (fromMaybe 0 (HM.lookup c cnts))
    format (Str l) = l

formatNumeral :: Numeral -> Integer -> LaTeX
formatNumeral Arabic i = TeXRaw $ T.pack $ show i
formatNumeral SmallRoman i = TeXRaw $ T.toLower $ toRoman i
formatNumeral CapitalRoman i = TeXRaw $ T.toUpper $ toRoman i
formatNumeral SmallGreek i =
  fromMaybe (TeXRaw $ T.pack $ show i) $
  map (TeXMath Dollar . TeXCommS)
  [ "alpha" , "beta", "gamma", "delta", "varepslion"
  , "zeta", "eta", "theta", "iota", "kappa"
  , "lambda", "mu", "nu", "xi"
  , "pi", "rho", "sigma", "tau"
  , "upsilon", "phi", "chi", "psi", "omega"] ^? ix (fromInteger i)
formatNumeral CapitalGreek i =
  fromMaybe (TeXRaw $ T.pack $ show i) $
  map (TeXMath Dollar . TeXCommS)
  [ "Alpha" , "Beta", "Gamma", "Delta"
  , "Theta"
  , "Lambda", "Xi"
  , "Pi", "Rho", "Sigma"
  , "Upsilon", "Phi", "Psi", "Omega"] ^? ix (fromInteger i)

formatNumeral SmallAlpha i =
  TeXRaw $ fromMaybe (T.pack $ show i) $
  map T.singleton ['a'..'z'] ^? ix (fromInteger i)
formatNumeral LargeAlpha i =
  TeXRaw $ fromMaybe (T.pack $ show i) $
  map T.singleton ['A'..'Z'] ^? ix (fromInteger i)

parseSection :: T.Text -> Maybe Int
parseSection comm
  | ss <- T.splitOn "sub" comm
  , (ks, ["section"]) <- span T.null ss = Just $ length ks
  | otherwise = Nothing
