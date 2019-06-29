{-# LANGUAGE RankNTypes      #-}
{-|
Module      : VeriFuzz.Report
Description : Generate a report from a fuzz run.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Generate a report from a fuzz run.
-}

{-# LANGUAGE TemplateHaskell #-}

module VeriFuzz.Report
    ( SynthTool(..)
    , SynthStatus(..)
    , SynthResult(..)
    , SimResult(..)
    , SimTool(..)
    , FuzzReport(..)
    , printResultReport
    , printSummary
    , synthResults
    , simResults
    , synthStatus
    , equivTime
    , fuzzDir
    , fileLines
    , reducTime
    , synthTime
    , defaultIcarusSim
    , defaultVivadoSynth
    , defaultYosysSynth
    , defaultXSTSynth
    , defaultQuartusSynth
    , defaultIdentitySynth
    , descriptionToSim
    , descriptionToSynth
    )
where

import           Control.DeepSeq               (NFData, rnf)
import           Control.Lens                  hiding (Identity, (<.>))
import           Data.Bifunctor                (bimap)
import           Data.ByteString               (ByteString)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   (Endo)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Lazy                (toStrict)
import           Data.Time
import           Data.Vector                   (fromList)
import           Prelude                       hiding (FilePath)
import           Shelly                        (FilePath, fromText,
                                                toTextIgnore, (<.>), (</>))
import           Statistics.Sample             (meanVariance)
import           Text.Blaze.Html               (Html, (!))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           VeriFuzz.Config
import           VeriFuzz.Internal
import           VeriFuzz.Result
import           VeriFuzz.Sim
import           VeriFuzz.Sim.Internal

-- | Common type alias for synthesis results
type UResult = Result Failed ()

-- | Commont type alias for simulation results
type BResult = Result Failed ByteString

data SynthTool = XSTSynth {-# UNPACK #-} !XST
               | VivadoSynth {-# UNPACK #-} !Vivado
               | YosysSynth {-# UNPACK #-} !Yosys
               | QuartusSynth {-# UNPACK #-} !Quartus
               | IdentitySynth {-# UNPACK #-} !Identity
               deriving (Eq)

instance NFData SynthTool where
    rnf (XSTSynth a)      = rnf a
    rnf (VivadoSynth a)   = rnf a
    rnf (YosysSynth a)    = rnf a
    rnf (QuartusSynth a)  = rnf a
    rnf (IdentitySynth a) = rnf a

instance Show SynthTool where
    show (XSTSynth xst)           = show xst
    show (VivadoSynth vivado)     = show vivado
    show (YosysSynth yosys)       = show yosys
    show (QuartusSynth quartus)   = show quartus
    show (IdentitySynth identity) = show identity

instance Tool SynthTool where
    toText (XSTSynth xst)           = toText xst
    toText (VivadoSynth vivado)     = toText vivado
    toText (YosysSynth yosys)       = toText yosys
    toText (QuartusSynth quartus)   = toText quartus
    toText (IdentitySynth identity) = toText identity

instance Synthesiser SynthTool where
    runSynth (XSTSynth xst)           = runSynth xst
    runSynth (VivadoSynth vivado)     = runSynth vivado
    runSynth (YosysSynth yosys)       = runSynth yosys
    runSynth (QuartusSynth quartus)   = runSynth quartus
    runSynth (IdentitySynth identity) = runSynth identity

    synthOutput (XSTSynth xst)           = synthOutput xst
    synthOutput (VivadoSynth vivado)     = synthOutput vivado
    synthOutput (YosysSynth yosys)       = synthOutput yosys
    synthOutput (QuartusSynth quartus)   = synthOutput quartus
    synthOutput (IdentitySynth identity) = synthOutput identity

    setSynthOutput (YosysSynth yosys)     = YosysSynth . setSynthOutput yosys
    setSynthOutput (XSTSynth xst)         = XSTSynth . setSynthOutput xst
    setSynthOutput (VivadoSynth vivado)   = VivadoSynth . setSynthOutput vivado
    setSynthOutput (QuartusSynth quartus) = QuartusSynth . setSynthOutput quartus
    setSynthOutput (IdentitySynth identity) = IdentitySynth . setSynthOutput identity

defaultYosysSynth :: SynthTool
defaultYosysSynth = YosysSynth defaultYosys

defaultQuartusSynth :: SynthTool
defaultQuartusSynth = QuartusSynth defaultQuartus

defaultVivadoSynth :: SynthTool
defaultVivadoSynth = VivadoSynth defaultVivado

defaultXSTSynth :: SynthTool
defaultXSTSynth = XSTSynth defaultXST

defaultIdentitySynth :: SynthTool
defaultIdentitySynth = IdentitySynth defaultIdentity

newtype SimTool = IcarusSim Icarus
                deriving (Eq)

instance NFData SimTool where
    rnf (IcarusSim a) = rnf a

instance Tool SimTool where
    toText (IcarusSim icarus) = toText icarus

instance Simulator SimTool where
    runSim (IcarusSim icarus) = runSim icarus
    runSimWithFile (IcarusSim icarus) = runSimWithFile icarus

instance Show SimTool where
    show (IcarusSim icarus) = show icarus

defaultIcarusSim :: SimTool
defaultIcarusSim = IcarusSim defaultIcarus

-- | The results from running a tool through a simulator. It can either fail or
-- return a result, which is most likely a 'ByteString'.
data SimResult = SimResult !SynthTool !SimTool !BResult !NominalDiffTime
                 deriving (Eq)

instance Show SimResult where
    show (SimResult synth sim r d) = show synth <> ", " <> show sim <> ": " <> show (bimap show (T.unpack . showBS) r) <> " (" <> show d <> ")"

getSimResult :: SimResult -> UResult
getSimResult (SimResult _ _ (Pass _) _) = Pass ()
getSimResult (SimResult _ _ (Fail b) _) = Fail b

-- | The results of comparing the synthesised outputs of two files using a
-- formal equivalence checker. This will either return a failure or an output
-- which is most likely '()'.
data SynthResult = SynthResult !SynthTool !SynthTool !UResult !NominalDiffTime
                   deriving (Eq)

instance Show SynthResult where
    show (SynthResult synth synth2 r d) = show synth <> ", " <> show synth2 <> ": " <> show r <> " (" <> show d <> ")"

getSynthResult :: SynthResult -> UResult
getSynthResult (SynthResult _ _ a _) = a

-- | The status of the synthesis using a simulator. This will be checked before
-- attempting to run the equivalence checks on the simulator, as that would be
-- unnecessary otherwise.
data SynthStatus = SynthStatus !SynthTool !UResult !NominalDiffTime
                 deriving (Eq)

getSynthStatus :: SynthStatus -> UResult
getSynthStatus (SynthStatus _ a _) = a

instance Show SynthStatus where
    show (SynthStatus synth r d) = "synthesis " <> show synth <> ": " <> show r <> " (" <> show d <> ")"

-- | The complete state that will be used during fuzzing, which contains the
-- results from all the operations.
data FuzzReport = FuzzReport { _fuzzDir      :: !FilePath
                             , _synthResults :: ![SynthResult]
                             , _simResults   :: ![SimResult]
                             , _synthStatus  :: ![SynthStatus]
                             , _fileLines    :: {-# UNPACK #-} !Int
                             , _synthTime    :: {-# UNPACK #-} !NominalDiffTime
                             , _equivTime    :: {-# UNPACK #-} !NominalDiffTime
                             , _reducTime    :: {-# UNPACK #-} !NominalDiffTime
                             }
                  deriving (Eq, Show)

$(makeLenses ''FuzzReport)

descriptionToSim :: SimDescription -> SimTool
descriptionToSim (SimDescription "icarus") = defaultIcarusSim
descriptionToSim s =
    error $ "Could not find implementation for simulator '" <> show s <> "'"

-- | Convert a description to a synthesiser.
descriptionToSynth :: SynthDescription -> SynthTool
descriptionToSynth (SynthDescription "yosys" bin desc out) =
    YosysSynth
        . Yosys (fromText <$> bin) (fromMaybe (yosysDesc defaultYosys) desc)
        $ maybe (yosysOutput defaultYosys) fromText out
descriptionToSynth (SynthDescription "vivado" bin desc out) =
    VivadoSynth
        . Vivado (fromText <$> bin) (fromMaybe (vivadoDesc defaultVivado) desc)
        $ maybe (vivadoOutput defaultVivado) fromText out
descriptionToSynth (SynthDescription "xst" bin desc out) =
    XSTSynth
        . XST (fromText <$> bin) (fromMaybe (xstDesc defaultXST) desc)
        $ maybe (xstOutput defaultXST) fromText out
descriptionToSynth (SynthDescription "quartus" bin desc out) =
    QuartusSynth
        . Quartus (fromText <$> bin)
                  (fromMaybe (quartusDesc defaultQuartus) desc)
        $ maybe (quartusOutput defaultQuartus) fromText out
descriptionToSynth (SynthDescription "identity" _ desc out) =
    IdentitySynth
        . Identity (fromMaybe (identityDesc defaultIdentity) desc)
        $ maybe (identityOutput defaultIdentity) fromText out
descriptionToSynth s =
    error $ "Could not find implementation for synthesiser '" <> show s <> "'"

status :: Result Failed () -> Html
status (Pass _           ) = H.td ! A.class_ "is-success" $ "Passed"
status (Fail EmptyFail   ) = H.td ! A.class_ "is-danger" $ "Failed"
status (Fail EquivFail   ) = H.td ! A.class_ "is-danger" $ "Equivalence failed"
status (Fail SimFail     ) = H.td ! A.class_ "is-danger" $ "Simulation failed"
status (Fail SynthFail   ) = H.td ! A.class_ "is-danger" $ "Synthesis failed"
status (Fail EquivError  ) = H.td ! A.class_ "is-danger" $ "Equivalence error"
status (Fail TimeoutError) = H.td ! A.class_ "is-warning" $ "Time out"

synthStatusHtml :: SynthStatus -> Html
synthStatusHtml (SynthStatus synth res diff) = H.tr $ do
    H.td . H.toHtml $ toText synth
    status res
    H.td . H.toHtml $ showT diff

synthResultHtml :: SynthResult -> Html
synthResultHtml (SynthResult synth1 synth2 res diff) = H.tr $ do
    H.td . H.toHtml $ toText synth1
    H.td . H.toHtml $ toText synth2
    status res
    H.td . H.toHtml $ showT diff

resultHead :: Text -> Html
resultHead name = H.head $ do
    H.title $ "Fuzz Report - " <> H.toHtml name
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.meta ! A.charset "utf8"
    H.link
        ! A.rel "stylesheet"
        ! A.href
              "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css"

resultReport :: Text -> FuzzReport -> Html
resultReport name (FuzzReport _ synth _ stat _ _ _ _) = H.docTypeHtml $ do
    resultHead name
    H.body
        . (H.section ! A.class_ "section")
        . (H.div ! A.class_ "container")
        $ do
              H.h1 ! A.class_ "title is-1" $ "Fuzz Report - " <> H.toHtml name
              H.h2 ! A.class_ "title is-2" $ "Synthesis"
              H.table ! A.class_ "table" $ do
                  H.thead
                      . H.toHtml
                      $ ( H.tr
                        . H.toHtml
                        $ [H.th "Tool", H.th "Status", H.th "Run time"]
                        )
                  H.tbody . H.toHtml $ fmap synthStatusHtml stat
              H.h2 ! A.class_ "title is-2" $ "Equivalence Check"
              H.table ! A.class_ "table" $ do
                  H.thead
                      . H.toHtml
                      $ ( H.tr
                        . H.toHtml
                        $ [ H.th "First tool"
                          , H.th "Second tool"
                          , H.th "Status"
                          , H.th "Run time"
                          ]
                        )
                  H.tbody . H.toHtml $ fmap synthResultHtml synth

resultStatus :: Result a b -> Html
resultStatus (Pass _) = H.td ! A.class_ "is-success" $ "Passed"
resultStatus (Fail _) = H.td ! A.class_ "is-danger" $ "Failed"

fuzzStats
    :: (Real a1, Traversable t)
    => ((a1 -> Const (Endo [a1]) a1) -> a2 -> Const (Endo [a1]) a2)
    -> t a2
    -> (Double, Double)
fuzzStats sel fr = meanVariance converted
    where converted = fromList . fmap realToFrac $ fr ^.. traverse . sel

fuzzStatus :: Text -> FuzzReport -> Html
fuzzStatus name (FuzzReport dir s1 s2 s3 sz t1 t2 t3) = H.tr $ do
    H.td
        . ( H.a
          ! A.href
                ( H.textValue
                $ toTextIgnore (dir </> fromText "index" <.> "html")
                )
          )
        $ H.toHtml name
    resultStatus
        $  mconcat (fmap getSynthResult s1)
        <> mconcat (fmap getSimResult s2)
        <> mconcat (fmap getSynthStatus s3)
    H.td . H.string $ show sz
    H.td . H.string $ show t1
    H.td . H.string $ show t2
    H.td . H.string $ show t3

summary :: Text -> [FuzzReport] -> Html
summary name fuzz = H.docTypeHtml $ do
    resultHead name
    H.body
        . (H.section ! A.class_ "section")
        . (H.div ! A.class_ "container")
        $ do
              H.h1 ! A.class_ "title is-1" $ "FuzzReport - " <> H.toHtml name
              H.table ! A.class_ "table" $ do
                  H.thead . H.tr $ H.toHtml
                      [ H.th "Name"
                      , H.th "Status"
                      , H.th "Size (loc)"
                      , H.th "Synthesis time"
                      , H.th "Equivalence check time"
                      , H.th "Reduction time"
                      ]
                  H.tbody
                      . H.toHtml
                      . fmap
                            (\(i, r) ->
                                fuzzStatus ("Fuzz " <> showT (i :: Int)) r
                            )
                      $ zip [1 ..] fuzz
                  H.tfoot . H.toHtml $ do
                      H.tr $ H.toHtml
                          [ H.td $ H.strong "Total"
                          , H.td mempty
                          , H.td
                          .   H.string
                          .   show
                          .   sum
                          $   fuzz
                          ^.. traverse
                          .   fileLines
                          , sumUp synthTime
                          , sumUp equivTime
                          , sumUp reducTime
                          ]
                      H.tr $ H.toHtml
                          [ H.td $ H.strong "Mean"
                          , H.td mempty
                          , fst $ bimap d2I d2I $ fuzzStats fileLines fuzz
                          , fst $ meanVar synthTime
                          , fst $ meanVar equivTime
                          , fst $ meanVar reducTime
                          ]
                      H.tr $ H.toHtml
                          [ H.td $ H.strong "Variance"
                          , H.td mempty
                          , snd $ bimap d2I d2I $ fuzzStats fileLines fuzz
                          , snd $ meanVar synthTime
                          , snd $ meanVar equivTime
                          , snd $ meanVar reducTime
                          ]
  where
    sumUp s = showHtml . sum $ fuzz ^.. traverse . s
    meanVar s = bimap d2T d2T $ fuzzStats s fuzz
    showHtml = H.td . H.string . show
    d2T      = showHtml . (realToFrac :: Double -> NominalDiffTime)
    d2I      = H.td . H.string . show

printResultReport :: Text -> FuzzReport -> Text
printResultReport t f = toStrict . renderHtml $ resultReport t f

printSummary :: Text -> [FuzzReport] -> Text
printSummary t f = toStrict . renderHtml $ summary t f
