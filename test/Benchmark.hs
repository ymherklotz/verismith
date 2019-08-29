module Main where

import           Control.Lens   ((&), (.~))
import           Criterion.Main (bench, bgroup, defaultMain, nfAppIO)
import           VeriSmith      (configProperty, defaultConfig, proceduralIO,
                                 propSize, propStmntDepth)

main :: IO ()
main = defaultMain
    [ bgroup "generation"
        [ bench "default" $ nfAppIO (proceduralIO "top") defaultConfig
        , bench "depth" . nfAppIO (proceduralIO "top") $ defaultConfig & configProperty . propStmntDepth .~ 10
        , bench "size" . nfAppIO (proceduralIO "top") $ defaultConfig & configProperty . propSize .~ 40
        ]
    ]
