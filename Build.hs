#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake

import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory

opts = shakeOptions { shakeFiles     = ".shake"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Normal
                    , shakeThreads   = 0
                    }

data Doc = Lab

main :: IO ()
main = getDirectoryFilesIO "samples" ["/*.lhs"] >>= \allSamps ->
       getDirectoryFilesIO "src" ["//*.hs"]     >>= \allSrc ->
       getDirectoryFilesIO "app" ["//*.hs"]     >>= \allApp ->
         shakeArgs opts $ do

    want ["all"]

    "all" ~>
      need ["pdf", "md", "haddocks", "gentags", "install"]

    "pdf" ~>
      need (map (\f -> "renders" </> takeFileName f -<.> "pdf") allSamps)

    "md" ~>
      need (map (\f -> "renders" </> takeFileName f -<.> "md") allSamps)

    "haddocks" ~> do
      need (("src" </>) <$> allSrc)
      cmd "jle-git-haddocks"

    "install" ~> do
      need . concat $ [ ("src" </>)     <$> allSrc
                      , ("samples" </>) <$> allSamps
                      , ("app" </>)     <$> allApp
                      ]
      cmd "stack install"

    "gentags" ~>
      need ["tags", "TAGS"]

    ["renders/*.pdf", "renders/*.md"] |%> \f -> do
      let src = "samples" </> takeFileName f -<.> "lhs"
      need [src]
      liftIO $ createDirectoryIfMissing True "renders"
      cmd "pandoc" "-V geometry:margin=1in"
                   "-V fontfamily:palatino,cmtt"
                   "-V links-as-notes"
                   "-sS"
                   "--highlight-style tango"
                   "--reference-links"
                   "--reference-location block"
                   "-o" f
                   src

    ["tags","TAGS"] &%> \_ -> do
      need (("src" </>) <$> allSrc)
      cmd "hasktags" "src/"

    "clean" ~> do
      unit $ cmd "stack clean"
      removeFilesAfter ".shake" ["//*"]

