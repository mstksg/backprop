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
main = getDirectoryFilesIO "samples" ["/*.lhs"] >>=
          \allSrc -> shakeArgs opts $ do

    want ["all"]

    "all" ~>
      need ["pdf", "md", "build", "haddocks", "gentags"]

    "pdf" ~>
      need (map (\f -> "renders" </> takeFileName f -<.> "pdf") allSrc)

    "md" ~>
      need (map (\f -> "renders" </> takeFileName f -<.> "md") allSrc)

    "build" ~>
      cmd "stack" "install"

    "haddocks" ~>
      cmd "jle-git-haddocks"

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

    ["tags","TAGS"] &%> \_ ->
      cmd "hasktags" "src/"

    "clean" ~> do
      unit $ cmd "stack clean"
      removeFilesAfter ".shake" ["//*"]

