#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake

import           Development.Shake
import           Development.Shake.FilePath

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
      need ["pdf", "md", "build", "haddocks"]

    "pdf" ~>
      need (map (\f -> "samples/rendered" </> takeFileName f -<.> "pdf") allSrc)

    "md" ~>
      need (map (\f -> "samples/rendered" </> takeFileName f -<.> "md") allSrc)

    "build" ~>
      cmd "stack" "install"

    "haddocks" ~>
      cmd "jle-git-haddocks"

    ["samples/rendered/*.pdf", "samples/rendered/*.md"] |%> \f -> do
      let src = "samples" </> takeFileName f -<.> "lhs"
      need [src]
      cmd "pandoc" "-V geometry:margin=1in"
                   "-V fontfamily:palatino,cmtt"
                   "-V links-as-notes"
                   "-sS"
                   "--highlight-style tango"
                   "--reference-links"
                   "--reference-location block"
                   "-o" f
                   src

    "clean" ~> do
      unit $ cmd "stack clean"
      removeFilesAfter ".shake" ["//*"]

