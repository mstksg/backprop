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
         shakeArgs opts $ do

    want ["all"]

    "all" ~>
      need ["pdf", "md", "haddocks", "gentags", "install", "exe"]

    "pdf" ~>
      need (map (\f -> "renders" </> takeFileName f -<.> "pdf") allSamps)

    "md" ~>
      need (map (\f -> "renders" </> takeFileName f -<.> "md") allSamps)

    "exe" ~>
      need (map (\f -> "samples-exe" </> dropExtension f) allSamps)

    "haddocks" ~> do
      need (("src" </>) <$> allSrc)
      cmd "jle-git-haddocks"

    "install" ~> do
      need . concat $ [ ("src" </>)     <$> allSrc
                      , ("samples" </>) <$> allSamps
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

    "samples-exe/*" %> \f -> do
      let src = "samples" </> takeFileName f -<.> "lhs"
      need ["install", src]
      liftIO $ do
        createDirectoryIfMissing True "samples-exe"
        createDirectoryIfMissing True ".build"
      removeFilesAfter "samples" ["/*.o"]
      cmd "stack ghc --" src
                         "-o" f
                         "-hidir" ".build"
                         "-threaded"
                         "-rtsopts"
                         "-with-rtsopts=-N"
                         "-Wall"
                         "-O2"

    ["tags","TAGS"] &%> \_ -> do
      need (("src" </>) <$> allSrc)
      cmd "hasktags" "src/"

    "clean" ~> do
      unit $ cmd "stack clean"
      removeFilesAfter ".shake" ["//*"]
      removeFilesAfter ".build" ["//*"]

