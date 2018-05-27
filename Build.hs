#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake-0.16.4 --stack-yaml stack.yaml

import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory

opts = shakeOptions { shakeFiles     = ".shake"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Normal
                    , shakeThreads   = 1
                    }

data Doc = Lab

main :: IO ()
main = getDirectoryFilesIO "samples" ["/*.lhs", "/*.hs"] >>= \allSamps ->
       getDirectoryFilesIO "src"     ["//*.hs"]          >>= \allSrc ->
         shakeArgs opts $ do

    want ["all"]

    "all" ~>
      need ["pdf", "md", "gentags", "install", "exe"]

    "pdf" ~>
      need [ "renders" </> takeFileName f -<.> ".pdf"
                | f <- allSamps, takeExtension f == ".lhs"
           ]

    "md" ~>
      need [ "renders" </> takeFileName f -<.> ".md"
                | f <- allSamps, takeExtension f == ".lhs"
           ]

    "exe" ~>
      need (map (\f -> "samples-exe" </> dropExtension f) allSamps)

    "haddocks" ~> do
      need $ ("src" </>) <$> allSrc
      cmd "jle-git-haddocks"

    "install" ~> do
      need $ ("src" </>) <$> allSrc
      cmd "stack install"

    "install-profile" ~> do
      need $ ("src" </>) <$> allSrc
      cmd "stack install --profile"

    "gentags" ~>
      need ["tags", "TAGS"]

    ["renders/*.pdf", "renders/*.md"] |%> \f -> do
      let src = "samples" </> takeFileName f -<.> "lhs"
      need [src]
      liftIO $ createDirectoryIfMissing True "renders"
      cmd "pandoc"
        "-V geometry:margin=1in"
        "-V fontfamily:palatino,cmtt"
        "-V links-as-notes"
        "-s"
        "--highlight-style tango"
        "--reference-links"
        "--reference-location block"
        "-o" f
        src
        
    "samples-exe/*" %> \f -> do
      need ["install"]
      [src] <- getDirectoryFiles "samples" $ (takeFileName f <.>) <$> ["hs","lhs"]
      liftIO $ do
        createDirectoryIfMissing True "samples-exe"
        createDirectoryIfMissing True ".build"
      removeFilesAfter "samples" ["/*.o"]
      cmd "stack ghc"
        "--stack-yaml stack.yaml"
        "--"
        ("samples" </> src)
        "-o" f
        "-hidir .build"
        "-Wall"
        "-O2"
        
    "profile" ~> do
      need $ do
        s <- ["manual","bp-lens","bp-hkd","hybrid"]
        e <- ["prof.html","svg"]
        return $ "bench-prof/bench-" ++ s <.> e

    "bench-prof/bench" %> \f -> do
      let src = "bench" </> takeFileName f <.> ".hs"
      need ["install-profile", src]
      unit $ cmd "stack install"
        "--profile"
        "--stack-yaml stack.yaml"
        [ "lens"
        , "hmatrix"
        , "one-liner-instances"
        , "split"
        , "criterion"
        ]
      unit $ cmd "stack ghc"
        "--profile"
        "--stack-yaml stack.yaml"
        src
        "--"
        "-o" f
        "-hidir .build"
        "-O2"
        "-prof"
        "-fexternal-interpreter"

    "bench-prof/bench-*.prof" %> \f -> do
      need ["bench-prof/bench"]
      let b = drop 6 $ takeBaseName f
      unit $ cmd "./bench-prof/bench"
                 ("gradient/" ++ b)
                 "+RTS"
                 "-p"
      cmd "mv" "bench.prof" f
      
    "**/*.prof.html" %> \f -> do
      let src = f -<.> ""
      need [src]
      cmd "profiteur" src

    "**/*.prof.folded" %> \f -> do
      let src = f -<.> ""
      need [src]
      Stdout out <- cmd "cat" [src]
      cmd (Stdin out)
          (FileStdout f)
          "ghc-prof-flamegraph"

    "bench-prof/*.svg" %> \f -> do
      let src = f -<.> "prof.folded"
      need [src]
      cmd (FileStdout f)
          "flamegraph.pl"
          "--width 2000"
          src

    ["tags","TAGS"] &%> \_ -> do
      need (("src" </>) <$> allSrc)
      cmd "hasktags" "src/"

    "clean" ~> do
      unit $ cmd "stack clean"
      removeFilesAfter ".shake" ["//*"]
      removeFilesAfter ".build" ["//*"]

