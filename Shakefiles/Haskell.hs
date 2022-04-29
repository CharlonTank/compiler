module Shakefiles.Haskell (cabalProject, executable) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified System.Directory
import Shakefiles.Platform (platform)
import qualified Shakefiles.Platform
import Data.Char (isSpace)
import Data.List (dropWhileEnd, stripPrefix)
import Shakefiles.Extra

-- --ghc-option required because of https://gitlab.haskell.org/ghc/ghc/-/issues/20592
ghcOptionFFI = "--ghc-option=-I/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/ffi"


cabalProject :: String -> [String] -> [String] -> [String] -> [String] -> [String] -> Rules ()
cabalProject name sourceFiles sourcePatterns deps testPatterns testDeps =
    let
        globalConfig =
            [ "cabal.project"
            , "cabal.project.freeze"
            -- , "cabal.project.local"
            ]

        needProjectFiles = do
            sourceFilesFromPatterns <- getDirectoryFiles "" sourcePatterns
            let allFiles = mconcat
                    [ globalConfig
                    , fmap (\d -> "_build/cabal" </> d </> "build.ok") deps
                    , sourceFiles
                    , sourceFilesFromPatterns
                    ]
            need allFiles
            liftIO $ getHashedShakeVersion allFiles
    in
    do
        "_build/cabal/" </> name </> "build.ok" %> \out -> do
            hash <- needProjectFiles
            cmd_ "cabal" "v2-build" ghcOptionFFI "-O0"  (cabalName name ++ ":libs") "--enable-tests"
            writeFile' out hash

        cabalBinPath name "noopt" %> \out -> do
            _ <- needProjectFiles
            cmd_ "cabal" "v2-build" ghcOptionFFI "-O0" (cabalName name ++ ":exes") "--enable-tests"

        cabalBinPath name "opt" %> \out -> do
            _ <- needProjectFiles
            cmd_ "cabal" "v2-build" ghcOptionFFI "-O2" (cabalName name ++ ":exes")

        "_build/cabal/" </> name </> "test.ok" %> \out -> do
            need globalConfig
            need $ fmap (\d -> "_build/cabal" </> d </> "build.ok") deps
            need $ fmap (\d -> "_build/cabal" </> d </> "build.ok") testDeps
            need sourceFiles
            sourceFilesFromPatterns <- getDirectoryFiles "" sourcePatterns
            need sourceFilesFromPatterns
            testFiles <- getDirectoryFiles "" testPatterns
            need testFiles
            cmd_ "cabal" "v2-test" "-O0" (cabalName name ++ ":tests") "--test-show-details=streaming" ghcOptionFFI
            writeFile' out ""


cabalBinPath :: String -> String -> FilePath
cabalBinPath projectName opt =
    let
        version =
            case projectName of
                "elm-format" -> "0.8.5"
                "lamdera" -> "0.19.1"
                "elm" -> "0.19.1"
                _ -> "0.0.0"
    in
    -- dist-newstyle/build/aarch64-osx/ghc-9.0.2/elm-0.19.1/x/lamdera/opt/build/lamdera/lamdera
    "dist-newstyle/build" </> Shakefiles.Platform.cabalInstallOs </> "ghc-9.0.2" </> cabalName projectName ++ "-" ++ version </> "x" </> projectName </> opt </> "build" </> projectName </> projectName <.> exe


cabalName :: String -> String
cabalName projectName =
    case projectName of
        "lamdera" -> "elm"
        "elm" -> "elm"
        _ -> projectName

executable :: FilePath -> String -> String -> Rules ()
executable target projectName gitDescribe =
    do
        target %> \out -> do
            copyFileChanged (cabalBinPath projectName "noopt") out

        phony ("dist-" ++ projectName) $ need
            [ "dist" </> projectName ++ "-" ++ gitDescribe ++ "-" ++ show platform <.> Shakefiles.Platform.zipFormatFor platform
            ]

        ("_build" </> "dist" </> show platform </> projectName <.> exe) %> \out -> do
            let binDist = cabalBinPath projectName "opt"
            need [ binDist ]
            cmd_ "strip" "-o" out binDist

        ("dist" </> projectName ++ "-" ++ gitDescribe ++ "-" ++ show platform <.> "tgz") %> \out -> do
            let binDir = "_build/dist/" ++ show platform
            need [ binDir </> projectName <.> exe ]
            cmd_ "tar" "zcvf" out "-C" binDir (projectName <.> exe)

        ("dist" </> projectName ++ "-" ++ gitDescribe ++ "-" ++ show platform <.> "zip") %> \out -> do
            let binDir = "_build/dist/" ++ show platform
            let bin = binDir </> projectName <.> exe
            need [ bin ]
            absoluteBinPath <- liftIO $ System.Directory.makeAbsolute bin
            liftIO $ removeFiles "." [ out ]
            cmd_ "7z" "a" "-bb3" "-tzip" "-mfb=258" "-mpass=15" out absoluteBinPath

        phonyPrefix (projectName ++ "-publish-") $ \version ->
            need $
                concatMap (\target ->
                    [ "publish" </> version </> projectName ++ "-" ++ version ++ "-" ++ show target <.> Shakefiles.Platform.zipFormatFor target
                    , "publish" </> version </> projectName ++ "-" ++ version ++ "-" ++ show target <.> Shakefiles.Platform.zipFormatFor target <.> "asc"
                    ]
                )
                Shakefiles.Platform.all

        let buildInDocker =
                [ Shakefiles.Platform.Linux
                ]
        let buildOnCi =
                [ Shakefiles.Platform.Windows
                , Shakefiles.Platform.Mac
                ]

        forEach buildInDocker $ \target -> do
            let zipExt = Shakefiles.Platform.zipFormatFor target

            ("_build" </> "docker" </> "*" </> show target </> projectName) %> \out -> do
                need
                    [ "package/linux/build-in-docker.sh"
                    ]
                let sha = takeDirectory1 $ dropDirectory1 $ dropDirectory1 out
                cmd_ "package/linux/build-in-docker.sh" sha

            ("publish" </> "*" </> projectName ++ "-*-" ++ show target <.> zipExt) %> \out -> do
                let tag = takeDirectory1 $ dropDirectory1 out
                StdoutTrim sha <- cmd "git" "rev-list" "-n1" ("tags/" ++ tag)
                let binDir = "_build" </> "docker" </> sha </> show target
                let binFile = projectName ++ Shakefiles.Platform.binExt target
                need [ binDir </> binFile ]
                cmd_ "tar" "zcvf" out "-C" binDir binFile

        forEach buildOnCi $ \target -> do
            let githubRunnerOs = Shakefiles.Platform.githubRunnerOs target
            let zipExt = Shakefiles.Platform.zipFormatFor target

            [ "_build" </> "github-ci" </> "unzipped" </> projectName ++ "-*-" ++ show target <.> zipExt,
              "_build" </> "github-ci" </> "unzipped" </> projectName ++ "-*-" ++ show target <.> zipExt <.> "sig"
              ] &%> \[zip, sig] -> do
                let outDir = takeDirectory zip
                let tag = drop (length projectName + 1) $ (reverse . drop (length (show target) + 1) . reverse) $ dropExtension $ takeFileName zip
                StdoutTrim sha <- cmd "git" "rev-list" "-n1" ("tags/" ++ tag)
                let ciArchive = "downloads" </> projectName ++ "-" ++ sha ++ "-" ++ githubRunnerOs <.> "zip"
                need [ ciArchive ]
                liftIO $ removeFiles "." [ zip, sig ]
                cmd_ "unzip" "-o" "-d" outDir ciArchive
                cmd_ "gpgv" "--keyring" "keys/github-actions.gpg" sig zip

            "publish" </> "*" </> projectName ++ "-*-" ++ show target <.> zipExt %> \out -> do
                let source = "_build" </> "github-ci" </> "unzipped" </> takeFileName out
                copyFileChanged source out
