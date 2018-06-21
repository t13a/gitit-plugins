module PlantUml (plugin) where

-- This plugin allows you to include a plantuml diagram
-- in a page like this:
--
-- ~~~ {.plantuml name="diagram1"}
-- Alice -> Bob: Authentication Request
-- Bob --> Alice: Authentication Response
-- 
-- Alice -> Bob: Another authentication Request
-- Alice <-- Bob: another authentication Response
-- ~~~
--
-- The "dot" and "plantuml" executable must be in the path.
-- The generated svg file will be saved in the static img directory.
-- If no name is specified, a unique name will be generated from a hash
-- of the file contents.

import GHC.IO.Handle
import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath ((</>))
import System.IO

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "plantuml" `elem` classes = do
  cfg <- askConfig
  let filetype = "svg" 
  let (name, basename) = case lookup "name" namevals of
                         Just fn -> ([Str fn], fn)
                         Nothing -> ([], uniqueName contents)
  let outfile = "plantuml-" ++ basename ++ "." ++ filetype
  liftIO $ do
    (ec, out, err) <- readProcessWithExitCode "plantuml"
                      ["-t" ++ filetype,
                       "-p"
                      ] ("@startuml\n" ++ contents ++ "\n@enduml" )
    let attr = ("image", [], [])
    if ec == ExitSuccess
      then do
        inh <- openFile (staticDir cfg </> "img" </> outfile) ReadWriteMode
        hPutStr inh out
        hClose inh
        return $ Para [Image attr name ("/img" </> outfile, "")]
      else error $ "graphviz returned an error status: " ++ err
transformBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
