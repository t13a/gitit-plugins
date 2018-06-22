module GraphViz (plugin) where

-- This plugin allows you to include a graphviz diagram
-- in a page like this:
--
-- ~~~ {.graphviz name="diagram1"}
-- digraph G {Hello->World}
-- ~~~
--
-- The "dot" executable must be in the path.
-- The generated svg file will be saved in the static img directory.
-- If no name is specified, a unique name will be generated from a hash
-- of the file contents.

import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath ((</>))

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "graphviz" `elem` classes = do
  cfg <- askConfig
  let (name, filename) = case lookup "name" namevals of
                           Just fn -> ([Str fn], fn)
                           Nothing -> ([], uniqueName contents)
      filetype         = "svg"
      outfile          = "dot-" ++ filename ++ "." ++ filetype
  liftIO $ do
    (ec, _out, err) <- readProcessWithExitCode "dot"
                       ["-T" ++ filetype,
                        "-o", staticDir cfg </> "img" </> outfile
                       ] contents
    let attr = ("image", [], [])
    if ec == ExitSuccess
      then return $ Para [Image attr name ("/img" </> outfile, "")]
      else error $ "graphviz returned an error status: " ++ err
transformBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
